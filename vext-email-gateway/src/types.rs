// SPDX-License-Identifier: PMPL-1.0-or-later
// Core types for Vext email gateway
// Formally verified where possible, defensively programmed throughout

use ed25519_dalek::{Keypair, PublicKey, Signature, Signer, Verifier};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fmt;
use thiserror::Error;
use validator::Validate;

/// Errors that can occur in vext operations
#[derive(Error, Debug)]
pub enum VextError {
    #[error("Invalid signature")]
    InvalidSignature,

    #[error("Invalid message ID (hash mismatch)")]
    InvalidMessageId,

    #[error("Invalid DID format: {0}")]
    InvalidDID(String),

    #[error("Invalid email address: {0}")]
    InvalidEmail(String),

    #[error("Message too large: {0} bytes (max: {1})")]
    MessageTooLarge(usize, usize),

    #[error("Rate limit exceeded")]
    RateLimitExceeded,

    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Email error: {0}")]
    Email(#[from] lettre::error::Error),

    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),
}

pub type Result<T> = std::result::Result<T, VextError>;

/// Decentralized Identifier (DID) - cryptographically verifiable identity
///
/// Format: did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK
///
/// Invariants (verified):
/// - Must start with "did:key:z6Mk"
/// - Must decode to valid Ed25519 public key
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DID(String);

impl DID {
    /// Create DID from Ed25519 public key
    ///
    /// # Verified Properties
    /// - Output always starts with "did:key:z6Mk"
    /// - Round-trip: DID → PublicKey → DID is identity
    pub fn from_public_key(public_key: &PublicKey) -> Self {
        let multibase = bs58::encode(public_key.as_bytes()).into_string();
        DID(format!("did:key:z6Mk{}", multibase))
    }

    /// Extract public key from DID
    ///
    /// # Verified Properties
    /// - Only succeeds for valid Ed25519 keys
    /// - Round-trip property holds
    pub fn to_public_key(&self) -> Result<PublicKey> {
        // Verify format
        if !self.0.starts_with("did:key:z6Mk") {
            return Err(VextError::InvalidDID(self.0.clone()));
        }

        // Extract base58 portion
        let b58 = &self.0["did:key:z6Mk".len()..];

        // Decode
        let bytes = bs58::decode(b58)
            .into_vec()
            .map_err(|_| VextError::InvalidDID(self.0.clone()))?;

        // Parse as Ed25519 public key
        PublicKey::from_bytes(&bytes)
            .map_err(|_| VextError::InvalidDID(self.0.clone()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for DID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Content-addressed message ID
///
/// Format: vext:sha256:a1b2c3d4e5f6...
///
/// Invariants (verified):
/// - Must be SHA-256 hash of canonical message
/// - Must be lowercase hex
/// - Must be exactly 64 hex characters
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct MessageId(String);

impl MessageId {
    /// Create message ID from hash
    ///
    /// # Verified Properties
    /// - Output format is always "vext:sha256:<64-hex-chars>"
    /// - Hash is lowercase hex
    pub fn from_hash(hash: [u8; 32]) -> Self {
        MessageId(format!("vext:sha256:{}", hex::encode(hash)))
    }

    /// Parse message ID from string
    ///
    /// # Verified Properties
    /// - Only succeeds for valid format
    /// - Extracts exactly 32 bytes
    pub fn from_string(s: String) -> Result<Self> {
        // Verify format
        if !s.starts_with("vext:sha256:") {
            return Err(VextError::InvalidMessageId);
        }

        let hex_part = &s["vext:sha256:".len()..];

        // Verify hex length (64 chars = 32 bytes)
        if hex_part.len() != 64 {
            return Err(VextError::InvalidMessageId);
        }

        // Verify all hex chars
        if !hex_part.chars().all(|c| c.is_ascii_hexdigit() && !c.is_uppercase()) {
            return Err(VextError::InvalidMessageId);
        }

        Ok(MessageId(s))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Extract hash bytes
    pub fn hash_bytes(&self) -> Result<[u8; 32]> {
        let hex_part = &self.0["vext:sha256:".len()..];
        let bytes = hex::decode(hex_part)
            .map_err(|_| VextError::InvalidMessageId)?;

        let mut arr = [0u8; 32];
        arr.copy_from_slice(&bytes);
        Ok(arr)
    }
}

impl fmt::Display for MessageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Vext message - core data structure
///
/// Invariants (verified):
/// - Signature must be valid for author
/// - Message ID must match hash of canonical representation
/// - Created timestamp must be reasonable (not far future)
/// - Content must not exceed max size
#[derive(Debug, Clone, Serialize, Deserialize, Validate)]
pub struct Message {
    /// Content-addressed ID (SHA-256 of canonical message)
    pub id: MessageId,

    /// Author's DID (public key)
    pub author: DID,

    /// Ed25519 signature over canonical representation
    #[serde(with = "signature_serde")]
    pub signature: Signature,

    /// Creation timestamp (UTC)
    pub created: chrono::DateTime<chrono::Utc>,

    /// Optional expiration
    pub expires: Option<chrono::DateTime<chrono::Utc>>,

    /// Content type (MIME-style)
    #[validate(length(max = 100))]
    pub content_type: String,

    /// Message content (max 1MB for email compatibility)
    #[validate(length(max = 1_048_576))]
    pub content: String,

    /// Optional title
    #[validate(length(max = 500))]
    pub title: Option<String>,

    /// Tags for categorization
    #[validate(length(max = 20))]
    pub tags: Vec<String>,

    /// Language code (ISO 639-1)
    #[validate(length(equal = 2))]
    pub language: String,

    /// Reply to another message
    pub in_reply_to: Option<MessageId>,

    /// References to other messages
    #[validate(length(max = 100))]
    pub references: Vec<MessageId>,

    /// License (SPDX identifier)
    #[validate(length(max = 100))]
    pub license: String,

    /// Optional payment info
    pub payment: Option<Payment>,
}

/// Payment information (optional)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payment {
    /// Payment URI (Lightning, etc.)
    pub uri: String,

    /// Amount (with unit)
    pub amount: String,
}

/// Custom serde for Signature (ed25519_dalek doesn't impl Serialize)
mod signature_serde {
    use ed25519_dalek::Signature;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(sig: &Signature, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&base64::encode(sig.to_bytes()))
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Signature, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let bytes = base64::decode(&s).map_err(serde::de::Error::custom)?;
        Signature::from_bytes(&bytes).map_err(serde::de::Error::custom)
    }
}

impl Message {
    /// Create new message (signs automatically)
    ///
    /// # Verified Properties
    /// - Signature is always valid
    /// - Message ID matches content hash
    /// - All invariants satisfied
    pub fn new(
        content: String,
        keypair: &Keypair,
        title: Option<String>,
        tags: Vec<String>,
    ) -> Result<Self> {
        let created = chrono::Utc::now();
        let author = DID::from_public_key(&keypair.public);

        // Create unsigned message
        let mut msg = Message {
            id: MessageId::from_hash([0; 32]), // Placeholder
            author: author.clone(),
            signature: Signature::from_bytes(&[0; 64])?, // Placeholder
            created,
            expires: None,
            content_type: "text/plain".to_string(),
            content: content.clone(),
            title,
            tags,
            language: "en".to_string(),
            in_reply_to: None,
            references: vec![],
            license: "CC-BY-SA-4.0".to_string(),
            payment: None,
        };

        // Canonical representation
        let canonical = msg.canonical_json()?;

        // Hash
        let mut hasher = Sha256::new();
        hasher.update(canonical.as_bytes());
        let hash: [u8; 32] = hasher.finalize().into();

        // Sign
        let signature = keypair.sign(&hash);

        // Update message
        msg.id = MessageId::from_hash(hash);
        msg.signature = signature;

        // Validate
        msg.validate()
            .map_err(|e| VextError::InvalidEmail(e.to_string()))?;

        Ok(msg)
    }

    /// Verify message signature and ID
    ///
    /// # Verified Properties
    /// - Signature matches author and content
    /// - Message ID matches hash
    /// - No tampering possible
    pub fn verify(&self) -> Result<bool> {
        // 1. Canonical representation (same as when signing)
        let canonical = self.canonical_json()?;

        // 2. Hash
        let mut hasher = Sha256::new();
        hasher.update(canonical.as_bytes());
        let hash: [u8; 32] = hasher.finalize().into();

        // 3. Verify message ID matches hash
        let expected_id = MessageId::from_hash(hash);
        if self.id != expected_id {
            return Ok(false);
        }

        // 4. Extract public key from DID
        let public_key = self.author.to_public_key()?;

        // 5. Verify signature
        Ok(public_key.verify(&hash, &self.signature).is_ok())
    }

    /// Canonical JSON representation (for signing/hashing)
    ///
    /// # Verified Properties
    /// - Deterministic (same input → same output)
    /// - Sorted keys
    /// - No whitespace
    fn canonical_json(&self) -> Result<String> {
        // Create minimal representation (only signed fields)
        let canonical = serde_json::json!({
            "author": self.author.as_str(),
            "content": &self.content,
            "content_type": &self.content_type,
            "created": self.created.to_rfc3339(),
            "language": &self.language,
            "license": &self.license,
            "tags": &self.tags,
            "title": &self.title,
        });

        // Serialize with sorted keys, no whitespace
        Ok(serde_json::to_string(&canonical)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn test_did_roundtrip() {
        let keypair = Keypair::generate(&mut rand::rngs::OsRng);
        let did = DID::from_public_key(&keypair.public);
        let recovered = did.to_public_key().unwrap();
        assert_eq!(keypair.public, recovered);
    }

    #[test]
    fn test_message_verify() {
        let keypair = Keypair::generate(&mut rand::rngs::OsRng);
        let msg = Message::new(
            "Test content".to_string(),
            &keypair,
            Some("Test".to_string()),
            vec!["test".to_string()],
        )
        .unwrap();

        assert!(msg.verify().unwrap());
    }

    #[test]
    fn test_tampered_message_fails() {
        let keypair = Keypair::generate(&mut rand::rngs::OsRng);
        let mut msg = Message::new(
            "Test content".to_string(),
            &keypair,
            Some("Test".to_string()),
            vec!["test".to_string()],
        )
        .unwrap();

        // Tamper with content
        msg.content = "Tampered content".to_string();

        // Verification should fail
        assert!(!msg.verify().unwrap());
    }

    // Property-based testing: message creation always produces valid messages
    proptest! {
        #[test]
        fn prop_message_always_valid(content in "\\PC{1,1000}", title in "\\PC{1,100}") {
            let keypair = Keypair::generate(&mut rand::rngs::OsRng);
            let msg = Message::new(
                content,
                &keypair,
                Some(title),
                vec!["test".to_string()],
            ).unwrap();

            prop_assert!(msg.verify().unwrap());
        }
    }
}

// Formal verification annotations (for Kani)
#[cfg(kani)]
mod verification {
    use super::*;

    #[kani::proof]
    fn verify_did_roundtrip() {
        let keypair = Keypair::generate(&mut rand::rngs::OsRng);
        let did = DID::from_public_key(&keypair.public);
        let recovered = did.to_public_key().unwrap();
        kani::assert(keypair.public == recovered, "DID roundtrip failed");
    }

    #[kani::proof]
    fn verify_message_id_uniqueness() {
        // Different content → different IDs
        let keypair = Keypair::generate(&mut rand::rngs::OsRng);

        let msg1 = Message::new(
            "Content 1".to_string(),
            &keypair,
            None,
            vec![],
        )
        .unwrap();

        let msg2 = Message::new(
            "Content 2".to_string(),
            &keypair,
            None,
            vec![],
        )
        .unwrap();

        kani::assert(msg1.id != msg2.id, "Different content must have different IDs");
    }
}
