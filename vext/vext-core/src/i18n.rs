// SPDX-License-Identifier: PMPL-1.0-or-later
//! Internationalization (i18n) support for vext
//!
//! Provides multi-language support for vext messages and notifications
//! using the Fluent localization system.

#[cfg(feature = "i18n")]
use fluent::{FluentBundle, FluentResource};

#[cfg(feature = "i18n")]
use fluent_bundle::FluentArgs;

#[cfg(feature = "i18n")]
use unic_langid::{LanguageIdentifier, langid};

#[cfg(feature = "i18n")]
use anyhow::Result;

#[cfg(feature = "i18n")]
use std::collections::HashMap;

/// Localization manager for vext messages
#[cfg(feature = "i18n")]
pub struct I18n {
    bundles: HashMap<String, FluentBundle<FluentResource>>,
    default_lang: LanguageIdentifier,
}

#[cfg(feature = "i18n")]
impl I18n {
    /// Create a new I18n instance with default language
    pub fn new(default_lang: &str) -> Result<Self> {
        let lang_id: LanguageIdentifier = default_lang.parse()?;
        Ok(Self {
            bundles: HashMap::new(),
            default_lang: lang_id,
        })
    }

    /// Load translations from a Fluent FTL file
    pub fn load_ftl(&mut, lang: &str, ftl_content: &str) -> Result<()> {
        let lang_id: LanguageIdentifier = lang.parse()?;
        let resource = FluentResource::try_new(ftl_content.to_string())
            .map_err(|e| anyhow::anyhow!("Failed to parse FTL: {:?}", e))?;

        let mut bundle = FluentBundle::new(vec![lang_id.clone()]);
        bundle.add_resource(resource)
            .map_err(|e| anyhow::anyhow!("Failed to add resource: {:?}", e))?;

        self.bundles.insert(lang.to_string(), bundle);
        Ok(())
    }

    /// Get a localized message
    pub fn get(&self, lang: &str, message_id: &str) -> Option<String> {
        self.bundles.get(lang)
            .or_else(|| self.bundles.get(&self.default_lang.to_string()))
            .and_then(|bundle| bundle.get_message(message_id))
            .and_then(|msg| msg.value())
            .map(|pattern| {
                let mut errors = vec![];
                bundle.format_pattern(pattern, None, &mut errors).to_string()
            })
    }

    /// Get a localized message with arguments
    pub fn get_with_args(&self, lang: &str, message_id: &str, args: &FluentArgs) -> Option<String> {
        self.bundles.get(lang)
            .or_else(|| self.bundles.get(&self.default_lang.to_string()))
            .and_then(|bundle| bundle.get_message(message_id))
            .and_then(|msg| msg.value())
            .map(|pattern| {
                let mut errors = vec![];
                bundle.format_pattern(pattern, Some(args), &mut errors).to_string()
            })
    }
}

#[cfg(not(feature = "i18n"))]
pub struct I18n;

#[cfg(not(feature = "i18n"))]
impl I18n {
    pub fn new(_lang: &str) -> anyhow::Result<Self> {
        Err(anyhow::anyhow!("i18n not enabled. Rebuild with --features i18n"))
    }

    pub fn load_ftl(&mut, _lang: &str, _ftl: &str) -> anyhow::Result<()> {
        Ok(())
    }

    pub fn get(&self, _lang: &str, message_id: &str) -> Option<String> {
        Some(message_id.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(feature = "i18n")]
    fn test_i18n_basic() {
        let mut i18n = I18n::new("en-US").unwrap();

        let ftl = r#"
hello = Hello, World!
goodbye = Goodbye!
        "#;

        i18n.load_ftl("en-US", ftl).unwrap();

        assert_eq!(i18n.get("en-US", "hello"), Some("Hello, World!".to_string()));
    }

    #[test]
    #[cfg(not(feature = "i18n"))]
    fn test_i18n_disabled() {
        let result = I18n::new("en-US");
        assert!(result.is_err());
    }
}
