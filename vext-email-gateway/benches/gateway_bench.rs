// SPDX-License-Identifier: PMPL-1.0-or-later
// Benchmarks for vext email gateway performance

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use vext_email_gateway::{Message, DID};
use ed25519_dalek::SigningKey;
use rand::RngCore;

fn generate_signing_key() -> SigningKey {
    let mut rng = rand::rngs::OsRng;
    let mut sk = [0u8; 32];
    rng.fill_bytes(&mut sk);
    SigningKey::from_bytes(&sk)
}

fn bench_message_creation(c: &mut Criterion) {
    let signing_key = generate_signing_key();

    c.bench_function("message_creation_small", |b| {
        b.iter(|| {
            Message::new(
                black_box("Small content".to_string()),
                black_box(&signing_key),
                black_box(Some("Title".to_string())),
                black_box(vec!["tag1".to_string(), "tag2".to_string()]),
            )
        })
    });

    c.bench_function("message_creation_medium", |b| {
        let medium_content = black_box("x".repeat(10_000));
        b.iter(|| {
            Message::new(
                medium_content.clone(),
                &signing_key,
                Some("Title".to_string()),
                vec!["tag".to_string()],
            )
        })
    });

    c.bench_function("message_creation_large", |b| {
        let large_content = black_box("x".repeat(100_000));
        b.iter(|| {
            Message::new(
                large_content.clone(),
                &signing_key,
                None,
                vec![],
            )
        })
    });
}

fn bench_message_verification(c: &mut Criterion) {
    let signing_key = generate_signing_key();
    let msg = Message::new(
        "Test content for verification".to_string(),
        &signing_key,
        None,
        vec![],
    ).expect("Message creation failed");

    c.bench_function("message_verification", |b| {
        b.iter(|| msg.verify())
    });
}

fn bench_did_operations(c: &mut Criterion) {
    let signing_key = generate_signing_key();

    c.bench_function("did_from_public_key", |b| {
        b.iter(|| DID::from_public_key(black_box(&signing_key.verifying_key())))
    });

    let did = DID::from_public_key(&signing_key.verifying_key());

    c.bench_function("did_to_public_key", |b| {
        b.iter(|| did.to_public_key())
    });
}

fn bench_serialization(c: &mut Criterion) {
    let signing_key = generate_signing_key();
    let msg = Message::new(
        "Test content".to_string(),
        &signing_key,
        Some("Title".to_string()),
        vec!["tag1".to_string(), "tag2".to_string()],
    ).expect("Message creation failed");

    c.bench_function("message_serialize_json", |b| {
        b.iter(|| serde_json::to_string(black_box(&msg)))
    });

    let json = serde_json::to_string(&msg).expect("Serialization failed");

    c.bench_function("message_deserialize_json", |b| {
        b.iter(|| {
            serde_json::from_str::<Message>(black_box(&json))
        })
    });
}

fn bench_signing_key_generation(c: &mut Criterion) {
    c.bench_function("signing_key_generation", |b| {
        b.iter(|| generate_signing_key())
    });
}

criterion_group!(
    benches,
    bench_message_creation,
    bench_message_verification,
    bench_did_operations,
    bench_serialization,
    bench_signing_key_generation
);

criterion_main!(benches);
