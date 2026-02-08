// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

use crate::detection::IncompletenessKind;
use crate::language::Language;
use regex::Regex;
use std::sync::OnceLock;

/// Pattern definition for detecting incompleteness
pub struct Pattern {
    pub kind: IncompletenessKind,
    pub regex: Regex,
    pub description: &'static str,
}

/// Get regex patterns for a specific language
pub fn get_patterns(language: Language) -> Vec<Pattern> {
    let mut patterns = vec![];

    // TODO patterns (common across languages)
    patterns.extend(todo_patterns());

    // Language-specific patterns
    match language {
        Language::Python => patterns.extend(python_patterns()),
        Language::Rust => patterns.extend(rust_patterns()),
        Language::JavaScript | Language::TypeScript => patterns.extend(javascript_patterns()),
        Language::Java => patterns.extend(java_patterns()),
        Language::Go => patterns.extend(go_patterns()),
    }

    patterns
}

fn todo_patterns() -> Vec<Pattern> {
    vec![
        Pattern {
            kind: IncompletenessKind::TodoComment,
            regex: Regex::new(r"(?i)//\s*TODO[:\s]").unwrap(),
            description: "C-style TODO comment",
        },
        Pattern {
            kind: IncompletenessKind::TodoComment,
            regex: Regex::new(r"(?i)#\s*TODO[:\s]").unwrap(),
            description: "Hash-style TODO comment",
        },
        Pattern {
            kind: IncompletenessKind::TodoComment,
            regex: Regex::new(r"(?i)/\*\s*TODO[:\s]").unwrap(),
            description: "Block comment TODO",
        },
        Pattern {
            kind: IncompletenessKind::PlaceholderText,
            regex: Regex::new(r"\.\.\.").unwrap(),
            description: "Ellipsis placeholder",
        },
        Pattern {
            kind: IncompletenessKind::PlaceholderText,
            regex: Regex::new(r"<placeholder>").unwrap(),
            description: "Explicit placeholder tag",
        },
        Pattern {
            kind: IncompletenessKind::TruncationMarker,
            regex: Regex::new(r"//\s*\.\.\.\s*\(truncated\)").unwrap(),
            description: "Truncation marker",
        },
        Pattern {
            kind: IncompletenessKind::TruncationMarker,
            regex: Regex::new(r"//\s*rest similar").unwrap(),
            description: "Rest similar marker",
        },
    ]
}

fn python_patterns() -> Vec<Pattern> {
    vec![
        Pattern {
            kind: IncompletenessKind::UnimplementedCode,
            regex: Regex::new(r"\braise\s+NotImplementedError\b").unwrap(),
            description: "Python NotImplementedError",
        },
        Pattern {
            kind: IncompletenessKind::NullImplementation,
            regex: Regex::new(r"^\s*pass\s*$").unwrap(),
            description: "Python pass statement",
        },
    ]
}

fn rust_patterns() -> Vec<Pattern> {
    vec![
        Pattern {
            kind: IncompletenessKind::UnimplementedCode,
            regex: Regex::new(r"\bunimplemented!\(").unwrap(),
            description: "Rust unimplemented! macro",
        },
        Pattern {
            kind: IncompletenessKind::UnimplementedCode,
            regex: Regex::new(r"\btodo!\(").unwrap(),
            description: "Rust todo! macro",
        },
        Pattern {
            kind: IncompletenessKind::UnimplementedCode,
            regex: Regex::new(r"\bunreachable!\(").unwrap(),
            description: "Rust unreachable! macro",
        },
    ]
}

fn javascript_patterns() -> Vec<Pattern> {
    vec![
        Pattern {
            kind: IncompletenessKind::UnimplementedCode,
            regex: Regex::new(r#"throw\s+new\s+Error\(\s*["'](?:unimplemented|not implemented)["']\s*\)"#).unwrap(),
            description: "JavaScript unimplemented error",
        },
        Pattern {
            kind: IncompletenessKind::NullImplementation,
            regex: Regex::new(r"return\s+null\s*;").unwrap(),
            description: "Return null",
        },
        Pattern {
            kind: IncompletenessKind::NullImplementation,
            regex: Regex::new(r"^\s*\{\s*\}\s*$").unwrap(),
            description: "Empty block",
        },
    ]
}

fn java_patterns() -> Vec<Pattern> {
    vec![
        Pattern {
            kind: IncompletenessKind::UnimplementedCode,
            regex: Regex::new(r#"throw\s+new\s+UnsupportedOperationException\(\s*["'].*not.*implemented.*["']\s*\)"#).unwrap(),
            description: "Java UnsupportedOperationException",
        },
        Pattern {
            kind: IncompletenessKind::NullImplementation,
            regex: Regex::new(r"return\s+null\s*;").unwrap(),
            description: "Return null",
        },
    ]
}

fn go_patterns() -> Vec<Pattern> {
    vec![
        Pattern {
            kind: IncompletenessKind::UnimplementedCode,
            regex: Regex::new(r#"panic\(\s*["']not implemented["']\s*\)"#).unwrap(),
            description: "Go panic(\"not implemented\")",
        },
        Pattern {
            kind: IncompletenessKind::NullImplementation,
            regex: Regex::new(r"return\s+nil").unwrap(),
            description: "Return nil",
        },
    ]
}
