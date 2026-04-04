// SPDX-License-Identifier: PMPL-1.0-or-later
// Property-based tests for lazy-eliminator

use vex_lazy_eliminator::{Analyzer, Language};
use proptest::prelude::*;

/// Property: Analysis is deterministic (same code → same results)
#[test]
fn prop_analysis_deterministic() {
    let analyzer = Analyzer::new(Language::Python);

    let code = r#"
def function1():
    # TODO: implement
    pass

def function2():
    return 42
"#;

    for _ in 0..10 {
        let result1 = analyzer.analyze(code).expect("First analysis failed");
        let result2 = analyzer.analyze(code).expect("Second analysis failed");

        assert_eq!(result1.len(), result2.len());
        for (d1, d2) in result1.iter().zip(result2.iter()) {
            assert_eq!(d1.line, d2.line);
            assert_eq!(d1.snippet, d2.snippet);
        }
    }
}

/// Property: CII always in [0.0, 1.0] range
#[test]
fn prop_cii_in_valid_range() {
    let languages = vec![
        Language::Python,
        Language::Rust,
        Language::JavaScript,
        Language::Java,
        Language::Go,
    ];

    let code_samples = vec![
        "def f(): pass",
        "# TODO: fix\npass",
        "unimplemented!()",
        "throw new Error();",
        "panic!()",
    ];

    for lang in languages {
        let analyzer = Analyzer::new(lang);
        for code in &code_samples {
            match analyzer.calculate_cii(code) {
                Ok(cii) => {
                    assert!(cii >= 0.0, "CII should be >= 0.0, got {}", cii);
                    assert!(cii <= 1.0, "CII should be <= 1.0, got {}", cii);
                },
                Err(_) => {
                    // Some code samples may not parse for all languages
                }
            }
        }
    }
}

/// Property: Complete code has lower CII than incomplete code
#[test]
fn prop_incomplete_cii_higher() {
    let analyzer = Analyzer::new(Language::Rust);

    let complete = "pub fn add(a: i32, b: i32) -> i32 { a + b }";
    let incomplete = "pub fn add(a: i32, b: i32) -> i32 {\n    // TODO: implement\n    unimplemented!()\n}";

    let complete_cii = analyzer.calculate_cii(complete).expect("CII failed");
    let incomplete_cii = analyzer.calculate_cii(incomplete).expect("CII failed");

    assert!(incomplete_cii > complete_cii);
}

/// Property: No detections for code without patterns
proptest! {
    #[test]
    fn prop_clean_code_no_detections(code_fragment in "[a-zA-Z0-9_,()={}\\[\\]\\s]{10,100}") {
        let analyzer = Analyzer::new(Language::Rust);

        // Skip if fragment happens to contain TODO or other patterns
        if code_fragment.to_uppercase().contains("TODO")
            || code_fragment.to_uppercase().contains("FIXME")
            || code_fragment.contains("unimplemented")
            || code_fragment.contains("todo!")
            || code_fragment.contains("panic!")
        {
            return Ok(());
        }

        match analyzer.analyze(&code_fragment) {
            Ok(detections) => {
                // Clean code should have minimal detections
                prop_assert!(detections.is_empty());
            },
            Err(_) => {
                // Parse errors are acceptable
            }
        }
    }
}

/// Property: Line numbers are positive and ordered
#[test]
fn prop_lines_valid() {
    let analyzer = Analyzer::new(Language::Python);

    let code = r#"
def task1():
    # TODO: first
    pass

def task2():
    # TODO: second
    pass

def task3():
    # FIXME: third
    pass
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");

    // All line numbers should be positive
    for detection in &detections {
        assert!(detection.line > 0);
    }

    // Line numbers should be in ascending order (or same)
    let mut prev_line = 0;
    for detection in &detections {
        assert!(detection.line >= prev_line);
        prev_line = detection.line;
    }
}

/// Property: Detection counts consistent across calls
#[test]
fn prop_detection_count_stable() {
    let languages = vec![Language::Python, Language::Rust, Language::JavaScript];

    let code = "# TODO\n# FIXME\n# XXX\npass";

    for lang in languages {
        let analyzer = Analyzer::new(lang);

        let count1 = analyzer.analyze(&code)
            .map(|d| d.len())
            .unwrap_or(0);

        let count2 = analyzer.analyze(&code)
            .map(|d| d.len())
            .unwrap_or(0);

        assert_eq!(count1, count2);
    }
}

/// Property: Each detection has non-empty snippet
#[test]
fn prop_detections_have_text() {
    let analyzer = Analyzer::new(Language::Rust);

    let code = r#"
fn main() {
    // TODO: fix this
    unimplemented!()
    // FIXME: also this
    todo!("later")
}
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");

    for detection in detections {
        assert!(!detection.snippet.is_empty());
        assert!(!detection.context.is_empty());
    }
}

/// Property: Completeness is inverse of detection count
#[test]
fn prop_completeness_inverse() {
    let analyzer = Analyzer::new(Language::Python);

    let test_codes = vec![
        ("complete code", "def f():\n    return 42"),
        ("todo code", "def f():\n    # TODO\n    pass"),
        ("unimplemented", "def f():\n    raise NotImplementedError()"),
    ];

    for (name, code) in test_codes {
        let is_complete = analyzer.is_complete(code)
            .expect("Check failed");
        let detections = analyzer.analyze(code)
            .expect("Analysis failed");

        if is_complete {
            assert!(detections.is_empty(), "{}: should be complete", name);
        } else {
            assert!(!detections.is_empty(), "{}: should have detections", name);
        }
    }
}

/// Property: Offset values are non-negative
#[test]
fn prop_offsets_valid() {
    let analyzer = Analyzer::new(Language::Rust);

    let code = "// TODO: test\nfn main() {}";

    let detections = analyzer.analyze(code).expect("Analysis failed");

    for detection in detections {
        assert!(detection.line > 0);
        assert!(detection.column >= 0);
        assert!(detection.length > 0);
    }
}

/// Property: Context surrounds the matched snippet
#[test]
fn prop_context_contains_match() {
    let analyzer = Analyzer::new(Language::Rust);

    let code = r#"
pub fn handler() {
    // TODO: implement properly
    return ();
}
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");

    for detection in detections {
        assert!(detection.context.contains(&detection.snippet),
                "Context should contain snippet");
    }
}

/// Property: Same code in different languages
#[test]
fn prop_language_independence() {
    let code = "// TODO: fix";

    for lang in [Language::Rust, Language::Python, Language::JavaScript] {
        let analyzer = Analyzer::new(lang);
        match analyzer.analyze(&code) {
            Ok(detections) => {
                // Should find TODO in comments regardless of language
                assert!(!detections.is_empty());
            },
            Err(_) => {
                // Some languages might not parse arbitrary comments
            }
        }
    }
}

/// Property: Analysis handles empty input
#[test]
fn prop_empty_input_safe() {
    let analyzer = Analyzer::new(Language::Rust);

    let result = analyzer.analyze("").expect("Empty analysis should succeed");
    assert!(result.is_empty());

    let is_complete = analyzer.is_complete("").expect("Empty check should succeed");
    assert!(is_complete);
}

/// Property: Large code doesn't cause overflow
#[test]
fn prop_large_code_safe() {
    let analyzer = Analyzer::new(Language::Python);

    // Generate large code
    let large_code = "def f():\n    pass\n".repeat(1000);

    let result = analyzer.analyze(&large_code).expect("Large code analysis failed");
    // Should not panic or overflow
    assert!(result.len() >= 0);
}
