// SPDX-License-Identifier: PMPL-1.0-or-later
// E2E integration tests for lazy-eliminator code analyzer

use vex_lazy_eliminator::{Analyzer, Language, IncompletenessKind};

/// E2E: Full Python analysis pipeline
#[test]
fn e2e_python_analysis_pipeline() {
    let analyzer = Analyzer::new(Language::Python);

    let code = r#"
def incomplete_function():
    # TODO: finish implementation
    pass

def another_function():
    """This is a stub"""
    ...
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");
    assert!(!detections.is_empty());

    // Should detect both TODO and incompleteness patterns
    let has_todo = detections.iter().any(|d| matches!(d.kind, IncompletenessKind::TodoComment));
    assert!(has_todo);
}

/// E2E: Rust analysis with unimplemented macros
#[test]
fn e2e_rust_analysis_pipeline() {
    let analyzer = Analyzer::new(Language::Rust);

    let code = r#"
fn incomplete_handler() {
    unimplemented!("Not yet implemented")
}

fn todo_function() {
    todo!("Implement this later")
}
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");
    assert!(!detections.is_empty());

    let has_unimplemented = detections.iter()
        .any(|d| matches!(d.kind, IncompletenessKind::UnimplementedCode));
    assert!(has_unimplemented);
}

/// E2E: JavaScript/TypeScript analysis
#[test]
fn e2e_javascript_analysis_pipeline() {
    let analyzer = Analyzer::new(Language::JavaScript);

    let code = r#"
async function processData() {
    // TODO: fetch data
    // ...
    return null;
}

const stub = () => {
    // FIXME: implement properly
    throw new Error("Not implemented");
};
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");
    assert!(!detections.is_empty());

    // Should detect TODO comments
    let has_todo = detections.iter()
        .any(|d| d.snippet.contains("TODO"));
    assert!(has_todo);
}

/// E2E: Java analysis with placeholder patterns
#[test]
fn e2e_java_analysis_pipeline() {
    let analyzer = Analyzer::new(Language::Java);

    let code = r#"
public class DataProcessor {
    public void processData() {
        // TODO: add validation
        int result = 0; // placeholder
        System.out.println(result);
    }
}
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");
    assert!(!detections.is_empty());
}

/// E2E: Go analysis
#[test]
fn e2e_go_analysis_pipeline() {
    let analyzer = Analyzer::new(Language::Go);

    let code = r#"
func Process() error {
    // TODO: implement error handling
    return nil
}

func Handler() {
    // FIXME: this is a workaround
    panic("not implemented")
}
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");
    assert!(!detections.is_empty());
}

/// E2E: Complete code analysis (should find nothing)
#[test]
fn e2e_complete_code_analysis() {
    let analyzer = Analyzer::new(Language::Rust);

    let code = r#"
pub fn process_items(items: Vec<i32>) -> i32 {
    items.iter().sum()
}

pub fn main() {
    let nums = vec![1, 2, 3, 4, 5];
    let total = process_items(nums);
    println!("Total: {}", total);
}
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");
    assert!(detections.is_empty(), "Complete code should have no detections");
}

/// E2E: Analysis with summary statistics
#[test]
fn e2e_analysis_with_summary() {
    let analyzer = Analyzer::new(Language::Python);

    let code = r#"
def task1():
    # TODO: implement
    pass

def task2():
    # FIXME: broken
    pass

def task3():
    return "complete"
"#;

    let (detections, summary) = analyzer.analyze_with_summary(code)
        .expect("Analysis failed");

    assert!(!detections.is_empty());
    assert!(summary.total > 0);
    assert!(summary.cii > 0.0);
}

/// E2E: Mixed language patterns in comments
#[test]
fn e2e_mixed_language_patterns() {
    let analyzer = Analyzer::new(Language::Rust);

    let code = r#"
fn main() {
    // TODO: handle errors
    let result = unsafe {
        // WARNING: unsafe block
        std::mem::zeroed()
    };

    // XXX: needs review
    println!("{:?}", result);
}
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");

    // Should detect TODO patterns
    let todos: Vec<_> = detections.iter()
        .filter(|d| d.snippet.contains("TODO"))
        .collect();
    assert!(!todos.is_empty());
}

/// E2E: Line number accuracy
#[test]
fn e2e_line_number_accuracy() {
    let analyzer = Analyzer::new(Language::Python);

    let code = "def func1():\n    pass\n\ndef func2():\n    # TODO: fix\n    pass";

    let detections = analyzer.analyze(code).expect("Analysis failed");
    assert!(!detections.is_empty());

    // TODO should be on line 5
    let todo_line = detections.iter()
        .find(|d| d.snippet.contains("TODO"))
        .map(|d| d.line);

    assert_eq!(todo_line, Some(5));
}

/// E2E: Multi-line code blocks
#[test]
fn e2e_multiline_code_block() {
    let analyzer = Analyzer::new(Language::Rust);

    let code = r#"
fn complex_operation() {
    let data = vec![
        1, 2, 3, 4, 5
    ];

    // TODO: process array elements
    // TODO: validate results

    data
}
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");

    // Should find both TODO comments
    let todos: Vec<_> = detections.iter()
        .filter(|d| d.snippet.contains("TODO"))
        .collect();
    assert_eq!(todos.len(), 2);
}

/// E2E: Context extraction
#[test]
fn e2e_context_extraction() {
    let analyzer = Analyzer::new(Language::Python);

    let code = r#"
def function1():
    return 1

def function2():
    # TODO: implement logic
    return None

def function3():
    return 3
"#;

    let detections = analyzer.analyze(code).expect("Analysis failed");

    assert!(!detections.is_empty());

    // Check that context includes surrounding lines
    for detection in detections {
        assert!(detection.context.contains("def function"));
    }
}

/// E2E: Completeness check
#[test]
fn e2e_completeness_check() {
    let analyzer = Analyzer::new(Language::Python);

    let incomplete_code = "def f():\n    # TODO\n    pass";
    let is_complete_incomplete = analyzer.is_complete(incomplete_code)
        .expect("Check failed");
    assert!(!is_complete_incomplete);

    let complete_code = "def f():\n    return 42";
    let is_complete_complete = analyzer.is_complete(complete_code)
        .expect("Check failed");
    assert!(is_complete_complete);
}

/// E2E: CII calculation
#[test]
fn e2e_cii_calculation() {
    let analyzer = Analyzer::new(Language::Rust);

    let complete_code = "pub fn add(a: i32, b: i32) -> i32 { a + b }";
    let complete_cii = analyzer.calculate_cii(complete_code)
        .expect("CII calculation failed");

    let incomplete_code = r#"
pub fn process() {
    // TODO: implement
    unimplemented!()
}
"#;
    let incomplete_cii = analyzer.calculate_cii(incomplete_code)
        .expect("CII calculation failed");

    // Incomplete code should have higher CII
    assert!(incomplete_cii > complete_cii);

    // Both should be in valid range
    assert!(complete_cii >= 0.0);
    assert!(incomplete_cii <= 1.0);
}
