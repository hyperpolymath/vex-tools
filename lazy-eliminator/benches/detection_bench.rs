// SPDX-License-Identifier: PMPL-1.0-or-later
// Benchmarks for lazy-eliminator performance

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use vex_lazy_eliminator::{Analyzer, Language};

fn bench_python_analysis(c: &mut Criterion) {
    let analyzer = Analyzer::new(Language::Python);

    c.bench_function("analyze_small_python", |b| {
        b.iter(|| {
            analyzer.analyze(black_box(
                r#"
def hello():
    # TODO: implement
    pass
"#,
            ))
        })
    });

    c.bench_function("analyze_medium_python", |b| {
        let code = black_box(
            r#"
def process_data(items):
    # TODO: validate input
    result = []
    for item in items:
        # FIXME: handle None
        result.append(item)
    # TODO: sort results
    return result

def main():
    # TODO: read from file
    data = [1, 2, 3]
    process_data(data)
"#,
        );
        b.iter(|| analyzer.analyze(&code))
    });

    c.bench_function("analyze_large_python", |b| {
        let code = black_box({
            let mut s = String::new();
            for i in 0..50 {
                s.push_str(&format!(
                    r#"
def func_{}():
    # TODO: implement
    # FIXME: refactor
    pass
"#,
                    i
                ));
            }
            s
        });
        b.iter(|| analyzer.analyze(&code))
    });
}

fn bench_rust_analysis(c: &mut Criterion) {
    let analyzer = Analyzer::new(Language::Rust);

    c.bench_function("analyze_small_rust", |b| {
        b.iter(|| {
            analyzer.analyze(black_box(
                r#"
fn handler() {
    // TODO: implement
    unimplemented!()
}
"#,
            ))
        })
    });

    c.bench_function("analyze_medium_rust", |b| {
        let code = black_box(
            r#"
pub fn process(items: Vec<i32>) -> Vec<i32> {
    // TODO: add validation
    let mut result = Vec::new();

    for item in items {
        // FIXME: handle edge case
        result.push(item);
    }

    // TODO: sort before returning
    result
}
"#,
        );
        b.iter(|| analyzer.analyze(&code))
    });
}

fn bench_language_detection(c: &mut Criterion) {
    let code = "# TODO: implement\npass";

    let languages = vec![
        Language::Python,
        Language::Rust,
        Language::JavaScript,
        Language::Java,
        Language::Go,
    ];

    for lang in languages {
        let analyzer = Analyzer::new(lang);
        c.bench_function(&format!("analyze_{:?}", lang), |b| {
            b.iter(|| analyzer.analyze(black_box(code)))
        });
    }
}

fn bench_cii_calculation(c: &mut Criterion) {
    let analyzer = Analyzer::new(Language::Rust);

    c.bench_function("calculate_cii_small", |b| {
        b.iter(|| analyzer.calculate_cii(black_box("fn f() { 42 }")))
    });

    c.bench_function("calculate_cii_medium", |b| {
        let code = black_box(
            r#"
fn main() {
    // TODO: implement
    let x = 42;
    unimplemented!()
}
"#,
        );
        b.iter(|| analyzer.calculate_cii(&code))
    });
}

fn bench_completeness_check(c: &mut Criterion) {
    let analyzer = Analyzer::new(Language::Python);

    c.bench_function("is_complete_true", |b| {
        b.iter(|| analyzer.is_complete(black_box("def f():\n    return 42")))
    });

    c.bench_function("is_complete_false", |b| {
        b.iter(|| {
            analyzer.is_complete(black_box(
                "def f():\n    # TODO\n    pass",
            ))
        })
    });
}

fn bench_summary_generation(c: &mut Criterion) {
    let analyzer = Analyzer::new(Language::Rust);

    let code = black_box(
        r#"
fn incomplete_1() {
    // TODO: fix
    todo!()
}

fn incomplete_2() {
    // FIXME: refactor
    unimplemented!()
}

fn complete() {
    println!("done");
}
"#,
    );

    c.bench_function("analyze_with_summary", |b| {
        b.iter(|| analyzer.analyze_with_summary(&code))
    });
}

criterion_group!(
    benches,
    bench_python_analysis,
    bench_rust_analysis,
    bench_language_detection,
    bench_cii_calculation,
    bench_completeness_check,
    bench_summary_generation
);

criterion_main!(benches);
