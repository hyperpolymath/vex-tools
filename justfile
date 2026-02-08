# SPDX-License-Identifier: PMPL-1.0-or-later
# vex-tools unified justfile
# Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

# Default recipe: list available commands
default:
    @just --list

# Build the vexometer (Ada)
build-vexometer:
    cd vexometer && just build

# Build vext (Rust)
build-vext:
    cd vext && cargo build --release

# Build the lazy eliminator
build-lazy-eliminator:
    cd lazy-eliminator && just build

# Build all components
build-all: build-vexometer build-vext build-lazy-eliminator

# Run vexometer tests
test-vexometer:
    cd vexometer && just test

# Run vext tests
test-vext:
    cd vext && cargo test

# Run lazy-eliminator tests
test-lazy-eliminator:
    cd lazy-eliminator && just test

# Run all tests
test-all: test-vexometer test-vext test-lazy-eliminator

# Clean all build artifacts
clean:
    cd vext && cargo clean
    cd vexometer && just clean || true
    cd lazy-eliminator && just clean || true

# Check formatting across Rust components
fmt-check:
    cd vext && cargo fmt -- --check

# Run clippy on Rust components
lint:
    cd vext && cargo clippy -- -D warnings
