<!--
<!-- Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> -->
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `vex-tools` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat(crg): add crg-grade and crg-badge justfile recipes
- feat: add stapeln.toml container definition
- feat: deploy UX Manifesto infrastructure
- feat: wire groove endpoint into vext main + lib
- feat: add groove discovery endpoint for vext
- feat: add CI gates and real vexometer core tests
- feat(vexometer): implement all Ada package bodies (8,336 lines)
- feat(vexometer): customize ABI/FFI with real types and Zig implementation
- feat: consolidate six vex repos into unified vex-tools monorepo

### Fixed

- fix(ci): sync hypatia-scan.yml to canonical (413: env.HOME+Phase-2+SARIF) (#18)
- fix(ci): build Hypatia escript from repo root (estate dogfood drift)
- fix(ci): build Hypatia escript from repo root (estate dogfood drift)
- fix(ci): build Hypatia escript from repo root (estate dogfood drift)
- fix(deps): bump vulnerable crates to patched versions (#15)
- fix(manifest): add disambiguation — ISA component, not the cryptographic Vext protocol
- fix(ci): regenerate vexometer trust manifest after README.adoc sync
- fix(deps): replace trust-dns-resolver with hickory-resolver and bump validator to resolve idna advisory
- fix(deps): update lock files to resolve security advisories
- fix(scorecard): enforce granular permissions and add fuzzing placeholder

### Documentation

- docs: record tech-debt audit findings (2026-05-26) (#28)
- docs: substantive CRG C annotation (EXPLAINME.adoc)
- docs: add EXPLAINME.adoc — prove-it file backing README claims

### CI

- ci: fix nonexistent actions/upload-artifact SHA pin (#17)
- ci: deploy dogfood-gate, add Groove manifest and CRG tests

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->
