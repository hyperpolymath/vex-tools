;; SPDX-License-Identifier: PMPL-1.0-or-later
(playbook
  (metadata
    (version "0.2.0")
    (last-updated "2026-02-28"))
  (workflow
    (principle "Keep docs and machine-readable state synchronized with tested behavior")
    (naming-policy "Use ISA in prose, retain vexometer identifiers for compatibility")
    (release-gate "Pass monorepo test-all before status upgrades")))
