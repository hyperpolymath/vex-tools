<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# VEX Toolkit (vex-tools) — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              OPERATOR / AI              │
                        │        (Analysis, Comm, Intervention)   │
                        └───────────────────┬─────────────────────┘
                                            │ Signal / Event
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           VEX TOOLKIT HUB               │
                        │                                         │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ Vexometer │  │  Lazy             │  │
                        │  │ (Metrics) │  │  Eliminator       │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        │        │                 │              │
                        │  ┌─────▼─────┐  ┌────────▼──────────┐  │
                        │  │ Vext      │  │  Satellite        │  │
                        │  │ (Protocol)│  │  Intervention     │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │           EXTERNAL INTERFACES           │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ Traditional│ │  Verified         │  │
                        │  │ Email     │  │  Endpoints        │  │
                        │  └───────────┘  └───────────────────┘  │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile Automation  .machine_readable/  │
                        │  Monorepo Consol     0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
VEXOMETER & INTERVENTION
  Vexometer (Analyser)              ██████████ 100%    10 metric dimensions stable
  Vexometer Satellites              ████████░░  80%    Intervention hub active
  Lazy Eliminator                   ██████████ 100%    Completeness logic verified
  Satellite Template                ██████████ 100%    Scaffolding stable

VEXT PROTOCOL
  Vext (Core Protocol)              ██████████ 100%    Cryptographic neutrality active
  Vext Email Gateway                ████████░░  80%    Email bridge stable
  Neutrality Proofs                 ██████░░░░  60%    Algorithmic verification active

REPO INFRASTRUCTURE
  Justfile Automation               ██████████ 100%    Standard build/test tasks
  .machine_readable/                ██████████ 100%    STATE tracking active
  Consolidation (2026)              ██████████ 100%    6 repos successfully merged

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            █████████░  ~90%   Unified toolkit operational
```

## Key Dependencies

```
Lazy Patterns ───► Eliminator Engine ──► Vexometer Score ──► Intervention
     │                   │                   │                    │
     ▼                   ▼                   ▼                    ▼
Comm Event ──────► Vext Protocol ──────► Email Gateway ──────► Recipient
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
