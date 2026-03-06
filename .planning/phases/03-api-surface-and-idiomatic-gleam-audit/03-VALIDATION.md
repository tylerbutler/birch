---
phase: 03
slug: api-surface-and-idiomatic-gleam-audit
status: draft
nyquist_compliant: false
wave_0_complete: false
created: 2026-03-05
---

# Phase 03 — Validation Strategy

> Per-phase validation contract for feedback sampling during execution.

---

## Test Infrastructure

| Property | Value |
|----------|-------|
| **Framework** | gleeunit 1.0+ |
| **Config file** | gleam.toml (test deps section) |
| **Quick run command** | `gleam test` |
| **Full suite command** | `just test` |
| **Estimated runtime** | ~15 seconds |

---

## Sampling Rate

- **After every task commit:** Run `gleam test`
- **After every plan wave:** Run `just test`
- **Before `/gsd:verify-work`:** Full suite must be green
- **Max feedback latency:** 15 seconds

---

## Per-Task Verification Map

| Task ID | Plan | Wave | Requirement | Test Type | Automated Command | File Exists | Status |
|---------|------|------|-------------|-----------|-------------------|-------------|--------|
| 03-01-T1 | 01 | 1 | API-01 | build | `gleam build` | Existing (needs update) | pending |
| 03-01-T2 | 01 | 1 | API-01 | unit | `just test` | Existing (needs update) | pending |
| 03-02-T1 | 02 | 2 | API-03 | unit | `just test` | Existing (needs update) | pending |
| 03-02-T2 | 02 | 2 | API-03 | unit | `just test` | Existing (needs update) | pending |
| 03-03-T1 | 03 | 3 | API-02, API-04, API-05 | build | `gleam build && gleam build --target javascript` | Existing | pending |
| 03-03-T2 | 03 | 3 | API-02, API-04, API-05 | integration | `just test` | Existing | pending |

*Status: pending / green / red / flaky*

---

## Wave 0 Requirements

Existing infrastructure covers all phase requirements. Tests need updating (call sites change from direct field access to accessor functions, renamed functions) but no new test files or framework changes needed.

---

## Manual-Only Verifications

| Behavior | Requirement | Why Manual | Test Instructions |
|----------|-------------|------------|-------------------|
| API docs reflect new naming | API-05 | Doc review | Run `gleam docs build`, inspect generated docs for consistency |

---

## Validation Sign-Off

- [ ] All tasks have `<automated>` verify or Wave 0 dependencies
- [ ] Sampling continuity: no 3 consecutive tasks without automated verify
- [ ] Wave 0 covers all MISSING references
- [ ] No watch-mode flags
- [ ] Feedback latency < 15s
- [ ] `nyquist_compliant: true` set in frontmatter

**Approval:** pending
