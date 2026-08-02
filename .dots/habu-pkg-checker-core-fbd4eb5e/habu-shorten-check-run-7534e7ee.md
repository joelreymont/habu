---
title: Shorten CHECK run orchestration
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:13:43.378868+02:00"
blocks:
  - habu-shorten-check-lint-0bb577c3
---

Why: after providers and child execution are package-owned, the remaining preverify, native run, diagnostic, and result orchestration is the final legacy CHK-prefixed private concern. Owner: package CHECK. Files: tools/check-core.f, tools/check.f, tools/check-main.f, tools/check-test-lib.f, and test/gate-diagnostics-lib.f. Rename the remaining private words from RUN-STATIC through the end of tools/check-core.f: diagnostic runner, preverify capture/file/order/failure handling, source-list report, native hb and JSON-only execution, non-JSON result handling, nominal/static phase dispatch, current/scoped run, and final result cleanup. Update CHECK public MAIN, RESET, CAPTURE-OFF, OPT, SOURCE, FILE, and RUN implementations plus GATE-DIAGNOSTICS calls; expose no additional word or state. Remove any obsolete direct-run entry left after the session cutover. Acceptance: no executable CHK-prefixed definition, storage, reference, or compatibility alias remains in production or test CHECK files; public CHECK is the sole caller surface; every success or throw restores checker scope and temporary state while preserving configured selection/options; command, direct, stdin, file, list, preverify, all-errors, JSON, nominal, package, and diagnostics behavior stays byte-exact. Forbidden: forwarding globals, public mutable state, copied orchestration, error masking, cleanup by process restart, second session authority, or behavior changes. Pre-change proof: the exact diff package gate rejects the first short orchestration helper until CHECK owns the complete caller chain. Verify through full tools/check-test.f, gate diagnostics, gate dictionary, checker strict/all-errors/nominal/package slices, checked-boundary lint, exact diff ownership/type, and host-lint.
