---
title: "V2 R3: declare CAD nominal kinds"
status: active
priority: 1
issue-type: task
created-at: "2026-07-12T07:09:02.620171+02:00"
blocks:
  - habu-v2-types-design-70831db1
---

Problem: MODEL-CAD-V2-PLAN.md R3 public handles and indexes are raw n and can swap. Fix: add maki/cad-kinds.f package CAD-KIND with the 20 public arity-zero TYPEFAMILY declarations; no universal n casts; add focused positive/negative checker fixtures for qualified identity, cross-kind rejection, typed pointer store/fetch, source replay, rollback, and diagnostic rendering. Register source in maki test loading and FILEMAP.md. Acceptance: every declared tail resolves only through CAD-KIND identity; BAD-ID and BAD-STORE reject with exact qualified expected/actual; valid identity/store definitions certify; no TRUSTED raw converter is public. Files: maki/cad-kinds.f, maki/cad-kinds-test.f, maki/test.f, FILEMAP.md. Verify: focused --load test, type-decl/rollback suites, maki suite, host/filemap lints. Depends: nominal-kind design.
