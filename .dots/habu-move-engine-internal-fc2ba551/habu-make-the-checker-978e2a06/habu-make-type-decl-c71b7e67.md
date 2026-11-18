---
title: "Make TYPE-DECL's last public internals private"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T17:00:09.405071+01:00"
closed-at: "2026-11-18T17:25:48.657496+01:00"
---

Problem: six TYPE-DECL words are public: REPLAY-ENUM, REPLAY-STRUCTURE, REPLAY-MUTUAL, SD-WRAP-OUTCOME, SD-ONE-NAME (EXPORTed in public sections of src/core/enum-decl.f and src/core/structure-decl.f) and TDECL-CONVERTERS (src/core/sumtype.f); `' TYPE-DECL:REPLAY-ENUM drop` passes tools/check.f -- --verify-only, rc 0. Their engine callers are src/habu/verify-source.f and src/lib/type/deftype.f, both loaded before the seal; tests call them from programs (structure-decl-suite.f, enum-decl-suite.f, load-reject-diag-test.f). Acceptance: each is E-UNDEFINED outside TYPE-DECL; the engine callers take them by EXPORT; each test case reaches the replay through tools/check.f or is deleted where no real input reaches it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-tc.
