---
title: "fix nfr-test: test the unknown binding contract with the JIT"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.815634+02:00"
closed-at: "2026-10-30T23:10:21.234748+02:00"
close-reason: "No route reaches CAPTURE-BIND binding-contract-unknown on the stack: JIT files never reach it (OBJECT-OPEN is tier-1-only); recovery records are withheld; unchecked records (colon, create, variable, defer, constant) carry a contract; a persisted -1 contract needs a live unknown first; EFF-UNRESOLVED arises only in quiet source composition; fixed kind -1 only at boot. The branch stays as the handler of CONTRACT$ answering false; no edit."
---

Problem: tools/native-unit-dependency.f:1413 CAPTURE-BIND binding-contract-unknown is reachable when broken.f compiles with the JIT selected; lqwprrlr deleted its only test (native-file-frame-e2e.f ~1221). Acceptance: a file in the same PROBE session selects the JIT and binds BROKEN's recovery record; the row asserts reason=binding-contract-unknown word=BROKEN; the T0 withheld assertion stays. Workspace: .jj-ws.noindex/jerry-nfrtest, brief /tmp/jerry-big/fix-nfrtest-brief.md. Claim: jerry.

Worker launched 2026-10-30 23:00 in .jj-ws.noindex/jerry-nfrtest on lqwprrlr.

Measured: the JIT cannot reach CAPTURE-BIND (native-definition-cache.f OBJECT-OPEN refuses files below tier 1) and both native tiers withhold a definition binding a recovery record. Second worker (brief /tmp/jerry-big/fix-nfrtest2-brief.md, same workspace) looks for another route to binding-contract-unknown: a `0 set-check` record, a persisted record without a contract; if none, the branch stays as the handler of CONTRACT$ answering false and nothing changes.
