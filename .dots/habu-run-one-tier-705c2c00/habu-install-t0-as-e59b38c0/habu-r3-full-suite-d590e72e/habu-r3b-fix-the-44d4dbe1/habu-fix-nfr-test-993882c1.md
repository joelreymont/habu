---
title: "fix nfr-test: test the unknown binding contract with the JIT"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.815634+02:00"
---

Problem: tools/native-unit-dependency.f:1413 CAPTURE-BIND binding-contract-unknown is reachable when broken.f compiles with the JIT selected; lqwprrlr deleted its only test (native-file-frame-e2e.f ~1221). Acceptance: a file in the same PROBE session selects the JIT and binds BROKEN's recovery record; the row asserts reason=binding-contract-unknown word=BROKEN; the T0 withheld assertion stays. Workspace: .jj-ws.noindex/jerry-nfrtest, brief /tmp/jerry-big/fix-nfrtest-brief.md. Claim: jerry.
