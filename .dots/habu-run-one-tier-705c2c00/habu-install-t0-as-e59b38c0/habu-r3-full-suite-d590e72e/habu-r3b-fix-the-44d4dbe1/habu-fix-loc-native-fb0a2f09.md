---
title: "fix loc: native refusals name the token's file and line"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.777141+02:00"
---

Problem: master's JIT refusals carry ' at <path>:<line>' (E-UNDEFINED: CKT-MISSING at …/fwd.f:1; hb: ; with a quotation open: RXSC at …:1); the native tiers print none (ncomp: cannot compile RXSC at ;). Tests that ratified the location-less text: tools/check-test-lib.f:1345, :1786, :7197, :3233; test/c2-memory-refusals.f:40; runtime-regression RXSC/RXSF legs. Acceptance: every native refusal names its token's path and line with the JIT's own location code; those tests assert the location again. Files: src/compiler/native/definition.f REPORT-FAILURE/REPORT-UNDEFINED, src/habu/outer.f AT-SOURCE/LINE, the listed tests. Verify: rows named in /tmp/jerry-big/fix-loc-brief.md. Depends: none. Ownership: those files. Workspace: .jj-ws.noindex/jerry-loc. Claim: jerry.
