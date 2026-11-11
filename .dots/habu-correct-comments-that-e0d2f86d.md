---
title: Correct comments that say a quotation cannot read a local
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:02:03.659543+02:00"
---

Problem: T0 compiles a quotation that reads its word's locals (arm64; Wasm with dot habu-compile-a-quotation-82916e39), yet comments still say it cannot, some beside globals kept only as that workaround: src/compiler/ir/context.f:251, src/core/sumtype.f:1937 and :2296, src/habu/verify-source.f:4373 and :4502, src/lib/net/http.f:203 :273 :712, src/lib/net/http-response.f:273, src/lib/net/http-test.f:173 :348 :460, test/engine-suite.f:507, test/checker-dup-record.f:18, test/certify-does-definer.f:353, test/checker-verify-pkg-scope.f:21. A comment whose quotation goes to a task stays true (E-LOCAL-ESCAPE). Acceptance: each listed comment is true (corrected or removed, by forth-card '9 Comments'); a global that exists only as the workaround becomes a local read by the quotation, where x86-64 does not compile the file (x86-64 refuses the read until dot habu-compile-a-quotation-ed6d5042). Files: those listed. Verify: each changed file's own test rc 0. Depends: habu-compile-a-quotation-82916e39. Ownership: none. Claim: unassigned.
