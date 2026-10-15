---
title: Share capture readers between the linkers
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.076990+03:00"
closed-at: "2026-10-15T18:35:06.602743+03:00"
close-reason: "Landed: one capture-row reader set in src/habu/aot-decl.f serves both linkers; wasm uses the x64 binary-search ROW-AT; x64 and wasm link output byte-identical."
---

Problem: link-x64.f:114-125, 406-410, 516-517 and link-wasm.f:86-122 each carry their own capture-record readers (CREC, SH@, SITE@, XT@ and others). link-wasm.f:331-340 ROW-AT searches linearly per site, O(sites x routines), while link-x64.f:158-165 binary-searches. Acceptance: one reader set shared through aot-shadow.f or aot-decl.f; link-wasm uses the binary search; x64 and wasm link output byte-identical. Files: src/habu/link-x64.f, src/habu/link-wasm.f, the shared home. Verify: x64 and wasm link tests; wasm run via test/wasm/run.mjs. Depends: none. Ownership: listed files. Claim: unassigned.
