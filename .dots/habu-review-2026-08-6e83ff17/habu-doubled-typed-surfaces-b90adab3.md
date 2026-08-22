---
title: doubled typed surfaces and nineteen OK extractors
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.946942+02:00"
---

Problem: lib/string.f:275-397 re-states FIND-SUB/INDEX-OF/SPLIT-NEXT/BUF-* under package STR as one-line wrappers; memory.f:92-257 and vector.f:463-567 likewise; the 7-arm 'MATCH CAD-NUM:numeric-result ok OF ... ;MATCH' extractor appears 19 times (string.f:298-325, memory.f:100-153, vector.f:472-504, byte-buffer.f:737-743, cad-num-arithmetic.f:275-298) differing only in the throw code. STR:COUNT 0 consumers; MEM:ALLOC-64K/64K-COUNT-FOR/64K-SPAN-BYTES/64K-BYTES/CELLS>BYTES/UNMAP test-only; VEC: 2 tools + 1 maki. Acceptance: one extractor '( numeric-result n -- a )' taking the code, or the typed re-statements deleted in favour of one surface; consumers updated; tests green. Files: lib/string.f, memory.f, vector.f, byte-buffer.f, cad-num-arithmetic.f. Verify: lib tests. Depends: the CAD-NUM shim dot (same surface). Ownership: lib surfaces. Claim: unassigned.
