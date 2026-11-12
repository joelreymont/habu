---
title: Refuse an unbounded dereference outside the library
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T11:33:02.856773+03:00"
---

Problem: band 3 of habu-bound-pointers (habu-bound-pointers-with-719ba3f4): once src/lib/ and the consumers carry spans, the checker refuses the shape the deleted bare-copy lint only reported: a pointer derived by arithmetic (+, cell+, char+, 1+) from a raw-storage base or a bare ( ptr t ) parameter and then dereferenced (@ ! c@ c! xt@ xt!) or copied (BYTE-COPY) in checked source outside src/lib/ and src/. Acceptance: a per-row 'dereferences a stepped pointer' check in src/core/checker.f keyed on the pointee kind the raw-storage rule (TVK-RAW) and the forgery rule (TVK-BASE, habu-bound-ptr-arithmetic-8bf6b54a first half) already track, raised wherever the step is not private (settled 2026-11-12: restriction by privacy, never a source-owner gate; the shape comes from the design in work for habu-checked-src-reads-4f721c9b), with a named diagnostic in the E-RAW-CELL-PTR family and repair class use_span; rejected fixtures for each dereference word and the copy, certifying controls for SPAN:AT / SPAN:C@ and for a ( ptr u8 n ) read through SPAN:$; a rule-hosted generation build; docs/forth.md. Files: src/core/checker.f, src/core/render.f, test/compiler/, docs/forth.md. Verify: the fixtures; three generations with cmp; test/run.f; every consumer's own gate green against the rule engine (owners). Depends: habu-move-the-lib-0000e583, bounded pointer arithmetic (its dot habu-bound-ptr-arithmetic-8bf6b54a is in neither the tracker nor the archive; the open decision is on habu-carry-the-offset-8633d95a), the consumers' band-2 dots. Ownership: checker. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
