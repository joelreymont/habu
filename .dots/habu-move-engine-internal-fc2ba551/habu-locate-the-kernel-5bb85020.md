---
title: "Locate the kernel's primitive table in a header cell"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T09:14:39.375811+02:00"
closed-at: "2026-11-11T09:46:09.155208+02:00"
close-reason: "landed with this commit: PRIM-TABLE-CELL at $2CF0 holds the kernel's primitive table offset, written by a kernel-object preset; link-kernel.f SECTIONS accepts an optional last section (CHECK-ROWS-AT, CHECK-ROWS-U); probe count 271; two engine generations equal, the suite (273 of 273) and the chain check rc 0; the consumer measure green on the candidate (~/.cache/tmp/jerry-census/m8)"
---

Problem: an engine build finds the running hb's primitive bodies by walking hb's dictionary by name (src/arch/arm64/link-kernel.f HOST-ENTRIES :268, KEEP :259), so an hb with its private names stripped cannot build an engine; the kernel's seed table (habu1.f EMIT-DICT, LNCOUNT then LDICT) holds every primitive by name but nothing tells a running hb where it is. Step 1 of the privacy design (.jj-ws.noindex/jerry-wasm/.out/privacy-design.md). Acceptance: a new header cell DICT-CELL (layout.f, claimed in data-claims.f) holds the seed table's offset from RBASE-CELL, written by a kernel-object preset; link-kernel.f SECTIONS accepts one more last section, the primitive rows (ROWS-AT, ROWS-U), and nothing reads either yet. Files: src/habu/layout.f, src/habu/data-claims.f, src/arch/arm64/kernel-object.f, src/arch/arm64/link-kernel.f. Verify: three engine generations rc 0, hb2 and hb3 equal up to the signature; a probe program reads the cell and prints the count (271) and the first and last names; an object with a rows section builds with the new reader and is refused by the installed one. Depends: none. Ownership: the four files. Claim: agent=jerry workspace=.jj-ws.noindex/priv-step1. Brief: .jj-ws.noindex/jerry-wasm/.out/briefs/priv-step1.md.

State 2026-11-11: step 1 is commit c71ed92c in .jj-ws.noindex/priv-step1 (DICT-CELL at $2CF0; hb1, hb2, hb3 rc 0, hb2 and hb3 equal up to the signature; probe count 271; a rows section builds with the new reader and is refused rc 74 by the installed one). Under review (briefs/review-priv-step1.md); then the suite, the chain check, the consumer measure, push and install.

Review 2026-11-11 (code-reviewer): REVISE, code correct; four comment and name fold-ins, all taken (.jj-ws.noindex/jerry-wasm/.out/review-priv-step1-findings.md): the cell is PRIM-TABLE-CELL (DICT-CELL collided with util.f DICT-CELL@ and layout.f's DATA record dictionary), the reader's variables are CHECK-ROWS-AT and CHECK-ROWS-U (ROWS already names the object's primitive rows section). F5, three copies of label-to-offset, is dot habu-one-public-word-be7607cb. Folded by a fresh worker (briefs/priv-step1-fold.md): hb1, hb2, hb3 rc 0, hb2 and hb3 equal up to the signature, the probe prints count 271. Stacked on link-crafts (dot habu-refuse-a-slot-2eee435e) for one landing.
