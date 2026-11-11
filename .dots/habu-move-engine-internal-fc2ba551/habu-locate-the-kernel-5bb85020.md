---
title: "Locate the kernel's primitive table in a header cell"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T09:14:39.375811+02:00"
---

Problem: an engine build finds the running hb's primitive bodies by walking hb's dictionary by name (src/arch/arm64/link-kernel.f HOST-ENTRIES :268, KEEP :259), so an hb with its private names stripped cannot build an engine; the kernel's seed table (habu1.f EMIT-DICT, LNCOUNT then LDICT) holds every primitive by name but nothing tells a running hb where it is. Step 1 of the privacy design (.jj-ws.noindex/jerry-wasm/.out/privacy-design.md). Acceptance: a new header cell DICT-CELL (layout.f, claimed in data-claims.f) holds the seed table's offset from RBASE-CELL, written by a kernel-object preset; link-kernel.f SECTIONS accepts one more last section, the primitive rows (ROWS-AT, ROWS-U), and nothing reads either yet. Files: src/habu/layout.f, src/habu/data-claims.f, src/arch/arm64/kernel-object.f, src/arch/arm64/link-kernel.f. Verify: three engine generations rc 0, hb2 and hb3 equal up to the signature; a probe program reads the cell and prints the count (271) and the first and last names; an object with a rows section builds with the new reader and is refused by the installed one. Depends: none. Ownership: the four files. Claim: agent=jerry workspace=.jj-ws.noindex/priv-step1. Brief: .jj-ws.noindex/jerry-wasm/.out/briefs/priv-step1.md.

State 2026-11-11: step 1 is commit c71ed92c in .jj-ws.noindex/priv-step1 (DICT-CELL at $2CF0; hb1, hb2, hb3 rc 0, hb2 and hb3 equal up to the signature; probe count 271; a rows section builds with the new reader and is refused rc 74 by the installed one). Under review (briefs/review-priv-step1.md); then the suite, the chain check, the consumer measure, push and install.
