---
title: AOT-REPL bl-compaction + Linux size baseline
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T17:59:14.114649+02:00"
---

Milestone B stretch (deferred, STOPPED-with-evidence). AOT-REPL net is now +12332B __text (12.04KB, under the accepted 12.5KB). Reaching the ~11.7KB stretch needs bl-compaction: rewrite the 156 captured 16B inter-word calls (movz/movk/movk x16 ; blr x16) to 4B PC-relative bl and reflow the blob (remove 12B/call), est blob 21660->19788 (-1872) => net ~10.5KB. STOPPED because it is a full mini-linker: the blob has 119 intra-blob PC-relative instructions that all need delta re-encoding under any reflow (49 B/BL, 8 B.cond, 55 CBZ, 7 ADR; no ADRP/LDR-literal), plus every reloc/record offset table must be remapped. High correctness risk vs already-under-budget. To do it safely: build the reflow via src/arch/arm64/disasm.f decoders + encoders, and add a build-time disassembly-equivalence proof (like the ACAP-PROVE-RECS recmm=0 record proof) that every non-call instruction maps to the same absolute target after reflow and each call became a bl to the same callee, plus keep the movz/movk fallback for any callee out of +-128MB bl range. ALSO: test/gate-build-size.f GB-SIZE-BASELINE-LINUX is still 90304 (stale); the AOT sections shrank ~6240B (platform-independent ARM64) so the Linux candidate will STALE-fail the ratchet until re-measured on a Linux host and lowered. ALSO: compact records use u16 blob-off/end (fail-closed asserts if the REPL blob ever exceeds 64K); if it grows past 64K, widen those record/reloc/site fields back to u32. Files: src/habu/aot-capture.f (blob reflow + relink + proof), src/habu/habu2.f (EM-AOT-COPY-BLOB/PATCH-SITES bl patch), test/gate-build-size.f (Linux row).
