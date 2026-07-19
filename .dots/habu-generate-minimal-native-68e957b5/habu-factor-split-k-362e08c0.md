---
title: Factor split K loop control
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:54:22.109835+02:00"
---

Current master bloat: lib/ptx/cg-mma.f:1189-1202 and 1370-1383 contain byte-for-byte duplicate double-buffer K-loop skeletons; only CPP-KT-INIT, CPP-KGUARD, and CPP-PF-TEST are replaced by their SPLIT variants. The single-buffer skeleton at 1213-1220 is cloned at 1384-1391 with only init and guard changed. The source comment explicitly describes the copies as verbatim. Setup is cloned too: GROUP and WIDE at 581-597 duplicate the same tid.x/tid.y/ctaid.x/ctaid.y/mad.lo sequence and SH assignment, and MMA-THREAD-SETUP-SPLIT at 1347-1365 copies those six emitted instructions a third time before its genuine split derivation. Factor one byte-identical thread/base prologue plus one single-buffer and one double-buffer skeleton over a typed full-range versus split-range policy, or checked range-specific quotations, so only CTA remap, split range/base, and the three genuine bound emitters differ. Do not add ambient raw mode integers; consume the shared typed mma-config/k-range from habu-structure-mma-autotune-e02d3197. Preserve emitted PTX byte-for-byte for split=1 and every split winner, cp.async typestate ordering, labels, barriers, prefetch overlap, and stage restrictions. Prove exact PTX/disassembly goldens for group/wide/split setup and full/split stages 1/2, mutation tests changing each bound hook independently, checker negatives for wrong-effect hooks/policies, and no duplicate setup/skeleton census. Measure source definitions/lines, loaded JIT/DATA/CODELEN, generated PTX size, and emit/device performance before and after. The closed split-K dot retains feature/performance history; this owns only duplicated construction.

This is retained-feature factoring only. The measured-losing path has no production consumer, so habu-remove-losing-split-5639823d is the default owner and deletes these clones. Implement this dot only if a concrete retained product requirement with target measurements is established first.
