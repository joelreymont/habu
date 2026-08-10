---
title: Inline string bodies in the emission
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:38:00.867136+02:00"
---

The long-term form (option B, aotsite lane 2026-08-11): put string bodies INLINE in the chain's emission addressed PC-relatively (the engine's own C-SDQ/C-ADR shape, AOT/ASLR-safe by construction) - retires NSTR outright and makes the whole DATA-address relocation class for strings not exist. Cost: A64EMIT must place non-instruction words, and every walker that assumes word=instruction (RELOC-CALLS' BL? decode, BRANCH-CK, TAIL-RELOC-CK, the source map, INSNS, the inliner's copy-safety scan, codegen byte comparisons) must learn the data span. Staged AFTER the cut; the bake+intern pair suffices for it. Files: src/compiler/native/{emit,publish,string}.f + walkers. Depends: the cut a5aa3f1f.
