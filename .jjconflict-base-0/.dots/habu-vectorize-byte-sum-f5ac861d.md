---
title: Vectorize BYTE-SUM with a 16-byte strip and scalar tail
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T20:15:36.586033+02:00"
blocks:
  - habu-give-the-ir-bba00e2a
  - habu-decide-and-build-4ae94353
---

The pass the three modelled SIMD forms exist for. Rows and encoders landed in 'codegen: model the byte-strip SIMD forms' (bookmark neon): Ld1v/Uaddlv/Umovh in formal/Common/Insn.v with ENC-LD1V/ENC-UADDLV/ENC-UMOVH in src/arch/arm64/asm.f. No emitter uses them yet - this is that emitter.

MEASURED TARGET, one row. CODEGEN-CORPUS:BYTE-SUM is among the ten largest chain-vs-clang time gaps: chain 12.433 ns vs clang 3.983 ns, gap 8.450 ns (a later run measured 13.663/6.882/6.781 - the gap is real, its size moves with the run). Chain 80 bytes, clang 516. The clang twin IS vectorized, heavily: 8 accumulators, 32 bytes per iteration, uaddw2 widening, a tbl-based epilogue.

BYTE-FIND IS EXPLICITLY NOT IN SCOPE, and that is a measurement rather than a scoping preference. clang does NOT vectorize it - the early exit blocks auto-vectorization, and hc1_byte_find at -O2 is eleven scalar instructions, 48 bytes. It is absent from the top-ten time gaps entirely; its 52-byte gap is a SCALAR codegen gap (the -1 literal chain and loop overhead). A NEON lane cannot close it and should not claim it. Same question must be asked of COUNT-CHAR before scoping it.

STRIP DESIGN the three forms were chosen for, and nothing speculative beyond it: acc=0; per 16-byte strip, ld1 {v.16b},[addr]; uaddlv h,v.16b; umov w,v.h[0]; add acc. UADDLV over sixteen unsigned bytes cannot overflow its 16-bit result (16*255=4080). Per-strip reduction needs no vector accumulator and no cross-strip vector add - which is why ADD.16B/ADDV/CMEQ were deliberately NOT modelled. The pinned corpus subject is 21 bytes ('habu codegen baseline'), so exactly ONE strip plus a 5-byte scalar tail, and per-strip reduction is optimal there. A long-input variant wanting cross-strip accumulation needs UADDW/UADDW2+ADDV as a FOLLOW-ON row set under the same CG-02 discipline - model first, emit second.

MODEL THE PASS ON src/compiler/native/combine.f. It is the only pass between selection and allocation (driver src/compiler/native/migrate.f:682-694 EMITTED; the multiply-add fold sits at migrate.f:609-623), and it already solves this lane's hardest acceptance problem. COUNT FIRST, REBUILD ONLY ON A HIT: migrate.f:609-612 releases and returns the ORIGINAL module when the fusion count is zero, because rebuilding a module renumbers its values and the allocator breaks ties on those numbers, so a routine that gained nothing would still come out with different bytes (migrate.f:601-608). A vectorizer that rebuilds unconditionally perturbs every unrelated row and fails 'no untouched row changes bytes' on its own.

ACCEPTANCE: BYTE-SUM improves with the emitted instruction delta shown and BOTH gaps reported (chain-vs-clang closed, chain-vs-own-baseline gained); answers bit-for-bit including the empty span; no untouched row changes bytes; codegen-compare 0 findings before re-pinning with --update-chain.

Found by agent neon while scoping habu-vectorize-the-byte-a0da35a7.
