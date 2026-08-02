---
title: Compile scalar float arithmetic
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-02T21:52:54.037513+02:00\""
---

First float compiler leaf, built against the semantics contract in tools/codegen-compare-corpus3.f's survey (read it FIRST - it is the acceptance spec). Scope: the straight-line scalar subset - f+ f- f* f/ fnegate fabs fsqrt, float literals (the tape must carry them: E-NFEED-KIND today - the reader's K-REAL class exists; the literal's cell IS its bits, so the tape carries the bits and the survey's reader-bug dot habu-fix-the-float-1d1467c8 stays independent), s>f and f>s conversions. Pieces: an FPR value class end to end (the contracts already carry FPR sets - A64EFF:FPR-NONE etc.; the dialect needs a float register type beside GPR-TYPE, the allocator a second register file with its own pool - read how C-GPR/C-TOKEN classes work and add C-FPR honestly, hulls and classes unchanged), D-register forms (a64.fadd/fsub/fmul/fdiv/fneg/fabs/fsqrt/scvtf/fcvtzs/fmov-immediate-or-literal-load - match the engine: 14 words are ONE instruction each, the survey lists the exact encodings the old emitter uses), asm.f encoders (check what exists; add in style, pinned in insn tests), float block arguments and data-stack crossing (a double is one cell - dtake/dload/dstore work unchanged? verify: the value class changes but the slot traffic does not; the convention's FPR argument places are dotted separately if the C-ABI float-args question arises - for data-stack words it should not), locals of type r. Acceptance: the SGD and SEG-1/SQRT rows compile and their gap rows retire with bit-exact outputs; every survey semantic honored (NaN propagation is hardware, conversions are the same instructions - the risk is literal materialization and evaluation order, both pinned). Mutations: literal bits off by one ulp (bit-exact output kill), operand order on f- and f/ (execution), wrong register class handed to a float op (validator). Then float compare/branch (RELU-F MAX-F FROUND) and the loop kernels are the following leaves.

Claim: agent=floatlane workspace=.jj-ws/habu-compile-scalar-float-4f1c250f
