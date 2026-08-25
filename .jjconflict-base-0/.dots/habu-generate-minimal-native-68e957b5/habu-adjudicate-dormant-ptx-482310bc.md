---
title: Adjudicate dormant PTX optimizer
status: closed
priority: 2
issue-type: task
created-at: "2026-07-19T20:39:15.920013+02:00"
closed-at: "2026-08-20T21:01:45.830208+02:00"
close-reason: Adjudicated DELETE by the 2026-08-20 hard cut (habu-delete-the-nimm-329100c9); the census stays on this leaf, the SASS question travels to the PTXIR2 canonical boundary.
---

lib/ptx/opt-ir.f and lib/ptx/opt.f provide 622 lines of parse/render, copy propagation, constant folding, common-subexpression elimination, dead-code elimination, and peephole logic, but lib/ptx/opt.f:37 and an exact repository search prove integration is opt-in, disabled by default, and no production emitter or ptxas path calls PTX-MAYBE-OPT or OPT-PTX; only optimizer tests call it. A fresh production-emitter census measured: SAXPY 21->21 instructions, attention 103->103 while text grows 4 bytes, generic MMA matmul 364->361 and -72 text bytes, Maki GELU+RELU 34->34, Maki layernorm 122->120 and -41 bytes, Maki blocked 64x64 matmul 929->917 and -276 bytes. Thus the subsystem currently adds codebase and loaded-memory bloat without changing any shipped PTX, while proving nonzero PTX reductions on three real emitters. The attention result also falsifies lib/ptx/opt-ir.f:24's capacity invariant: OPTX-SRC-CAP and OPTX-OUT-CAP are both 128 KiB because the comment claims render output never exceeds input, yet canonical rendering grows a real module by four bytes. Any integration must preflight the exact rendered length or provide a proved larger/dynamic output arena; a valid maximum-size input must not fail merely because canonical spacing expands it. Root adjudication must be device-measured because ptxas may already remove the same PTX and make the pass redundant: assemble each before/after pair with the pinned target toolchains, compare cubin bytes and nvdisasm instruction/control-code output, run exact numerics and PERF-VERDICT bands. If cubin/SASS is byte-equivalent across the complete emitter matrix, delete opt-ir.f/opt.f and their unused toggle rather than maintain a second optimizer. If any target code shrinks or improves, make optimization mandatory at the single canonical module-to-ptxas/export boundary, remove the off-by-default toggle, preserve opaque-line barriers, and gate every emitter through it. Acceptance: one evidence-backed outcome, no dormant optimizer path, complete Maki/PTX producer watch set, idempotence and malformed-input rejection, maximum-capacity render growth proof, sm_87 and sm_121 device proof, ptx-stdlib/Maki/full gates green. Update habu-codegen-deficiencies-no-a79f059a with the verdict.

ADJUDICATED BY THE USER'S HARD-CUT RULING (2026-08-20): DELETE. This leaf
offered two branches and both of them ended in the same place. The
byte-equivalent branch says delete. The win branch says "make optimization
mandatory at the single canonical module-to-ptxas/export boundary" - which is a
rebuild at a boundary that does not exist yet, not a reason to keep a
line-oriented text pass no emitter calls. A device measurement can only choose
between two futures, and neither of them is the dormant code. So the code went
first: habu-delete-the-nimm-329100c9 deleted lib/ptx/opt.f, lib/ptx/opt-ir.f,
lib/ptx/opt-test.f, lib/ptx/opt-ir-test.f, their suite and fork rows, their two
perf-watch producer rows and their two error codes. The measured census above
(SAXPY 21->21; attention 103->103 with +4 text bytes; MMA matmul 364->361,
-72B; Maki GELU+RELU 34->34; Maki layernorm 122->120, -41B; Maki blocked 64x64
matmul 929->917, -276B) is the durable part of this leaf and stays here as the
evidence any future PTX optimizer starts from.

What travels: whether sm_87/sm_121 SASS still shrinks after ptxas has had its
turn is a real question, and it belongs to whoever owns the PTXIR2 canonical
module-to-ptxas boundary (design section 8.8 and the lib/ptx/ir2/ layout in
section 13). It is not a question about deleted code. What dies with the code:
the four-byte render-growth bug against opt-ir.f:24's never-exceeds-input
capacity claim. If a PTXIR renderer is ever written, that counterexample is the
first test to write - canonical spacing can make output longer than input, so
the output arena must be preflighted or dynamic.
