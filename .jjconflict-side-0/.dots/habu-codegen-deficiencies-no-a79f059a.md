---
title: "Codegen deficiencies: no opt passes, naive JIT, hand-shaped emitters"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:09:44.303469+02:00"
---

Joel-confirmed finding (depth review 2026-07-18). THREE deficiencies: (1) NO optimizer anywhere native — zero const-fold/CSE/DCE/peephole in src/habu; the only opt pass is bootstrap/cg/opt.fs, 240 lines of gforth-side peephole (wrong side of the trust boundary, unavailable to the native engine); (2) the arm64 JIT is a naive stack-machine with inline stencils and a 91-line regalloc (src/habu/jit.f, regalloc.f) — adequate for engine/tooling duty per gate timings but never measured against optimized code; (3) each PTX kernel family is hand-shaped by its own emitter (lower-mm, lower-ew, lower-red...), which caps generality — a Triton reimpl handling arbitrary SPEC: dataflow cannot ship a hand-emitter per shape. FIX PATH: habu-ptx-opt-layer-325b9507 (typed PTX IR + target-independent opt, native-first) is the load-bearing remedy for (1)+(3) and joins the critical path if habu-codegen-verdict-roofline-4d6bf436 measures kernels far under roof; (2) is measure-first via the same verdict — do not rewrite the host JIT without evidence its naivety costs anything that matters.

2026-07-19 SIZE MEASURED (Mac review, exact ARM64 output): `+` 48 B, `dup` 44 B, `swap` 52 B, `+ 1 +` 88 B (complete spill/pop/push repeated per prim), empty leaf 20 B frame instead of bare ret. Pre/post-indexed addressing alone would cut the operational part of `+` 28->16 B, `dup` 24->8, `swap` 32->8. So (2)'s CODE-SIZE inefficiency is now measured; RUNTIME-speed cost remains unproven and the measure-first guard for any JIT rewrite stands unchanged. MATCH-specific slimming split out as mechanical dots habu-slim-match-emitted-66941fb5 (B.cond forward-patching, single tag load, imm cmp, out-of-line die tail) then habu-factor-repeated-match-95a4db2e (compiler-stencil factoring, byte-identical emitted code) — independent of the opt-layer question.
