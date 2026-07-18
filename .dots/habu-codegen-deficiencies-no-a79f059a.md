---
title: "Codegen deficiencies: no opt passes, naive JIT, hand-shaped emitters"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:09:44.303469+02:00"
---

Joel-confirmed finding (depth review 2026-07-18). THREE deficiencies: (1) NO optimizer anywhere native — zero const-fold/CSE/DCE/peephole in src/habu; the only opt pass is bootstrap/cg/opt.fs, 240 lines of gforth-side peephole (wrong side of the trust boundary, unavailable to the native engine); (2) the arm64 JIT is a naive stack-machine with inline stencils and a 91-line regalloc (src/habu/jit.f, regalloc.f) — adequate for engine/tooling duty per gate timings but never measured against optimized code; (3) each PTX kernel family is hand-shaped by its own emitter (lower-mm, lower-ew, lower-red...), which caps generality — a Triton reimpl handling arbitrary SPEC: dataflow cannot ship a hand-emitter per shape. FIX PATH: habu-ptx-opt-layer-325b9507 (typed PTX IR + target-independent opt, native-first) is the load-bearing remedy for (1)+(3) and joins the critical path if habu-codegen-verdict-roofline-4d6bf436 measures kernels far under roof; (2) is measure-first via the same verdict — do not rewrite the host JIT without evidence its naivety costs anything that matters.
