---
title: Package PTX autodiff passes
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:19:55.142362+02:00"
---

lib/ptx/ad.f, ad-dag.f, ad-dag-eval.f, ad-gen.f, ad-ir.f, and ad-saved.f expose more than 250 AD-/ADG-/ADE-/OP-/PTXIR- implementation words globally: graph arrays, node/value indexes, save policy/state, evaluator buffers, codegen cursors, and differentiation helpers. These are active PTX tool and Maki autograd dependencies. Give each distinct pass a real owner (PTX-AD, PTX-AD-DAG, PTX-AD-EVAL, PTX-AD-GEN, PTX-AD-IR), reopening only where files share one concern; export the minimal build/evaluate/differentiate/lower API and keep representation/state private. Remove raw compatibility globals. Coordinate representation migrations: habu-type-ptx-ad-56e323fc owns save-policy ENUM, habu-type-ad-dag-644833ca and habu-type-ad-dag-643a3aaa own operation/identity typing, and habu-pkg-ptx-vjp-1a41c708 owns the registry boundary. Preserve graph identity, op order, saved/recompute decisions, host gradients, finite differences, exact emitted PTX/register/resource counts, device results, errors, and allocation bounds. Add old-global/private rejection and qualified public positives per package. Measure dictionary-name bytes, loaded JIT/DATA, CODELEN, build/evaluation/generation latency, and PTX before/after; require no unexplained growth. Verify every AD/AD-DAG/eval/gen/IR/saved/autograd suite, ptx-stdlib, Maki, device goldens, typed-local diff, package/host/filemap/dot lints, fixpoint, and full native gate. Parent: habu-pkg-ptx-compiler-db8cfa46; package boundary and caller migration only.
