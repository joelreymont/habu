---
title: Type AD DAG operations
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:05:32.643861+02:00"
blocks:
  - habu-pkg-ptx-autodiff-d15a611e
---

lib/ptx/ad-dag.f:14-22 declares the forward instruction vocabulary as nine raw integers. Raw create arrays in ad-dag.f:25-31, every fixture and tool producer (ad-dag-test.f, ad-dag-eval-test.f, ir-test.f, ad-ir.f, tools/ptx/ad-entry-lib.f, softmax-bwd*.f, softmax-fb-cg.f) store those integers through ptr a; AD-BUILD (:154-159) loads them as n. The same raw code then crosses three hand-written case tables in AD-DO-OP (:137-149), AD-EMIT-NODE (:163-175), and AD-VJP (:215-227), plus two more in ad-dag-eval.f:79-89 and :145-155. OP-DUP is legal in the input stream but illegal as a node; OP-LEAF is a node-only sentinel. The checker therefore cannot distinguish an input instruction from a node kind, a node id, or any n, and runtime AD-NODE-OP-CHECK repeats a manual membership table. After habu-pkg-ptx-autodiff-d15a611e gives the subsystem its package owner, use two package-owned ENUMs: input instruction {dup,bmax,bsub,exp,bsum,bdiv,mul,add} and stored node kind {leaf,bmax,bsub,exp,bsum,bdiv,mul,add}. Migrate instruction producers to typed fixed-capacity storage or a typed builder API, migrate AD-OP to LAYOUT-BUFFER, map instructions to node kinds explicitly, and replace every case with exhaustive MATCH. AD-BUILD/AD-DO-OP and evaluator APIs must take the instruction type, never ptr a/n; a node-only kind cannot enter an instruction buffer and dup cannot enter AD-OP. Delete raw OP-* constants and membership checks. Preserve operation order, graph identity, host gradients, emitted PTX, and device results byte-for-byte. Add checker negatives for n/node-kind/node-id swaps, exhaustive mutation coverage for every variant in every consumer, overflow/underflow parity, and exact before/after CODELEN, loaded JIT/DATA, PTX, and numerical gradient evidence; require no unexplained growth. Files: lib/ptx/ad-dag.f, ad-dag-eval.f, ad-ir.f, all focused tests, named tools/ptx producers, docs/autograd.md. Ownership: operation-domain typing only; node identity/optional-edge typing is separate.
