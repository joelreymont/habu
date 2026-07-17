---
title: Verify PTX virtual machine state
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:09:10.200654+02:00"
blocks:
  - habu-ptx-phantom-preserving-3df9db92
---

Edge note 2026-07-17: blocker habu-ptx-m5-mask-eb0716f1 SATISFIED and removed
- the M5 uniformity/divergent-barrier model landed (commit e87cb494:
CTL-BARRIER structural flag at E-ADD-EFFECT, E-DIVERGENT-BARRIER rejection,
docs/type-families.md 9.1.2, fixtures lib/ptx/uniform-barrier-test.f). This
dot's "an M5-provided divergent barrier rejects" acceptance leg should compose
with that landed surface; the M5b remainder (uniform-branch acceptance,
explicit barrier marking) is tracked by habu-m5b-uniform-branch-f75d3e9e.

Problem: `lib/ptx/ir.f` is a value-numbered expression DAG while current emitters append PTX text, so a checked emitter stack effect cannot prove the generated virtual-register, predicate, address-space, control-flow, barrier, or resource-declaration state. Fix: add package `PTX-INSTRUCTION` with target-indexed instruction, operand, register-class, address-space, predicate, label, control, and resource-effect ADTs, package `PTX-STATE` with a deterministic verifier over the actual instruction CFG, and package `PTX-RENDER` that accepts only verified state. Virtual registers have nominal identity and exactly one definition; every use is dominated by a same-class definition; branch joins agree on live type/state; memory operations match pointer space and value type; declared resources agree with used resources. Reuse `habu-ptx-m5-mask-eb0716f1` as the sole owner of uniformity/divergent-barrier semantics and `habu-ptx-phantom-preserving-3df9db92` as the sole owner of phantom propagation through checked emitters. Acceptance negatives: undefined use, duplicate definition, wrong register class, predicate use before definition, predicate/control mismatch, incompatible branch join, address-space/type mismatch, missing resource declaration, contradictory declaration, and an M5-provided divergent barrier all reject before PTX text or an artifact is published, with structured instruction/location/expected/actual diagnostics. Acceptance positives: checked SAXPY, predicated tail, one uniform reduction barrier, and matrix lowering verify; rendering a verified fixture is deterministic and its pinned PTX is unchanged except where canonicalization is explicitly versioned. Ownership: new `lib/ptx/instruction.f`, `lib/ptx/instruction-test.f`, `lib/ptx/state-verify.f`, `lib/ptx/state-verify-test.f`, `lib/ptx/render.f`, and `lib/ptx/render-test.f`; render-gate adapters only in `lib/ptx/ir.f`, `lib/ptx/cg.f`, `lib/ptx/header.f`, `lib/ptx/cg-collective.f`, `src/arch/ptx/emit.f`, and `maki/lower-ew.f`, `maki/lower-red.f`, `maki/lower-mm.f`, `maki/lower-move.f`; required manifest, `FILEMAP.md`, and trust-inventory rows. Excludes M5 uniformity algorithms, phantom-preservation capability, physical allocation, `ptxas`, cubin, launch, and promotion. Verify: focused positive/negative suites, every exact emitter load path, `ptx-stdlib`, Maki lowering tests, typed-local-diff-lint, trust lint, host-lint, filemap-lint, native fixpoint and full native gate.
