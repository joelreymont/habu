---
title: Verify PTX virtual machine state
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:09:10.200654+02:00"
blocks:
  - habu-ptx-phantom-preserving-3df9db92
  - habu-freeze-compiler-baseline-b9777eee
  - habu-encode-compiler-ir-545ee6d1
  - habu-type-dsl-prove-93da83c4
---

Edge note 2026-07-17: blocker habu-ptx-m5-mask-eb0716f1 SATISFIED and removed
- the M5 uniformity/divergent-barrier model landed (commit e87cb494:
CTL-BARRIER structural flag at E-ADD-EFFECT, E-DIVERGENT-BARRIER rejection,
docs/type-families.md 9.1.2, fixtures lib/ptx/uniform-barrier-test.f). This
dot's "an M5-provided divergent barrier rejects" acceptance leg should compose
with that landed surface; the M5b remainder (uniform-branch acceptance,
explicit barrier marking) is tracked by habu-m5b-uniform-branch-f75d3e9e.

Compiler-IR reconciliation: this dot is the sole `GPU-PTXIR2` instruction schema,
state verifier, and renderer owner for design section 8.7 and GPU Wave A. Define
target-indexed instruction, operand, virtual-register class, address space,
predicate, label, control, and resource-effect `ENUM`/`STRUCTURE` records; verify
the actual control-flow graph independently; render only verified state. Reuse
the existing uniformity and phantom owners. Undefined, duplicate, wrong-class,
predicate/control, join, address-space/type, resource, target, and divergent
barrier mutations reject before text or artifact publication. Checked SAXPY,
tail, reduction-barrier, and matrix fixtures render deterministically. External
parser roundtrip remains separate. Excludes RIR/KIR/GIR, physical register
assignment, cubin/launch, and promotion.

GROOMED 2026-08-04 (dot-groom). Dangling blockers repointed. Both PTX M5 dots named above are
gone from the graph. habu-ptx-m5-mask-eb0716f1 was closed by commit e33770093 "Close M5 dot;
mint M5b remainder; doc uniformity model", which minted habu-m5b-uniform-branch-f75d3e9e as
the remainder leaf; that successor has since been closed and archived too. So the explicit
barrier-marking work this dot deferred to M5b is no longer tracked anywhere - if it is still
wanted, it needs a fresh dot rather than a pointer.
