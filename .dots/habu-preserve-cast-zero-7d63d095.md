---
title: Preserve the zero-instruction CAST call contract on both tiers
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T23:25:17.713050+03:00"
---

B3's `cast` gate fails F18/F19: expected equal code-pointer spans of 40 bytes,
but callers containing two and six casts occupy 48 and 64 bytes. These assert
the public `docs/forth.md` contract that `CAST:` call sites emit no instructions.
The runtime values and nominal type checks pass; this is a code generation
contract regression, not grounds to bless the larger measurements.

Reproducer (measure `cp@` before and after each definition):

```forth
NEWTYPE role 0
CAST: >ROLE ( n -- role )
CAST: ROLE>N ( role -- n )
: NOCAST ( n -- n ) ;
: ONECAST ( n -- n ) >ROLE ROLE>N ;
: THREECAST ( n -- n ) >ROLE ROLE>N >ROLE ROLE>N >ROLE ROLE>N ;
```

The complete probe preserves its pointer measurements and disassembly in
`/home/joel/.cache/cedar-cast-field-triage-3lw270rd/cast-{spans,disasm-tier0,disasm-tier1}.f`.
On B3, tier 0 spans are 40/48/64; tier 1 spans are 24/44/60. Calling each with
11 returns 11. The span measurement includes the same constant-record overhead
in each case; it does not claim those totals are isolated routine lengths.

Responsible paths in source `f5f30e8f`: `src/habu/habu2.f:C-CAST` publishes an
empty colon-shaped identity. It still documents the former general inliner's
empty-span shortcut, but `C-CALL` now unconditionally emits `LCEMITBL`: one
four-byte call per cast. General tier-0 inlining was disabled because its raw
instruction scan cannot establish cross-tier body safety. Tier 1 likewise
routes these words through `NELAB:DO-WORD-CALL`/`STAGE-WCALL`, retaining calls
instead of expressing the checked retype as identity data flow.

Implement explicit checked-retype semantics owned by the CAST declaration and
consumed by both compiler tiers. Preserve that semantic identity through
dictionary publication, owner transfer and artifact capture/restore; keep
ordinary same-spelling words and nonempty trusted converters distinct. Do not
re-enable the retired general JIT inliner or infer CAST authority from a name.
Retain nominal rejection and runtime identity tests, and prove zero call-site
instructions at both tiers, including restored declarations. The exact shared
metadata interface belongs to the implementation step; no cast source changed
with this diagnosis.

B3 SHA256:
`2ef6c87b233f5bd1f4a0e009b806851890aa67e85ccb13844d5a4f8570143781`.
