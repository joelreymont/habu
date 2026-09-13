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

The implementation assigns the remaining value 3 of the existing two-bit
`DKIND` enum to CAST. The declaration stamps its own record; the JIT skips only
that resolved binding before spilling cached values, and the native word model
uses its existing one-input identity rename. Fixed-value/address readers now
compare exact enum values. First-class execution retains the real cast body.
Capture already transports both kind bits; no record shape or AOT version
changes. The recovery emitter stamps the same enum and clears fourteen name
flag bits, matching production.

Private source-built product `hb-cast-native` (SHA256
`7e88e5d5a0ef100a49cf79e4e6eec60f05b73b94b8ac31e4b595069ead64907e`)
passes the new-declaration span/value controls, unchanged nominal rejection
suite, package/local/prior-binding shadows, non-cast callees, native model and
binding suites, and a real APP-IMAGE save/restore whose persisted declarations
have equal caller spans and valid first-class execution at both tiers. Evidence
is in `/home/joel/.cache/cedar-cast-identity-rYkeN5/`.

Migration remains explicit: this first product was built by old E, so its baked
core role casts were declared with kind 0. They remain ordinary calls: >IDX/IDX>N
give 40/48 byte spans at tier 0 and 24/44 at tier 1. The durable `CORE-ROLE`
span assertion detects exactly this remaining failure in each tier. Build the
next product with a CAST-capable host to stamp its captured core declarations;
do not infer semantic identity from old body bytes. Root owns that composed
product build and the full gate. The standalone second build was stopped before
completion to avoid duplicating that work.

Recovery execution is not claimed: the same Gforth probe refuses rc70 with no
output on both unchanged parent and changed emitters. Production cold emission
and the native product build succeeded; the recovery startup refusal remains a
separate validation limit.
