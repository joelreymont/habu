---
title: Enforce the field-projection arming boundary after owner transfer
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T23:25:17.713050+03:00"
---

B3's `field-proj` gate fails F17: ordinary source calling `FIELD-PROJ!`
certifies (-1), where the fixture expected an unresolved word (1). This is a
missing boundary, not an obsolete acceptance expectation. Public reproducer:

```forth
s" FPX-FORGE ( ptr u8 n n n -- ) FIELD-PROJ!" CHECK-CANDIDATE! .
: FPX-FORGE ( ptr u8 n n n -- ) FIELD-PROJ! ;
s" FIELD-PROJ!" 0 search-wl .
```

At both tiers the definition loads successfully; the candidate returns -1 and
lookup returns a callable address. B3 has an active user effect for symbol
3726 (minimum input 4), no primitive policy row, and no `DNAME-INT` flag.
`FIELD-PROJ!` had relied on its pre-hook definition lacking a user signature.
Source-owner transfer makes that assumption false. The existing same-symbol
primitive restriction cannot help without a policy row.

The four raw state cells (`FIELD-PROJ-A/U/FID/OFF`) were also callable, and
`FP-CELL-CLEAR ( -- ) 0 FIELD-PROJ-U !` certified. Protecting the armer alone
would leave that direct bypass open. The bounded correction adds trusted-only
effects and `REG-PROTECT` for the armer and exactly these four cells. Candidate
refusal is now 0; record protection also rejects direct execution and tick.
The user effects remain intact.
`docs/type-families.md` states this explicit policy instead of relying on load
order. Same-spelling package and local symbols retain their own meanings.

The focused regression loads current checker source, replays the declaration
and asserts an active user row, then executes the production sealing pass.
It covers both tiers, real evaluator rejection before publication, direct
execution/tick and raw mutation refusal, shadow controls, a trusted raw-cell
round trip, and the existing field projection suite's trusted generated
accessors with exact scalar/offset/generic reads. `test/field-proj-boundary.f`
passes on `hb-indexed-B` (source `f5f30e8f`, SHA256
`70ccdba483dc7356f501c32fbd22b31a14d36373233ef2a6aa5a0277afc8edb1`).
The original-source control accepts the armer and raw-cell candidates and
compiled callers at both tiers after the same declaration replay/sealing
setup. The existing protection registry has 65 of 192 slots occupied in B;
the repair adds five registrations without changing its capacity.
The integration owner performs independent review and rebuilt-engine gates.

Evidence: `/home/joel/.cache/cedar-cast-field-triage-3lw270rd/`.

The rebuilt product E exposed one follow-up: its retained policy refused a
tier-1 source rebuild of `CHECKER-CAPTURE-SCRATCH-PREPARE` at `FIELD-PROJ-A`
(`E-CAP-TRUSTED`, then native verdict -8579). Reproducer from source a99cd3fc:

```sh
/tmp/cedar-family-stage-abi/hb-graph-persist-E --load \
  test/compiler/aot-mode.f test/native-window-owner-child.f -- \
  /home/joel/.cache/cedar-cast-identity-rYkeN5/field-owner-ok.f
```

The failure precedes the fixture and owner transfer.

Raw access now lives in tiny protected helpers for scratch reset, name read
and schema read; the existing armer and clear operations are trusted storage
boundaries. Capture cleanup, accessor-name comparison and field-schema
validation remain checked, with their existing effects and behavior.
`test/field-proj-boundary.f` also starts tier 1 before the source-owner window,
verifies native origin for the rebuilt helpers, and repeats the generated
accessor positives and ordinary-user refusals. All three paths pass on E
(SHA256 `cefd25fdbd7db8126fb0f63feeb834b662eed2569c7a4cd49584ed69fef1c3d0`).
Before/after logs: `/home/joel/.cache/cedar-cast-identity-rYkeN5/field-native-owner-E.log`
and `field-rebuild-regression.log`.

Original B3 SHA256:
`2ef6c87b233f5bd1f4a0e009b806851890aa67e85ccb13844d5a4f8570143781`.
