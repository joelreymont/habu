---
title: "Refuse a scanless hook's zero verdict at tier 1"
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T05:34:48.932827+03:00"
---

Problem: at tier 1, under the Habu loop and the engine loop alike (engines 9751f482 and 3d8f5166), a program check hook that answers 0 without the lowering-certificate hook makes the compiler die instead of refusing the definition. ~/.cache/tmp/heron-arm64/evidence/gfcodegen/probes/lb/hk1.f (`: NOPE ( ptr u8 n -- n ) 2drop 0 ;  ' NOPE set-check  : F ( -- n ) 1 ;`) prints `lowering certificate source hash mismatch`, rc 76 (src/core/lower-cert-base.f:165); hk3 also runs its exit hook first; hk2 and the Gforth host's case r47 are the same class. Tier 0 refuses F `does> at <path>:3`, rc 70, and runs the exit hook. A hook's zero verdict refuses the definition; a die is not a refusal.
Acceptance: at tier 1, under both loops, a definition a program hook answers 0 for is refused as tier 1 refuses any zero verdict (E-NCOMP-VERDICT, rc 70, catchable, records retracted), with no certificate die, and an armed exit hook runs as at tier 0. hk1-hk3 join the native suite and r47 the Gforth host's test/gforth/cases/.
Files: src/core/lower-cert-base.f, src/compiler/native/compiler.f, the native test that holds the zero-verdict cases (test/compiler/native-hookless-reject.f), test/gforth/cases/.
Verify: native build per docs/gate.md; `bin/hb --load test/run.f`; `bin/hb --load test/outer-interpret.f`.
Depends: none. Worker: worker-max.
Superseded: the one-pass codegen (docs/architecture.md, "The codegen is one pass over the checked events") deletes the tier-1 code this fixes; its reproducers become that codegen's cases. Do not start.
