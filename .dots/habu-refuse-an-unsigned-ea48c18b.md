---
title: Refuse an unsigned does> definer at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T03:30:36.654259+03:00"
---

Problem: ~/.cache/tmp/carl-gfrest/c/c7-nosig-definer.f (`: MK create , does> ( -- n ) @ ;  5 MK X  X .`) is refused at tier 0 (`E-UNDEFINED habu: in mk: undefined word 'does>'`, `hook: non-certified definition: mk at 'does>'`, rc 70) and by tools/check.f (E-UNDEFINED at `does>`, rc 70); tier 1 accepts it and prints 5, rc 0, under the Habu loop and the engine loop alike (master 9751f482). Native: src/compiler/native/compiler.f:879 STAGE takes the engine's does> offset (DOES-BYTE@ :174-175) into M-DOES, so CHECK-RECORDED :347-349 runs CHECK-DOES-SPLIT :325-345, which certifies the head `MK create ,` and the clause apart (verdicts :330, :334); the checker's whole-body scan models `create` only after a signature (src/core/checker.f:16382 DEFINER-TOK, SGSEEN) and refuses at `does>`. The language requires the signature (docs/forth-card.md:70-71); an unsigned definer without `does>` (`: MK create , ;`) is accepted at both tiers. The same split certifies c9-tick-pending.f's clause (`['] MK` inside MK's own clause; tools/check.f: E-UNDEFINED in mk;does), which tier 1 then refuses later, -8651 at elaborate.f:3454-3457 DO-TICK, where tier 0 refuses E-UNDEFINED, rc 70.
Acceptance: at tier 1 c7-nosig-definer.f is refused with the checker's verdict (E-UNDEFINED at `does>`), rc 70, and publishes neither MK nor X; a signed definer (`: MK ( n -- ) create , does> ( -- n ) @ ;`) compiles and runs as before; c9-tick-pending.f stays refused. The reproducer joins test/compiler/native-create-does.f (tier 1).
Files: src/compiler/native/compiler.f, src/compiler/native/checker-owner.f if the owner ABI must answer the whole-body verdict, test/compiler/native-create-does.f.
Verify: rebuild bin/hb per docs/gate.md; the reproducer under `bin/hb --load test/outer-loop-on.f <file holding 1 set-tier> <case>`; `bin/hb --load test/compiler/native-create-does.f`; `bin/hb --load test/run.f`.
Depends: none.
Worker: worker.
Superseded: the one-pass codegen (docs/architecture.md, "The codegen is one pass over the checked events") deletes the tier-1 code this fixes; its reproducers become that codegen's cases. Do not start.
