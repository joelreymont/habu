---
title: "Transfer residency to a trap's live operands"
status: open
priority: 3
issue-type: task
created-at: "2026-09-30T11:38:43.415689+03:00"
---

Problem: a `hir.trap` whose cell is a live value, such as the routine's own argument (`( a -- )` passing `a` to `die`, `NORET-LEAF-FRAMED 1 0`), is refused before emission with `E-A64RAV-DKEEP` (-8611) on both machines. Host probes (C5's review, 2026-09-30): a copy of `test/compiler/native-trap.f` with the first trap cell `ARG+` throws `E-A64RAV-DKEEP` on ARM64; the x86 twin of `BUILD-TRAP` under `NORET-LEAF-FRAMED 1 0 0` dies rc 67 -8611.
Cause: no residency transfer for a trap. `DOP-XFER` has no `O-TRAP` arm (`src/compiler/native/select.f:2710-2721`, `select-x64.f:1524-1535`); ARM64's `RULE` passes a literal 0 mask for trap (`select.f:2757`) where `terminal` passes the real one (2758); x86 `EMIT-TRAP` (`select-x64.f:807-813`) stores every operand with no mask, unlike `CALL-SAVE` (709-716). The verifier (`regalloc-verify.f:1746-1750`) is right; `prune.f:14` already places the fix in selection.
Reach: source traps stage only fresh literals (`elaborate.f:2931-2944` `TRAP-ARGS`/`MATCH-TRAP`, 3100-3104 `DEAD-END`) and a source `die` call lowers as `terminal` (3146), so today only hand-built HIR reaches it.
Acceptance: both selectors transfer residency for a trap's operands as `terminal` does; the HIR above compiles and emits on both machines; a case in `test/compiler/native-trap.f` and one in `test/compiler/x64-emit-fixture.f`.
Files: `src/compiler/native/select.f`, `src/compiler/native/select-x64.f`, those two tests.
Verify: ARM64 is shared compiler code: rebuild, chain, gate. x86: `test/compiler/x64-emit.f`.
Route: krait lands it after the Linux proof; Alder pools the Mac gate.
Ownership: krait (Intel lane).
Claim: unassigned.
