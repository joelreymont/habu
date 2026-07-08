---
title: Mechanical checker-guided repairer for the eval loop
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T12:11:42.956736+02:00"
---

repair-rounds in maki/eval-repair.f are counted from author-supplied trajectories. Build a mechanical repairer that consumes tools/repair-packet.f's structured repair classes (remove_producer/add_producer/fix_type/fix_return_stack) and applies them to a rejected candidate automatically, so repair-rounds/tokens-to-green are produced by the checker+repairer loop, not hand-authored.

## Landed

- `maki/eval-repair-mech.f` (package EVAL): `EVAL:MECH-REPAIR ( ptr u8 n -- n )`
  runs candidate -> `REPAIR-STEP` (shared metric engine, so `REPAIR-ROUNDS@` /
  `REPAIR-TOKENS@` / `REPAIR-GREEN?` read exactly as for author runs) -> on
  rejection: in-process JSON diagnostic capture (`DIAG-JSON!` + `DIAG-BUFFER!`
  + `DIAG-ORIGIN! 1 1 0` around `CHECK-CANDIDATE!`, fully checked, no TRUSTED)
  -> real repair packet via `RP-COUNT`/`RP-FIRST`/`RP-PACKET` -> parse packet ->
  class-dispatched edit -> re-check, up to `MECH-MAX-ROUNDS` (8) rejections.
  Results: `MECH-GREEN` / `MECH-UNREPAIRABLE` / `MECH-CAPPED`; `MECH-SOURCE$`
  and `MECH-CLASS$` expose the final source and last packet class.
- `maki/eval-repair-mech-test.f`, wired into `maki/test.f`: per-class fixtures,
  name-token guard, round cap, metric assertions, and a SAXPY surplus-load
  mech-vs-author A/B (mech: 3 rounds / 122 tokens; author: 1 round / 61 tokens).
  Author-trajectory path untouched.

## Mechanical edits per packet class (what packets actually carry)

- remove_producer: `token` + `byte_start`/`byte_end` name the excess producer
  (candidate-relative with origin 1 1 0; checker flags the LAST surplus
  producer) -> delete the span. `token_index` 0 flags the definition-name token
  (unconsumed-input surplus, e.g. `( i64 -- )` with empty body): nothing
  deletable -> UNREPAIRABLE.
- fix_return_stack: `return_stack.expected/actual` rows + flagged transfer
  token. Surplus (actual deeper) -> delete the flagged transfer (`>r`);
  a deficit carries no insertable token -> UNREPAIRABLE.
- add_producer: packet names only the flagged consumer + expected/actual rows;
  NO producer token, NO insertion point -> UNREPAIRABLE.
  Gap dot: habu-repair-packet-machine-879ad716.
- fix_type: packet has expected/actual rows + prose suggestion only; NO typed
  replacement token -> UNREPAIRABLE.
  Gap dot: habu-repair-packet-typed-62bc5df2.
- any other class (unknown_rejection, rewrite_uncheckable, ...): no modeled
  edit -> UNREPAIRABLE.

## Review finding: error-code collisions (fixed)

The first cut reused throw codes -5100..-5103 for E-MECH-*, which
maki/adjoint.f already owned (E-ADJ-KIND/-ID/-SAVE/-NONE) — a thrown -5100
was ambiguous. Nothing in the gate proved error-code uniqueness, and the same
class of collision already existed three times: E-CUDA/E-FUSE at -5002,
E-PTX-READBACK/E-MK-EVAL at -5003, E-LMV-NOOUT+E-LMV-REG/E-ABL-NOSUB+
E-ABL-CAP at -5210/-5211. Fixes in the same change:
- E-MECH-* renumbered to -5240..-5243 (eval-repair-mech owns the block).
- E-ABL-NOSUB/E-ABL-CAP -> -5250/-5251 (lower-move keeps -5205..-5212).
- E-FUSE -> -5004, E-MK-EVAL -> -5005 (lib/ptx device-boundary codes keep
  -5002/-5003; the maki misc block -5000.. keeps its neighbors).
- tools/error-code-lint.f (+ -core.f, -test.f) now enforces uniqueness of
  negative `-NNNN constant E-*` claims across src/ lib/ tools/ test/ maki/,
  wired into test/gate-stdlib-cases.f beside namespace-lint. Allowances:
  positive sysexits-style exit codes, E-*-FIRST/-LAST range sentinels, exact
  (code, name) re-registrations, and the frozen bootstrap/ seed.
