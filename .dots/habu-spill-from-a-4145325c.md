---
title: Spill from a block that is neither the entry nor the exit
status: active
priority: 3
issue-type: task
created-at: "\"2026-08-01T20:29:42.092121+02:00\""
---

src/compiler/native/regalloc.f (MB-KEEP-BLOCK) will only spill a value whose definition and every read are in the block the caller enters or the block control leaves through. The reason is the memory order the dialect's frame forms thread: it has to be read exactly once on every run, and two frame-touching blocks where one is reachable from the other are two readers of one order on one path (src/compiler/native/regalloc-verify.f ORDER-CK). The entry block dominates everything and every returning run passes through the exit block, so that pair is the one that can never have the problem. Spilling inside an arm or a loop body needs the frame's order to MERGE at a join, which means a token block argument on every block with more than one predecessor and a refusal for a merge fed by a two-way branch, which carries no operands. Until it lands, a value read inside a branch arm holds its register and the shortage is refused E-A64RA-SPILL.

Scope finding (2026-08-05, spill lane measurement): NEITHER corpus-4 refusal needs this redesign — CALL-PRESSURE closes with an around-the-loop split (habu-split-call-crossed) and PRESSURE-LOOP likely with loop-invariant rematerialization (habu-remat-loop-invariant). This dot proceeds only if a real program shape demands spill placement inside a middle block after those two land; until then it has no measured consumer.

Ownership cross-reference (2026-08-05, agent=callsplit): habu-split-call-crossed-6eda1613 owns the elaborator-side call-crossing protocol change (splitting a call-surviving local around a loop at elaboration); this dot keeps only the allocator-side middle-block frame-order redesign, which that change does not touch and does not need.

THE GATE IS SATISFIED (2026-08-12): both prior roads are done or dead -
split-call-crossed landed (f0983cf2, conditional threading), remat
constants landed (9adfc89d), and remat LOADS is refused on the IR's own
facts (1faad3e1's leaf carries the refutation: both load forms declare a
memory READ answering a new order, ALDR with UNRESTRICTED aliasing, so a
re-emitted load IS the middle-block frame access KEEP? exists to refuse).
PRESSURE-LOOP - the judge's LAST refused row - demands exactly this
capability, and per the user's no-refusals ruling it is cut-blocking.
This dot is now the owner of the final corpus refusal. Design-first: the
leaf's own design question stands (the frame's memory order must MERGE at
a join - a token block argument on every multi-predecessor block and a
refusal for a two-way-branch-fed merge, which carries no operands); the
alternative road is a loop-invariant-load HOIST with a real dependence
proof (the missing fact: "the alias class this load reads is unwritten
between definition and re-emission point" - derivable from the token
chain, derived by no pass today). Probe BOTH before building either.

Claim: agent=midblock workspace=.jj-ws/habu-midblock-design

PROBED 2026-08-12 (lane midblock-design, design-first, no production code
written). Every number below is through NMIGRATE:MEASURE-HELD or
NMIGRATE:DEFINE - the production migration entry - at the 18-register budget
tools/codegen-spill-probe.f uses. Probe sources: /tmp/hb-midblock/probe{1..7}.f.

THE PRESSURE MEASUREMENT, FIRST, BECAUSE IT DECIDES THE ROADS.

  shape                                        where the 14 live   rc      slots
  PRESSURE-LOOP verbatim                       loop body           -8508   0
  thirteen loads, same body                    loop body           0       0
  the 14 loads HOISTED, adds left in the body  across the loop     -8508   -
  eight loads hoisted the same way             across the loop     0       -
  loads AND their 13 adds hoisted (full LICM)  entry block         0       0
  the same full hoist into a GUARDED arm       a middle block      0       0
  the body's add tree RE-ASSOCIATED            loop body           0       0

  Walls, pinned from both sides: 13/14 inside the loop body, 15/16 inside a
  guarded middle block, and NO wall in the entry block - 18 loads there compile
  with 1 slot, 20 with 3, 22 with 5, because entry-block values are spillable.
  That contrast is also the control proving the guarded arm really is a middle
  block: it refuses with slots 0 (nothing there is a candidate) where the entry
  block spills instead.

  SO HOISTING THE LOADS ALONE MAKES IT WORSE, MEASURED. The 14 become values
  live across the loop's edges - block arguments, a class of more than one
  value - which is neither spillable nor rematable, and the refusal is the same
  code at the same count. Only hoisting the loads AND the 13 invariant adds
  helps; the last add is the only one that touches the accumulator, so the
  invariant sub-expression is already grouped in the source's own association.

  AND THE HOIST BUYS EXACTLY TWO VALUES. Pre-header wall 15 against body wall
  13: a sixteen-field record refuses again. Re-association has no width wall
  until E-NTAPE-CAP at 28 loads.

  A FOURTH ROAD, REFUTED HERE. The 14 loads are off one base with constant
  offsets and this dialect has no base+offset load form (combine.f's header
  counts 25 pairable loads it does not model), so each is an add and a load.
  Folding the offset does NOT close this row: fourteen loads of the SAME
  address, with no address temporary at all, still refuse -8508 at 14, 15, 16
  and 17. The wall is the loaded values, not the addressing.

ROAD 1 - SPILL FROM A MIDDLE BLOCK - PRICED, AND ITS PREMISE IS STALE.

  THE BLOCK-ARGUMENT MACHINERY THIS LEAF ASKS FOR ALREADY EXISTS AND SHIPS.
  elaborate.f OPEN-ARGS-H already adds a HIR:MEM-TYPE block argument to every
  block it opens with arguments whenever TOK-LIVE is set, and the file's own
  header states the rule: "the way it reaches a block that control can arrive
  at twice is the way every other live value reaches one: as a BLOCK ARGUMENT
  ... so a loop body's load reads the order the previous turn left and the
  block after a branch reads whichever arm ran." select.f OPEN-ARG1 carries it
  into the machine module; regalloc-verify.f ORDER-CK's rule is already
  per-path and explicitly accommodates loops and two-way branches. The
  two-way-branch refusal the leaf predicts already exists by name
  (E-A64SEL-ORDER, select.f ORDER-EDGE!) and is already avoided the way the
  leaf proposes: "a real join whose arms leave different orders is written in
  the source with the order among the block's arguments".

  WHAT IS ACTUALLY MISSING IS NARROWER AND IS IN ONE FILE. spill.f threads the
  FRAME's order as a single running variable in MODULE order (spill.f:285 TOK,
  set once by EMIT-RESERVE and re-set by EMIT-STORE / EMIT-LOAD /
  BIND-RESULTS), not as a per-block SSA value entering through the block's
  token argument. Measured: cutting MB-KEEP-BLOCK's middle-block clause turns
  PRESSURE-LOOP from -8508 into -8091 E-IR-VERIFY-DOM - an operand naming a
  value whose defining block does not dominate the using block - which is that
  running variable handing a token minted in the loop body to an operation in a
  block the body does not dominate. The work is to make spill.f's frame
  threading block-structured like the program's already is: a per-block order
  in, a per-block order out, a token block argument added where the spill made
  the incoming orders disagree, and OPEN-BLOCK (which copies arguments one for
  one today) able to add one. formal/ models block arguments as ordinal windows
  (Common/Structure.v) and models the memory order not at all, so this adds no
  formal obligation - it mints no new IR primitive.

  WHAT IT WOULD BUY PRESSURE-LOOP: exactly one slot. The allocator's own plan
  under the relaxation is two rows - store value 15 at block 3 pos 1, reload
  value 15 at block 3 pos 39 - so the body would run one store and one reload
  EVERY TURN on top of its 14 loads.

  AND WHAT IT BUYS THE REAL TREE: NOTHING, MEASURED. Census over all 50 files
  in src/ + lib/ that produce a spill refusal, today's master, baseline against
  the same census with the middle-block clause cut:
      E-A64RA-SPILL   116 -> 111
      E-A64RA-POOL    103 -> 108
      pressure class  219 -> 219
  Five rows move, and they move to a code no spill placement can serve - one
  operation needing more registers at one instant than the routine may destroy.
  ZERO definitions compile that did not compile before. The capability's only
  known consumer is the corpus row that was written to demand it.

ROAD 2 - HOIST WITH A DEPENDENCE PROOF - AND THE PROOF IS SMALLER THAN NAMED.

  THE SEAT EXISTS. src/compiler/native/loop.f is the HIR closed-forming pass
  (habu-close-the-loops-1571fb6f landed as this file; both hoist leaves name it
  as their dependency). It already recognises the exact `?do` shape INCLUDING
  the pre-header block `pr`, already reasons about "a value that cannot change
  with the turn", and already has FUN-DEF - the whole-function search for a
  value's defining operation - which is the primitive a loop-invariance test
  needs. It runs in migrate.f CLOSED, on the HIR, before selection.

  THE MISSING FACT IS ANSWERABLE FROM THE SCHEMA TODAY. The HIR declares each
  opcode's memory effect (hir.f: IR--SCHEMA-EFFECT:READ / WRITE / READ-WRITE
  over GENERIC-MEM) and IR-SCHEMA:EFFECT@ / FEFFECT@ read it back. The sound
  structural precondition for hoisting a READ out of a loop is not a general
  alias analysis but "no operation in the loop declares a WRITE, and there is
  no call and no trap" - one scan of the loop's three blocks against the schema
  the module already carries. PRESSURE-LOOP's body is fourteen READs and
  nothing else. loop.f already declines any body that touches memory for
  exactly this reason ("the closed form of its ARITHMETIC would be right and
  its memory would be gone"), so the hoist is what turns a declined body into
  one it folds.

  AND THE COMPOSITION IS THE WHOLE PRIZE. After the hoist PRESSURE-LOOP's body
  holds one addition into one accumulator whose other operand cannot change
  with the turn - loop.f's recognised shape exactly - so the row closes as a
  CLOSED FORM, which is what the C twin does. tools/clang/twins.c says so in
  its own header: "clang hoists all fourteen loads out of the loop, vectorises
  the sum and multiplies by the trip count, so the reference never holds
  fourteen values and never spills." Baseline for the row: engine 1096 bytes /
  199.61 ns, chain refused, clang 72 bytes / 0.06 ns.

ROAD 3, FOUND WHILE PROBING AND KEPT AS THE FALLBACK. Re-associating the
  reduction closes the row on its own (rc 0), touches no memory reasoning, moves
  no load, and has no width wall - the association the source writes is
  right-leaning, and left-leaning is exact for wrapping integer addition. It is
  strictly weaker as an ANSWER (the body still runs 14 loads a turn) and it
  would need its own guard against floats, but it is the shape that closes a
  loop whose loads a write really does alias.

RECOMMENDATION, FOR THE ORCHESTRATOR'S RULING. Do not build this dot. Give
PRESSURE-LOOP to habu-hoist-loop-invariant-ac9aca87 - a loop-invariant hoist in
loop.f, gated on the schema's own "no WRITE in the loop", feeding the
closed-forming recogniser already in that file. This leaf's capability has zero
measured consumers in src/ + lib/, buys its one corpus row a per-turn store and
reload where the hoist removes the loads entirely, and its stated blocker
(a token block argument on multi-predecessor blocks) was built elsewhere and
already ships. If the ruling keeps the leaf alive, its description should be
rewritten to what is actually missing: spill.f's frame order is a running
variable in module order and must become a per-block value.
