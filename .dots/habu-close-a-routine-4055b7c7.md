---
title: "Close a routine's caller-save against the callee's clobber set"
status: active
priority: 2
issue-type: task
created-at: "2026-08-02T10:38:37.613933+02:00"
---

A call site in src/compiler/native/select.f writes EVERY value the caller still holds into its own data stack and reads all of them back, whatever the callee destroys. That is what makes the discipline correct against a callee this compiler did not produce - an engine-compiled word, or one from a later back end - and it is why chain-calls-old works today. It is also maximally conservative: a callee whose contract destroys only part of the register pool leaves the rest live, and a value in one of those registers need not cross the stack at all. The callee's contract is already the thing the arity comes from once habu-resolve-a-callee-0340dfde lands, so the clobber set can come from the same place. Fix: give the source dialect's wordcall operation the callee's destroyed set, have the elaborator consume and re-answer only the values that set can reach, and leave the rest as ordinary SSA values crossing the operation. Measure it on the corpus before believing it: a call site that saves nothing is not obviously faster if the register pressure it creates spills instead. Blocked on habu-resolve-a-callee-0340dfde.


NOTE 2026-08-09: the blocker this leaf names, habu-resolve-a-callee-0340dfde,
no longer exists (closed and merged - name resolution landed). This dot is
unblocked; re-derive its premise on the current tree before claiming.

PRICED AS THE PRE-CUT POOL CLOSER (census 2026-08-12): 140 first-refusals
E-A64RA-POOL - zero/low-arity routines whose calls put every live value
across the data stack under caller-save-everything. The register
convention (da01bd62) also closes the class but is sequenced POST-CUT by
the plan, so THIS dot is the pre-cut road; its first deliverable is the
pricing - how many of the 140 does caller-save narrowing close, measured
against /tmp/hb-census-scout/refusals.tsv's POOL rows (or a re-census).
If the answer is materially short of 140, the cut plan has a sequencing
decision to make and the orchestrator needs the number, not a fix.

Claim: agent=poolclose workspace=.jj-ws/habu-pool-close

PRICED 2026-08-13 (poolclose, master 3cdb1188). THE ANSWER IS ZERO, and the
premise this leaf was written on is wrong about what the class is. It is also
the pricing this leaf and habu-price-the-pool-8a692e8d both asked for; neither
needs a second measurement.

THE RE-CENSUS. tools/chain-census-core.f over src and lib at the eighteen
register budget: 3944 definitions examined, 2946 compiled, 998 refused.
E-A64RA-POOL is 149 of them and is the largest refusal the chain itself makes -
the two bigger lines are the census's own (RC-UNDEFINED 258 self-check,
E-NMIGRATE-TEXT 151 recorder ceiling). E-A64RA-SPILL is 116.

WHAT THE REFUSAL IS, ATTRIBUTED BY MUTATION RATHER THAN BY READING. Changing
the E-A64RA-POOL throw in regalloc.f TAKE moves no row; changing the one in
MB-VICTIM moves every row, so the class is MB-VICTIM's `MB-SPARE-N 0=` branch.
An instrumented build reports the failing position as free=18, spare=0,
forbid=$3FFFF: every register of the pool is FREE and every one of them is
BARRED. Nothing holds a register, so no spill and no re-emission can free one -
the value has nowhere it is ALLOWED to be. The bar is MB-FORBID, which is the
callee's clobber record, and an address with no row answers the whole pool.

THE NARROWING THIS LEAF ASKS FOR IS ALREADY IN THE TREE, in all three places.
regalloc.f CALL-BITS reads NCLOB:GPR-CLOB, regalloc-verify.f VCALL-BITS
re-derives the same bar, and elaborate.f CALL-KEEPS? asks NCLOB:KNOWN? before it
makes a local travel. tools/codegen-compare-test.f already pins the pair:
CALL-PRESSURE-N against the chain's published callee compiles, CALL-PRESSURE-E-N
against the engine's compilation of the same callee is refused. What this leaf
proposed on top - narrowing the wordcall OPERAND list so fewer values cross the
data stack - is not what the class turns on: the ceiling measurement below
relaxes only the forbid mask, leaves CALL-OPERANDS+ untouched, and the whole
class closes.

THE CEILING IS 149 OF 149 AND IT IS UNREACHABLE. Mutating CALL-BITS and
VCALL-BITS together to answer "this callee destroys nothing" and re-censusing
the same tree: 3944 examined, 3210 compiled, 734 refused, E-A64RA-POOL 0 and
E-A64RA-SPILL 1. Row by row, all 148 distinct POOL rows and all 115 distinct
SPILL rows compile and nothing that compiled before is refused (148 and 115
rather than 149 and 116 because two rows of each share a file-and-name key).
A three-register
mask instead of an empty one - the width a real chain-published routine records,
measured at 3 of 18 on CODEGEN-CORPUS4:C-LONG-N - gives the identical numbers.
So the mechanism is worth 264 definitions IF the callee has a row.

AND THE CALLEE CANNOT HAVE ONE, BECAUSE IT IS A NAMED CONSTANT. Measured on
lib/memory.f MEM-64K-COUNT-FOR, one of the 149, through NMIGRATE with no
mutation anywhere. The one call its emission makes is not to either word it
names: it is to `MEM-64K`, and elaborate.f RESOLVE-STEP resolves every name the
dialect does not model as a CALLABLE, so a reference to a `constant` or a
`create`d word compiles to a branch to that word's engine code.
HIR-WORD:DECLARE-FIXED - the path that would fold it to its value - is reached
only from migrate.f DECLARE-DATA, for the ONE data word a caller stages through
NMIGRATE:DEFINE-DATA, and the census stages none. Four verdicts, same body:

  against its real engine callees                                 -8446
  against chain-published copies of both callees (records known)   -8446
  against leaf callees whose records say 0 registers destroyed     -8446
  against its real engine callees, `MEM-64K` spelled `$10000`      0

The same pair in isolation: `bytes CHK-A bytes 1 - K7 / 1 + dup CHK-B` where
`7 constant K7` is refused -8446 and the character-identical body with `7`
written as a digit compiles. Value, shape, callees and register budget held
still; the only difference is whether a name or a number named it.

HOW MUCH OF THE 149 IS THAT SHAPE IS ESTIMATED AND NOT MEASURED. A text scan of
the 149 refused bodies against the 6560 names src and lib define with
`constant`, `variable`, `create`, `2constant` or `value` puts 131 of them on a
body that mentions at least one. The other 18 - lib/ptx/cg-activation.f
EMIT-MULC, lib/process.f PROC-RUN-RC and their like - cross an ordinary word
call instead, and those are the rows a clobber record really could close. That
is a scan of text and not of what the chain compiled; the honest measurement is
a diagnostic that names the crossed callee at the refusal, which this lane did
not build.

TWO MORE FACTS THE MEASUREMENTS TURNED UP. A census leaves NCLOB:ROWS at 0
before and after a real run, so every callee it measures against answers the
worst case by construction - the instrument cannot price a record-conditioned
mechanism at all. And a chain-published callee that can `throw` records 24
destroyed general registers, WIDER than the 18 the no-record path answers, so
publishing such a callee makes its caller's narrowing worse than not knowing.

WHAT SHOULD HAPPEN. Nothing in this leaf: the narrowing is built, and no work on
the caller-save discipline reaches the rows measured here. Most of the class
waits on a capability that does not exist yet - fold a named `constant` or
`create`d word to its value instead of calling it, for every such word a body
names rather than for the one staged through DEFINE-DATA. That needs a way to
tell a constant from an ordinary zero-input word without running it, which is a
design decision and not a mutation, so it is not taken here. Until it lands the
POOL and SPILL buckets are not a caller-save measurement.
