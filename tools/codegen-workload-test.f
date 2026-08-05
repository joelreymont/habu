\ codegen-workload-test.f - the scheduled half of the end-to-end workload
\ measurement. Run: bin/hb --load tools/codegen-workload-test.f
\
\ NOT ONE ASSERTION HERE READS A CLOCK, and that is what makes it safe to
\ schedule. The workload report's numbers are timings, and a timing fails for
\ host load; the standing rule is that the scheduled suites hold facts about
\ compiled code and the hand-run report holds the measurements. So this file
\ never calls CODEGEN-RUN:MEASURE. It does measure a small family of rows of its
\ own, through the same store every reported row goes through - but it reads
\ their ANSWERS, their kinds and the shape of their runs, never their times,
\ because what needs pinning there is which column belongs to which arm and not
\ how long either took. What it checks is everything the report's numbers would
\ be MEANINGLESS without:
\
\   the inline rule           the predicate that says whether the engine copies
\                             a body is checked against the engine's own
\                             behaviour, on fixtures that bracket every reason
\                             the engine has to refuse: a straight-line body
\                             exactly at the size limit, a straight-line body
\                             exactly one instruction over it, and one body per
\                             refusal class whose ONLY reason to be refused is
\                             that class.
\   the call count            checked on the same fixtures, in both directions:
\                             a copied body has no call site anywhere, a called
\                             one has the site its caller was compiled with.
\   the wiring                every arm's driver enters ITS OWN column's word and
\                             NOT the other column's. This is the mutation the
\                             whole measurement turns on: an after-arm still
\                             calling the before-arm's record would run old code
\                             under a new name, report a delta of nothing, and
\                             look perfectly healthy.
\   the reach                 the checker's own fold was migrated between the two
\                             arms and every call instruction that entered its
\                             old code was MOVED onto the chain's routine, so the
\                             compile-shaped row's arms really are separated by a
\                             migration the checker itself runs. Counted rather
\                             than asserted in prose: the number of sites moved
\                             is the number the scan finds entering the new
\                             record, and the old record has none left.
\   the answers               the two arms of every workload compute the same
\                             value, and that value is pinned. Two arms that
\                             disagree ran different programs.
\   the bodies                the two arms of a workload are compiled from ONE
\                             string, so their machine code has to be the same
\                             size when the subjects behind it are: a name lives
\                             in a dictionary record and not in a body.
\   the verdict               a row's delta keeps its sign, so a loss is reported
\                             as a loss; a pair row's two columns hold the arms
\                             they were given rather than each other; and a
\                             sweep row's columns are its extremes, the old one
\                             never the greater. All three without a clock.
\
\ THE FIXTURES ARE BUILT TO FOOL THE SCAN, not merely to agree with it. The
\ text-versus-structure trap is the one that matters here: a driver whose SOURCE
\ names two words and whose compiled code contains no call at all, because the
\ engine copied both bodies in. A check that searched the source for a call would
\ pass that; the check here reads the emitted instructions.
\
\ AND EVERY FIXTURE IS MEASURED, NEVER DESCRIBED. A comment saying a body is one
\ instruction over the limit is worth nothing - the first version of this file
\ said exactly that about a body thirteen times the limit, and about a "small"
\ branchy body that was already too big to copy, so neither fixture isolated
\ what its comment claimed. Each fixture below states its record length as a
\ CASE, against tools/codegen-workload-scan.f's copy of the engine's own limit,
\ so a fixture that drifts off its boundary fails here instead of quietly
\ testing something else.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require tools/codegen-workload-scan.f
require tools/codegen-workload-run.f

package CODEGEN-WORKLOAD-TEST

private

\ ---- fixtures for the inline rule -------------------------------------------
\ The engine copies a body of at most INL-MAX bytes in which every instruction
\ survives being moved. Two halves, and a fixture for each side of each half.
\
\ Each is published by the same route the workload's subjects are, and the
\ CALLER of each is compiled afterwards - which is the only moment the engine's
\ decision is made and recorded.

: SMALL$ ( -- ptr u8 n )
   s" : FX-SMALL ( n -- n ) 7 and ;" ;

\ The size boundary from above, by one instruction. `7 and` compiles to exactly
\ INL-MAX bytes of body; `negate invert` compiles to exactly one instruction
\ more, and contains nothing the engine would refuse to move. So its refusal has
\ one possible cause and the pair pins the limit from both sides: shrink INL-MAX
\ and FX-SMALL stops being copied, grow it and FX-OVER starts being.
: OVER$ ( -- ptr u8 n )
   s" : FX-OVER ( n -- n ) negate invert ;" ;

\ Far past the limit, and the reason a size rule exists at all: a dozen `1 +`
\ takes the body to thirteen times what the engine will move.
: BIG$ ( -- ptr u8 n )
   s" : FX-BIG ( n -- n ) 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 7 and ;" ;

\ Small, branchy, and refused for BOTH reasons: an `if` compiles to more than
\ INL-MAX bytes of body on its own, so this fixture says only that a body can be
\ refused twice over. It isolates nothing, and the per-class fixtures below are
\ what isolate the branch clauses.
: BRANCHY$ ( -- ptr u8 n )
   s" : FX-BRANCH ( n -- n ) dup 7 > if drop 0 then ;" ;

\ ---- one fixture per refusal class ------------------------------------------
\ A body UNDER the size limit whose only unmovable instruction is one of the
\ class named, so the engine's refusal - and this file's - has exactly one
\ possible cause. Delete that clause from either side and the fixture's pair of
\ cases disagree.
\
\ Two of the five are compiled here. The other three are already in the
\ dictionary: `abs`, `/` and `execute` are engine words whose bodies are under
\ the limit and carry exactly one unmovable instruction each, and what is
\ compiled here is a CALLER for each, because the engine's decision about a
\ callee is only recorded when a caller is compiled against it.

\ A body of one instruction: the call to `abs`, which the engine will not copy.
\ Nothing else fits in four bytes, so only the `bl` clause can refuse it - and
\ this same word is the caller that records the engine's decision about `abs`.
: BL$ ( -- ptr u8 n )
   s" : FX-BL ( n -- n ) abs ;" ;

\ `exit` before the end of a body compiles to an unconditional branch to the
\ epilogue and to nothing else, so this body is `dup` plus one `b`.
: B$ ( -- ptr u8 n )
   s" : FX-B ( n -- n n ) dup exit ;" ;

\ The callers for the three engine words. Each names its subject once and
\ nothing else that the engine would refuse to copy, so the call instruction
\ these bodies do or do not contain is the engine's answer about that subject.
: DIV$ ( -- ptr u8 n )
   s" : FX-DIV ( n n -- n ) / ;" ;

: EXEC$ ( -- ptr u8 n )
   s" : FX-EXEC ( -- ) [: ;] execute ;" ;

\ ---- the return slot, which is the rule's other half ------------------------
\ A record with no prologue is copied whole, and only if the word just past it
\ is a `ret`. A definition whose return slot was patched has something else
\ there, and the engine will not move its body however short and however
\ straight-line it is.
\
\ These two fixtures are that rule and nothing else. `variable` and
\ PTR-VARIABLE compile to the same twenty-four bytes of the same six
\ instructions - four that build the variable's address and two that push it,
\ differing only in the address each carries - and the only difference that
\ reaches C-CALL is what sits in the return slot. One is copied and one is
\ called.
: VAR$ ( -- ptr u8 n )
   s" variable FX-VAR" ;

: PVAR$ ( -- ptr u8 n )
   s" PTR-VARIABLE FX-PVAR" ;

: USES-VARS$ ( -- ptr u8 n )
   s" : FX-USES-VARS ( -- n ) FX-PVAR @ drop FX-VAR @ ;" ;

\ ---- the size limit for a record with no frame ------------------------------
\ The engine measures a record with a prologue against INL-MAX plus the frame it
\ carries, and a record without one against INL-MAX bare. Those are two separate
\ comparisons in C-CALL and they need two separate pairs of fixtures. `over` is
\ an engine word of exactly INL-MAX bytes with nothing unmovable in it; `emit` is
\ the same shape one instruction longer. What is compiled here is a caller for
\ each.
: USES-OVER$ ( -- ptr u8 n )
   s" : FX-USES-OVER ( n n -- n n n ) over ;" ;

: USES-EMIT$ ( -- ptr u8 n )
   s" : FX-USES-EMIT ( n -- ) emit ;" ;

public

: PUBLISH-FIXTURES ( -- )
   SMALL$ CODEGEN-HOT:EV
   OVER$ CODEGEN-HOT:EV
   BIG$ CODEGEN-HOT:EV
   BRANCHY$ CODEGEN-HOT:EV
   BL$ CODEGEN-HOT:EV
   B$ CODEGEN-HOT:EV
   DIV$ CODEGEN-HOT:EV
   EXEC$ CODEGEN-HOT:EV
   VAR$ CODEGEN-HOT:EV
   PVAR$ CODEGEN-HOT:EV
   USES-VARS$ CODEGEN-HOT:EV
   USES-OVER$ CODEGEN-HOT:EV
   USES-EMIT$ CODEGEN-HOT:EV ;

;package

\ The fixtures, and then the callers that name them. A caller has to be compiled
\ AFTER its callee: the engine decides call-or-copy while it compiles a caller,
\ so a caller compiled first would call everything and the fixtures would prove
\ nothing.
package FIXTURE
public
CODEGEN-WORKLOAD-TEST:PUBLISH-FIXTURES
: FX-CALLER ( n -- n )
   FX-SMALL FX-BIG FX-BRANCH ;
: FX-CALLER2 ( n -- n )
   FX-OVER FX-BL FX-B drop ;
;package

package CODEGEN-WORKLOAD-TEST

private

using CODEGEN-SCAN

\ ---- the two sides of every case below --------------------------------------
\ ENGINE-COPIES? is this repository's copy of the engine's rule. CALLS? is not a
\ rule at all: it reads the caller's machine code, where the engine wrote its own
\ decision about the callee at the moment it compiled that caller. The two have
\ to be opposite answers, and a clause that went missing from either side leaves
\ them agreeing.

: CALLED-NOT-COPIED ( ptr u8 n ptr u8 n -- ) {: ca:ptr cu:n ta:ptr tu:n :}
   ta tu ENGINE-COPIES? TFALSE
   ca cu ta tu CALLS? TTRUE ;

: COPIED-NOT-CALLED ( ptr u8 n ptr u8 n -- ) {: ca:ptr cu:n ta:ptr tu:n :}
   ta tu ENGINE-COPIES? TTRUE
   ca cu ta tu CALLS? TFALSE
   ta tu CALL-SITES 0 T= ;

\ A fixture's record length, measured and stated against the engine's own limit
\ for the shape of record it is. A compiled word opens with the prologue and the
\ size test allows it a frame; an engine word that does not is measured against
\ the bare limit. Which of the two a record is, is read off its first
\ instruction rather than assumed.
: FITS-FRAMED ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u WORD-BYTES want T=
   a u 0 WORD-INSN-AT C-CALL-PROLOGUE-INSTR T=
   want INL-MAX FRAME-BYTES + <= TTRUE ;

\ The same measurement for the fixture on the far side of the limit: a compiled
\ record whose length is stated exactly and is over what the engine will move.
: OVER-FRAMED ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u WORD-BYTES want T=
   a u 0 WORD-INSN-AT C-CALL-PROLOGUE-INSTR T=
   want INL-MAX FRAME-BYTES + > TTRUE ;

\ The same two measurements for a subject this file did not compile.
\
\ Only the engine's own primitive emitter produces a record with no prologue:
\ every colon definition gets one (src/habu/habu2.f emits `sub sp, sp, #16` at
\ entry), so there is no way for a test to publish a frameless word of a size it
\ chose. The bare half of the rule can therefore only be exercised against engine
\ words - and a fixture that also states their byte count is stating something it
\ does not own. `emit` was pinned at INL-MAX + one instruction and grew to 52,
\ which is the whole of why this suite was red.
\
\ So these MEASURE the subject and assert the rule against the measurement. What
\ is claimed is what the engine's bare comparison must do at whatever size the
\ word actually has; a word that changes size keeps its case, and only a word
\ that CROSSES the limit changes which case it belongs in - at which point the
\ case says so by name instead of reporting a number nobody can read.
: BARE-FITS ( ptr u8 n -- ) {: a:ptr u:n :}
   a u 0 WORD-INSN-AT C-CALL-PROLOGUE-INSTR T<>
   a u WORD-BYTES INL-MAX <= TTRUE ;

: BARE-OVER ( ptr u8 n -- ) {: a:ptr u:n :}
   a u 0 WORD-INSN-AT C-CALL-PROLOGUE-INSTR T<>
   a u WORD-BYTES INL-MAX > TTRUE ;

: FRAMED-FITS ( ptr u8 n -- ) {: a:ptr u:n :}
   a u 0 WORD-INSN-AT C-CALL-PROLOGUE-INSTR T=
   a u WORD-BYTES INL-MAX FRAME-BYTES + <= TTRUE ;

\ For a subject this file did not publish, whose other callers it does not own:
\ the engine copied it into THIS caller, which is the whole claim. A count of
\ every call site in the dictionary is a claim about words that are none of this
\ file's business.
: COPIED-NOT-CALLED-BY ( ptr u8 n ptr u8 n -- ) {: ca:ptr cu:n ta:ptr tu:n :}
   ta tu ENGINE-COPIES? TTRUE
   ca cu ta tu CALLS? TFALSE ;

\ The last instruction the walk over a record reaches, and an index one past it.
: LAST-INSN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u  a u WORD-INSNS 1-  WORD-INSN-AT ;

: PAST-END ( -- )
   s" FIXTURE:FX-SMALL" {: a:ptr u:n :}
   a u  a u WORD-INSNS  WORD-INSN-AT drop ;

\ ---- the size limit, from both sides ----------------------------------------
\ Two straight-line bodies one instruction apart across INL-MAX, each with
\ nothing in it the engine would refuse to move - which is stated, not assumed,
\ because "refused on size" only means anything while nothing else could have
\ refused it. Move the limit in either direction and one of these two fails.
: SIZE-CASES ( -- )
   s" a straight-line body exactly at the size limit is copied" T-LABEL
   s" FIXTURE:FX-SMALL" INL-MAX FRAME-BYTES + FITS-FRAMED
   s" FIXTURE:FX-SMALL" UNMOVABLE-IN 0 T=
   s" FIXTURE:FX-SMALL" COPY-BYTES INL-MAX T=
   s" FIXTURE:FX-CALLER" s" FIXTURE:FX-SMALL" COPIED-NOT-CALLED

   s" and one instruction more is a call, on size and nothing else" T-LABEL
   s" FIXTURE:FX-OVER"
      INL-MAX FRAME-BYTES + INSN-BYTES + OVER-FRAMED
   s" FIXTURE:FX-OVER" UNMOVABLE-IN 0 T=
   s" FIXTURE:FX-OVER" COPY-BYTES 0 T=
   s" FIXTURE:FX-CALLER2" s" FIXTURE:FX-OVER" CALLED-NOT-COPIED

   s" a record with no frame is measured against the bare limit" T-LABEL
   s" over" BARE-FITS
   s" over" UNMOVABLE-IN 0 T=
   s" over" COPY-BYTES  s" over" WORD-BYTES T=
   s" FIXTURE:FX-USES-OVER" s" over" COPIED-NOT-CALLED-BY

   s" and a bare record past that limit is called, on size alone" T-LABEL
   s" emit" BARE-OVER
   s" emit" UNMOVABLE-IN 0 T=
   s" FIXTURE:FX-USES-EMIT" s" emit" CALLED-NOT-COPIED ;

\ ---- the return slot --------------------------------------------------------
\ The other half of the rule for a record with no frame: the engine copies it
\ only when the word just past it is a `ret`, so a definition whose return slot
\ was patched is never moved. The pair below is that clause with nothing else in
\ it. Both fixtures are six instructions of the same shape - four that build the
\ variable's address and two that push it, differing only in which address they
\ carry - the same number of bytes, and neither holds anything the engine refuses
\ to move. One has a `ret` in its return slot and is copied; the other does not
\ and is called.
: SLOT-CASES ( -- )
   s" the two fixtures are the same code, and both are small enough" T-LABEL
   s" FIXTURE:FX-VAR" WORD-BYTES  s" FIXTURE:FX-PVAR" WORD-BYTES  T=
   s" FIXTURE:FX-VAR" WORD-BYTES INL-MAX <= TTRUE
   s" FIXTURE:FX-VAR" UNMOVABLE-IN 0 T=
   s" FIXTURE:FX-PVAR" UNMOVABLE-IN 0 T=
   s" FIXTURE:FX-VAR" WORD-INSNS  s" FIXTURE:FX-PVAR" WORD-INSNS  T=
   s" FIXTURE:FX-VAR" LAST-INSN  s" FIXTURE:FX-PVAR" LAST-INSN  T=

   s" the one with a ret in its return slot is copied" T-LABEL
   s" FIXTURE:FX-USES-VARS" s" FIXTURE:FX-VAR" COPIED-NOT-CALLED

   s" and the one whose return slot was patched is called" T-LABEL
   s" FIXTURE:FX-USES-VARS" s" FIXTURE:FX-PVAR" CALLED-NOT-COPIED
   s" FIXTURE:FX-PVAR" CALL-SITES 1 T= ;

\ ---- one refusal clause at a time -------------------------------------------
\ Every fixture here is under the size limit and holds exactly ONE instruction
\ the rule refuses to move, so its refusal has exactly one possible cause. Delete
\ that cause from this repository's copy and the fixture is judged copyable while
\ the engine's own call instruction is still sitting in its caller; delete it
\ from the engine and the call instruction goes away while the copy still refuses
\ it. Either way the fixture's two cases stop being opposite answers.
\
\ FOUR OF THE NINE CLAUSES HAVE NO FIXTURE HERE, AND CANNOT GET ONE. C-CALL
\ refuses `tbz`/`tbnz`, `br`, a `ret` inside a body, and `adr`; nothing in this
\ system emits any of the four into a body that is otherwise movable:
\
\   tbz   src/arch/arm64/asm.f has no encoder for it at all. The only live
\         records that match its mask match it in inline STRING DATA read as an
\         instruction, inside records far past the size limit.
\   br    emitted by hand-written engine assembly only (`evaluate`, `throw`),
\         in records of 228 and 284 bytes carrying several other refused
\         instructions each.
\   ret   a `ret` lands inside a body only where a quotation's body is compiled
\         inline, which also emits the `b` that jumps over it - and the smallest
\         such body is 52 bytes, past the limit.
\   adr   emitted for an inline string literal, which also emits the `b` that
\         jumps over the literal's bytes.
\
\ A body carrying two refused instructions isolates neither: delete one clause
\ and the other still refuses it. So there is no fixture, rather than a fixture
\ that proves nothing, and dot habu-reach-the-four-e5ca60a5 records the missing
\ capability. A sweep of every live record is what establishes this, and it is
\ repeatable: no record in the dictionary has a candidate body whose only refused
\ instruction is one of the four.
: CLAUSE-CASES ( -- )
   s" a call is the one thing this body holds, and it is called" T-LABEL
   s" FIXTURE:FX-BL" FRAME-BYTES INSN-BYTES + FITS-FRAMED
   s" FIXTURE:FX-BL" UNMOVABLE-IN 1 T=
   s" FIXTURE:FX-BL" BLS-IN 1 T=
   s" FIXTURE:FX-CALLER2" s" FIXTURE:FX-BL" CALLED-NOT-COPIED

   s" an early exit leaves one branch behind, and that is enough" T-LABEL
   s" FIXTURE:FX-B" 44 FITS-FRAMED         \ 28 bytes of body inside its frame
   s" FIXTURE:FX-B" UNMOVABLE-IN 1 T=
   s" FIXTURE:FX-B" BLS-IN 0 T=
   s" FIXTURE:FX-CALLER2" s" FIXTURE:FX-B" CALLED-NOT-COPIED

   \ These three are engine words, so the size line says only what this file has
   \ to establish for the case to mean anything - that the subject FITS, and so
   \ its refusal is the branch it holds and not its length. Their byte counts
   \ belong to the engine, not to this suite.
   s" the engine's own abs holds one conditional branch and is called" T-LABEL
   s" abs" BARE-FITS
   s" abs" UNMOVABLE-IN 1 T=
   s" FIXTURE:FX-BL" s" abs" CALLED-NOT-COPIED

   s" its divide holds one compare-and-branch and is called" T-LABEL
   s" /" BARE-FITS
   s" /" UNMOVABLE-IN 1 T=
   s" FIXTURE:FX-DIV" s" /" CALLED-NOT-COPIED

   s" and execute holds one register branch and is called" T-LABEL
   s" execute" FRAMED-FITS
   s" execute" UNMOVABLE-IN 1 T=
   s" FIXTURE:FX-EXEC" s" execute" CALLED-NOT-COPIED ;

\ ---- the walk over a record, end to end -------------------------------------
\ Every count this file reports comes out of one walk over a record's
\ instructions, so what that walk covers is a fact worth stating. A walk that
\ began one instruction late or ended one early would report a call count that is
\ right for most records and quietly wrong for the ones with a call at an end.

: SPAN-CASES ( -- )
   s" the walk sees as many instructions as the record has bytes" T-LABEL
   s" FIXTURE:FX-SMALL" WORD-INSNS
      s" FIXTURE:FX-SMALL" WORD-BYTES INSN-BYTES / T=
   s" FIXTURE:FX-CALLER" WORD-INSNS
      s" FIXTURE:FX-CALLER" WORD-BYTES INSN-BYTES / T=
   s" FIXTURE:FX-BL" WORD-INSNS
      s" FIXTURE:FX-BL" WORD-BYTES INSN-BYTES / T=

   s" and the instructions at its two ends are the frame's own" T-LABEL
   s" FIXTURE:FX-SMALL" 0 WORD-INSN-AT C-CALL-PROLOGUE-INSTR T=
   s" FIXTURE:FX-SMALL" LAST-INSN C-CALL-FRAME-DOWN-INSTR T=
   s" FIXTURE:FX-CALLER" 0 WORD-INSN-AT C-CALL-PROLOGUE-INSTR T=
   s" FIXTURE:FX-CALLER" LAST-INSN C-CALL-FRAME-DOWN-INSTR T=

   s" an index the walk never reached is refused, not answered" T-LABEL
   [: PAST-END ;] E-WLSCAN-INSN TTHROWSQ

   s" and the call counts over those records are the calls they make" T-LABEL
   s" FIXTURE:FX-CALLER2" BLS-IN 3 T=
   s" FIXTURE:FX-B" BLS-IN 0 T=
   s" FIXTURE:FX-DIV" BLS-IN 1 T=
   s" FIXTURE:FX-EXEC" BLS-IN 1 T= ;

\ ---- the engine's rule, against the engine ----------------------------------
: RULE-CASES ( -- )
   s" a small straight-line body is one the engine copies into its caller" T-LABEL
   s" FIXTURE:FX-SMALL" ENGINE-COPIES? TTRUE
   s" FIXTURE:FX-SMALL" COPY-BYTES 0 > TTRUE

   s" the same shape past the size limit is a call" T-LABEL
   s" FIXTURE:FX-BIG" ENGINE-COPIES? TFALSE
   s" FIXTURE:FX-BIG" COPY-BYTES 0 T=

   s" a branchy body is refused twice over, on size and on the branch" T-LABEL
   s" FIXTURE:FX-BRANCH" ENGINE-COPIES? TFALSE
   s" FIXTURE:FX-BRANCH" WORD-BYTES INL-MAX FRAME-BYTES + > TTRUE
   s" FIXTURE:FX-BRANCH" UNMOVABLE-IN 0 > TTRUE
   s" FIXTURE:FX-BRANCH" WORD-BYTES  s" FIXTURE:FX-BIG" WORD-BYTES  < TTRUE

   s" a copied body has no call instruction anywhere pointing at it" T-LABEL
   s" FIXTURE:FX-SMALL" CALL-SITES 0 T=
   s" FIXTURE:FX-SMALL" CALLERS-OF 0 T=

   s" and each called one has the site its caller was compiled with" T-LABEL
   s" FIXTURE:FX-CALLER" s" FIXTURE:FX-BIG" CALLS? TTRUE
   s" FIXTURE:FX-CALLER" s" FIXTURE:FX-BRANCH" CALLS? TTRUE
   s" FIXTURE:FX-CALLER" s" FIXTURE:FX-SMALL" CALLS? TFALSE

   s" so the caller's own code holds exactly the two calls it makes" T-LABEL
   s" FIXTURE:FX-CALLER" BLS-IN 2 T= ;

\ ---- the surveyed hot words of the live engine ------------------------------
\ These are facts about the engine bin/hb is running. Two of them decide what a
\ migration of a checker word can reach at all: a word the engine COPIES has its
\ body pasted into every caller and leaves no call instruction to move, and a
\ word it CALLS is called from code compiled into this binary and never
\ recompiled - which is what src/compiler/native/reach.f moves.
: SURVEY-CASES ( -- )
   s" the checker's two smallest hot words are copied, never called" T-LABEL
   s" TAG" ENGINE-COPIES? TTRUE
   s" PAY" ENGINE-COPIES? TTRUE
   s" TAG" CALL-SITES 0 T=
   s" PAY" CALL-SITES 0 T=

   s" the fold the checker runs per byte is too big to copy, so it is called" T-LABEL
   s" SYM-FOLD-C" ENGINE-COPIES? TFALSE

   s" and every one of those call sites was moved onto the chain's routine" T-LABEL
   CODEGEN-HOT:REACHED 0 > TTRUE
   s" HOT-REACH:SYM-FOLD-C" CALL-SITES CODEGEN-HOT:REACHED T=
   s" SYM-FOLD-C" CALL-SITES 0 T=

   s" so the checker's callers enter the chain's fold and nothing else" T-LABEL
   s" SYM-STR=CI" s" HOT-REACH:SYM-FOLD-C" CALLS? TTRUE
   s" SYM-STR=CI" s" SYM-FOLD-C" CALLS? TFALSE

   s" and the type-variable walk is called too, because it is a loop" T-LABEL
   s" T-RES-WALK" ENGINE-COPIES? TFALSE
   s" T-RES-WALK" CALL-SITES 0 > TTRUE ;

\ ---- the subjects -----------------------------------------------------------
: SUBJECT-CASES ( -- )
   s" the chain compiled every subject smaller than the engine did" T-LABEL
   s" HOT-CHAIN:FOLD-C" WORD-BYTES    s" HOT-ENGINE:FOLD-C" WORD-BYTES    < TTRUE
   s" HOT-CHAIN:COUNT-CH" WORD-BYTES  s" HOT-ENGINE:COUNT-CH" WORD-BYTES  < TTRUE
   s" HOT-CHAIN:TERM-TAG" WORD-BYTES  s" HOT-ENGINE:TERM-TAG" WORD-BYTES  < TTRUE
   s" HOT-CHAIN:TERM-PAY" WORD-BYTES  s" HOT-ENGINE:TERM-PAY" WORD-BYTES  < TTRUE

   s" the loop subject is a call in both columns" T-LABEL
   s" HOT-ENGINE:COUNT-CH" ENGINE-COPIES? TFALSE
   s" HOT-CHAIN:COUNT-CH" ENGINE-COPIES? TFALSE

   s" while the fold is a call in the engine's column and a COPY in the chain's"
   T-LABEL
   s" HOT-ENGINE:FOLD-C" ENGINE-COPIES? TFALSE
   s" HOT-CHAIN:FOLD-C" ENGINE-COPIES? TTRUE
   s" HOT-CHAIN:FOLD-C" WORD-BYTES INL-MAX <= TTRUE
   s" HOT-ENGINE:FOLD-C" WORD-BYTES INL-MAX > TTRUE

   s" and the two the engine inlines are copied in both columns" T-LABEL
   s" HOT-ENGINE:TERM-TAG" ENGINE-COPIES? TTRUE
   s" HOT-CHAIN:TERM-TAG" ENGINE-COPIES? TTRUE
   s" HOT-ENGINE:TERM-PAY" ENGINE-COPIES? TTRUE
   s" HOT-CHAIN:TERM-PAY" ENGINE-COPIES? TTRUE

   s" the control's subjects are the engine's code, byte for byte" T-LABEL
   s" HOT-FIXED:FOLD-C" WORD-BYTES   s" HOT-ENGINE:FOLD-C" WORD-BYTES   T=
   s" HOT-FIXED:COUNT-CH" WORD-BYTES s" HOT-ENGINE:COUNT-CH" WORD-BYTES T=
   s" HOT-FIXED:TERM-TAG" WORD-BYTES s" HOT-ENGINE:TERM-TAG" WORD-BYTES T= ;

\ ---- the wiring, in both directions -----------------------------------------
\ The mutation this whole measurement turns on. An arm that entered the other
\ column's record would time one code generator twice.
: WIRING-CASES ( -- )
   s" each calling arm enters its own column's word" T-LABEL
   s" WORKLOAD:SCAN-OLD" s" HOT-ENGINE:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:COUNT-OLD" s" HOT-ENGINE:COUNT-CH" CALLS? TTRUE
   s" WORKLOAD:COUNT-NEW" s" HOT-CHAIN:COUNT-CH" CALLS? TTRUE

   s" and the arm over the copied fold holds that column's body, not the other's"
   T-LABEL
   s" WORKLOAD:SCAN-NEW" s" HOT-CHAIN:FOLD-C" COPIED-FROM? TTRUE
   s" WORKLOAD:SCAN-OLD" s" HOT-CHAIN:FOLD-C" COPIED-FROM? TFALSE

   s" and no arm enters the other column's" T-LABEL
   s" WORKLOAD:SCAN-OLD" s" HOT-CHAIN:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:SCAN-NEW" s" HOT-ENGINE:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:COUNT-OLD" s" HOT-CHAIN:COUNT-CH" CALLS? TFALSE
   s" WORKLOAD:COUNT-NEW" s" HOT-ENGINE:COUNT-CH" CALLS? TFALSE

   s" the control's two arms both enter the subject nothing migrated" T-LABEL
   s" WORKLOAD:SCAN-CTL-A" s" HOT-FIXED:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:SCAN-CTL-B" s" HOT-FIXED:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:SCAN-CTL-B" s" HOT-CHAIN:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:COUNT-CTL-B" s" HOT-CHAIN:COUNT-CH" CALLS? TFALSE

   s" a calling arm holds exactly one call, which is its subject's" T-LABEL
   s" WORKLOAD:SCAN-OLD" BLS-IN 1 T=
   s" WORKLOAD:COUNT-OLD" BLS-IN 1 T=
   s" WORKLOAD:COUNT-NEW" BLS-IN 1 T=

   s" and the arm over the copied fold holds none" T-LABEL
   s" WORKLOAD:SCAN-NEW" BLS-IN 0 T=

   s" and the arms over the inlined subjects hold none, in either column" T-LABEL
   s" WORKLOAD:TERM-OLD" BLS-IN 0 T=
   s" WORKLOAD:TERM-NEW" BLS-IN 0 T=
   s" WORKLOAD:TERM-CTL-A" BLS-IN 0 T=
   s" WORKLOAD:TERM-CTL-B" BLS-IN 0 T=

   s" so the after-arm over the inlined subjects is the smaller code" T-LABEL
   s" WORKLOAD:TERM-NEW" WORD-BYTES  s" WORKLOAD:TERM-OLD" WORD-BYTES  < TTRUE
   s" WORKLOAD:TERM-CTL-B" WORD-BYTES s" WORKLOAD:TERM-OLD" WORD-BYTES T= ;

\ ---- one body, two arms ------------------------------------------------------
\ A driver is published under a different name in each arm because two records
\ with one name in one wordlist is a duplicate definition. A name lives in the
\ dictionary record and not in the compiled body, so two arms whose subjects
\ compile to a call in both columns must come out the same number of bytes.
: BODY-CASES ( -- )
   s" the two arms of a calling workload are the same code size" T-LABEL
   s" WORKLOAD:SCAN-CTL-A" WORD-BYTES s" WORKLOAD:SCAN-OLD" WORD-BYTES T=
   s" WORKLOAD:SCAN-CTL-B" WORD-BYTES s" WORKLOAD:SCAN-OLD" WORD-BYTES T=
   s" WORKLOAD:COUNT-NEW" WORD-BYTES  s" WORKLOAD:COUNT-OLD" WORD-BYTES T=
   s" WORKLOAD:COUNT-CTL-B" WORD-BYTES s" WORKLOAD:COUNT-OLD" WORD-BYTES T= ;

;using

\ ---- the answers -------------------------------------------------------------
\ The generated data, and what each workload computes over it. These are pinned
\ so that "the two arms agree" is a statement about the code rather than about
\ two arms that both compute nothing.
: ANSWER-CASES ( -- )
   s" the generated data is the same bytes in every run" T-LABEL
   CODEGEN-HOT:BYTE-SUM 320399 T=
   CODEGEN-HOT:TERM-SUM 2211949911040 T=

   s" the two arms of each workload compute the same answer" T-LABEL
   CODEGEN-RUN:SCAN-OLD-SUM CODEGEN-RUN:SCAN-NEW-SUM T=
   CODEGEN-RUN:COUNT-OLD-SUM CODEGEN-RUN:COUNT-NEW-SUM T=
   CODEGEN-RUN:TERM-OLD-SUM CODEGEN-RUN:TERM-NEW-SUM T=

   s" and the control's two arms do too" T-LABEL
   CODEGEN-RUN:SCAN-CTL-A-SUM CODEGEN-RUN:SCAN-CTL-B-SUM T=
   CODEGEN-RUN:COUNT-CTL-A-SUM CODEGEN-RUN:COUNT-CTL-B-SUM T=
   CODEGEN-RUN:TERM-CTL-A-SUM CODEGEN-RUN:TERM-CTL-B-SUM T=

   s" and the answers are the pinned ones" T-LABEL
   CODEGEN-RUN:SCAN-OLD-SUM 355375 T=
   CODEGEN-RUN:COUNT-OLD-SUM 47 T=
   CODEGEN-RUN:TERM-OLD-SUM 276493745152 T= ;

\ ---- the compile-shaped row --------------------------------------------------
\ Its delta is a timing and is not checked here. What IS checked is the fact
\ without which the delta would mean nothing: both arms compiled the same amount.
\ A batch publishes one record per definition and one for the package it opens,
\ and an arm runs one untimed batch ahead of its timed ones.
: CHECK-ROW-CASES ( -- )
   CODEGEN-HOT:BATCH-DEFS 1+ CODEGEN-RUN:CHECK-ROUNDS 1+ * {: want:n :}
   s" the compile-shaped workload's two arms compiled the same amount" T-LABEL
   s" check-batch" CODEGEN-CLOCK:ROW-OF {: k:n :}
   k 0 >= TTRUE
   k CODEGEN-CLOCK:SAME-ANSWER? TTRUE
   k CODEGEN-CLOCK:OLD-SUM want T=
   k CODEGEN-CLOCK:NEW-SUM want T=

   s" and it is recorded as the one row whose arms could not be interleaved" T-LABEL
   k CODEGEN-CLOCK:INTERLEAVED? TFALSE ;

\ ---- the two timing words that decide what a row MEANS ------------------------
\ A row's delta and a row's columns are the two places where the report can be
\ confidently wrong: a delta that lost its sign turns a loss into a win of the
\ same size, and a column that got the other arm turns the sign around as well.
\ Neither of those is a timing, and neither needs a clock to check.
\
\ THE DELTA, ON PAIRS THIS FILE CHOSE. DELTA-OF is the arithmetic behind every
\ number the report prints, handed old and new directly. A row's own two times
\ only ever arrive from a clock; these do not, so the sign can be stated.
: DELTA-CASES ( -- )
   s" a new arm that ran faster saves a positive share of the old" T-LABEL
   1000 800 CODEGEN-CLOCK:DELTA-OF 200 T=
   2000 1000 CODEGEN-CLOCK:DELTA-OF CODEGEN-CLOCK:PERMILLE 2 / T=

   s" a new arm that ran slower reports a LOSS, and keeps the sign" T-LABEL
   1000 1200 CODEGEN-CLOCK:DELTA-OF -200 T=
   1000 2000 CODEGEN-CLOCK:DELTA-OF CODEGEN-CLOCK:PERMILLE 0 swap - T=
   1000 1200 CODEGEN-CLOCK:DELTA-OF 0 < TTRUE

   s" two arms that ran the same report neither" T-LABEL
   1000 1000 CODEGEN-CLOCK:DELTA-OF 0 T=

   s" and an old arm of no time at all is refused, never divided by" T-LABEL
   [: 0 1 CODEGEN-CLOCK:DELTA-OF drop ;] CODEGEN-CLOCK:E-WLTIME-CLOCK TTHROWSQ ;

\ THE COLUMNS, ON ROWS MEASURED THE WAY EVERY ROW IS. The rows below go through
\ CODEGEN-CLOCK:OPEN-REAL / OPEN-NULL and PAIR and SWEEP - the same words the
\ report's rows go through - and they form a family of their own so the bar
\ machinery sees them exactly as it sees a workload's.
\
\ WHAT IDENTIFIES A PAIR ROW'S COLUMNS IS ITS ANSWERS. The real row's two arms
\ do nothing and compute two DIFFERENT answers. An answer is the part of a row
\ that is not a measurement, and because each column's three numbers are stored
\ in one decision, a store that put an arm's run in the other column put its
\ answer there too. So this reads the answers back and fails on a swap, with
\ nothing in it that depends on how long anything took.
\
\ A SWEEP ROW'S COLUMNS ARE NOT ARMS, AND ITS ANSWERS CANNOT TELL THEM APART.
\ A sweep times five publications of ONE body, so every arm computes the same
\ value and both columns carry it; the answer test above says nothing there. What
\ identifies a sweep's columns is its CONSTRUCTION: the old column takes the
\ smallest fastest run of the five and the new column the largest, so the old is
\ never the greater and the row's delta is never positive. That holds whatever
\ the five times were - the least of a set is not its greatest - so it is a fact
\ about the store and not a timing, and it is the case that catches a sweep
\ writing its extremes into the wrong columns.
1000 constant ARM-REPS
2 constant ARM-ROUNDS
11 constant ARM-OLD-ANSWER
22 constant ARM-NEW-ANSWER
33 constant ARM-NULL-ANSWER

: NOTHING-OLD ( -- ) ;
: NOTHING-NEW ( -- ) ;

: MEASURE-ARM-ROWS ( -- )
   s" arm-store" s" arm-store" CODEGEN-CLOCK:OPEN-REAL
   ARM-REPS ARM-ROUNDS ARM-OLD-ANSWER ARM-NEW-ANSWER
      [: NOTHING-OLD ;] [: NOTHING-NEW ;] CODEGEN-CLOCK:PAIR
   s" arm-store-null" s" arm-store" CODEGEN-CLOCK:OPEN-NULL
   ARM-REPS ARM-ROUNDS ARM-NULL-ANSWER ARM-NULL-ANSWER
      [: NOTHING-OLD ;] [: NOTHING-OLD ;] CODEGEN-CLOCK:PAIR
   s" arm-store-sweep" s" arm-store" CODEGEN-CLOCK:OPEN-NULL
   ARM-REPS ARM-ROUNDS ARM-NULL-ANSWER
      [: NOTHING-OLD ;] [: NOTHING-OLD ;] [: NOTHING-OLD ;]
      [: NOTHING-OLD ;] [: NOTHING-OLD ;] CODEGEN-CLOCK:SWEEP ;

: ARM-CASES ( -- )
   MEASURE-ARM-ROWS
   s" a pair row's two columns hold the arms they were given, not each other" T-LABEL
   s" arm-store" CODEGEN-CLOCK:ROW-OF {: k:n :}
   k 0 >= TTRUE
   k CODEGEN-CLOCK:REAL? TTRUE
   k CODEGEN-CLOCK:OLD-SUM ARM-OLD-ANSWER T=
   k CODEGEN-CLOCK:NEW-SUM ARM-NEW-ANSWER T=
   k CODEGEN-CLOCK:SAME-ANSWER? TFALSE

   s" and it recorded the shape of the run it actually made" T-LABEL
   k CODEGEN-CLOCK:REPS ARM-REPS T=
   k CODEGEN-CLOCK:ROUNDS ARM-ROUNDS T=
   k CODEGEN-CLOCK:INTERLEAVED? TTRUE

   s" and it kept a time for each column, which is not the same as a fast one" T-LABEL
   k CODEGEN-CLOCK:OLD-NS 0 > TTRUE
   k CODEGEN-CLOCK:NEW-NS 0 > TTRUE

   s" the row is what it was opened as, in the family it was opened in" T-LABEL
   k CODEGEN-CLOCK:FAM$ s" arm-store" T$=
   s" arm-store-null" CODEGEN-CLOCK:ROW-OF CODEGEN-CLOCK:NULL? TTRUE
   s" arm-store" CODEGEN-CLOCK:NULLS 2 T=

   s" a sweep keeps its extremes, so its old column is never the greater" T-LABEL
   s" arm-store-sweep" CODEGEN-CLOCK:ROW-OF {: w:n :}
   w 0 >= TTRUE
   w CODEGEN-CLOCK:NULL? TTRUE
   w CODEGEN-CLOCK:OLD-NS 0 > TTRUE
   w CODEGEN-CLOCK:OLD-NS  w CODEGEN-CLOCK:NEW-NS  <= TTRUE
   w CODEGEN-CLOCK:DELTA-PERMILLE 0 <= TTRUE
   w CODEGEN-CLOCK:SAME-ANSWER? TTRUE ;

\ ---- the timing store's own refusals -----------------------------------------
\ An arm handed to a store with no row open. The body is a word rather than a
\ quotation written inline because the arm itself takes one, and the case has to
\ hand the whole call to `catch`.
: NOTHING ( -- ) ;

: ARM-WITH-NO-ROW ( -- )
   1 1 [: NOTHING ;] CODEGEN-CLOCK:ARM-OLD ;

\ The store is what every reported number comes out of, so the ways it can be
\ misused are checked rather than assumed. None of these reads a clock.
: STORE-CASES ( -- )
   s" a row index past the recorded count is refused" T-LABEL
   [: CODEGEN-CLOCK:ROWS CODEGEN-CLOCK:OLD-NS drop ;] CODEGEN-CLOCK:E-WLTIME-ROW TTHROWSQ
   [: -1 CODEGEN-CLOCK:NAME$ drop drop ;] CODEGEN-CLOCK:E-WLTIME-ROW TTHROWSQ

   s" an arm measured with no row open is refused" T-LABEL
   [: ARM-WITH-NO-ROW ;] CODEGEN-CLOCK:E-WLTIME-STATE TTHROWSQ
   [: CODEGEN-CLOCK:CLOSE ;] CODEGEN-CLOCK:E-WLTIME-STATE TTHROWSQ

   s" and a subject the dictionary does not hold is refused by the scan" T-LABEL
   [: s" CODEGEN-WORKLOAD-TEST:NO-SUCH-WORD" CODEGEN-SCAN:WORD-BYTES drop ;]
      CODEGEN-SCAN:E-WLSCAN-SUBJECT TTHROWSQ ;

using CODEGEN-SCAN

\ ---- the placement sweep's arms ---------------------------------------------
\ The bar every verdict is held against comes out of five drivers over identical
\ code that reach five different publications of one subject. If two of them
\ entered the same record the sweep would be timing a body against itself, the
\ widest gap it found would be too small, and every verdict beside it would be
\ too generous. So each arm is checked to enter ITS OWN publication and no other.
: PLACE-WIRING-CASES ( -- )
   s" each placement arm enters its own publication" T-LABEL
   s" WORKLOAD:SCAN-F1" s" HOT-F1:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:SCAN-F2" s" HOT-F2:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:SCAN-F3" s" HOT-F3:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:SCAN-F4" s" HOT-F4:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:COUNT-F1" s" HOT-F1:COUNT-CH" CALLS? TTRUE
   s" WORKLOAD:COUNT-F4" s" HOT-F4:COUNT-CH" CALLS? TTRUE

   s" and no other publication, the reference one included" T-LABEL
   s" WORKLOAD:SCAN-F1" s" HOT-ENGINE:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:SCAN-F1" s" HOT-F2:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:SCAN-F2" s" HOT-F1:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:SCAN-F4" s" HOT-CHAIN:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:COUNT-F1" s" HOT-ENGINE:COUNT-CH" CALLS? TFALSE
   s" WORKLOAD:COUNT-F4" s" HOT-CHAIN:COUNT-CH" CALLS? TFALSE

   s" a placement arm holds exactly the one call its subject needs" T-LABEL
   s" WORKLOAD:SCAN-F1" BLS-IN 1 T=
   s" WORKLOAD:SCAN-F4" BLS-IN 1 T=
   s" WORKLOAD:COUNT-F1" BLS-IN 1 T=

   s" and the five arms of one sweep are the same code size" T-LABEL
   s" WORKLOAD:SCAN-F1" WORD-BYTES s" WORKLOAD:SCAN-OLD" WORD-BYTES T=
   s" WORKLOAD:SCAN-F2" WORD-BYTES s" WORKLOAD:SCAN-OLD" WORD-BYTES T=
   s" WORKLOAD:SCAN-F3" WORD-BYTES s" WORKLOAD:SCAN-OLD" WORD-BYTES T=
   s" WORKLOAD:SCAN-F4" WORD-BYTES s" WORKLOAD:SCAN-OLD" WORD-BYTES T= ;

\ ---- the mixed-coverage workloads -------------------------------------------
\ These two rows are the middle of the coverage curve, and what makes them that
\ is a COUNT of call instructions: mix66 reaches the migrated subject twice per
\ pass and the unmigrated one once, mix33 the other way round. Read off the
\ compiled code, because the claim is about what the arm executes and not about
\ what its source says - a compiler that folded the two identical calls into one
\ would leave the source untouched and the coverage wrong.
: MIX-CASES ( -- )
   s" the mixed arms reach both the migrated subject and the fixed one" T-LABEL
   s" WORKLOAD:MIX66-OLD" s" HOT-ENGINE:COUNT-CH" CALLS? TTRUE
   s" WORKLOAD:MIX66-OLD" s" HOT-FIXED:COUNT-CH" CALLS? TTRUE
   s" WORKLOAD:MIX66-NEW" s" HOT-CHAIN:COUNT-CH" CALLS? TTRUE
   s" WORKLOAD:MIX66-NEW" s" HOT-FIXED:COUNT-CH" CALLS? TTRUE
   s" WORKLOAD:MIX33-OLD" s" HOT-ENGINE:COUNT-CH" CALLS? TTRUE
   s" WORKLOAD:MIX33-NEW" s" HOT-CHAIN:COUNT-CH" CALLS? TTRUE

   s" and no mixed arm enters the other column's subject" T-LABEL
   s" WORKLOAD:MIX66-OLD" s" HOT-CHAIN:COUNT-CH" CALLS? TFALSE
   s" WORKLOAD:MIX66-NEW" s" HOT-ENGINE:COUNT-CH" CALLS? TFALSE
   s" WORKLOAD:MIX33-OLD" s" HOT-CHAIN:COUNT-CH" CALLS? TFALSE
   s" WORKLOAD:MIX33-NEW" s" HOT-ENGINE:COUNT-CH" CALLS? TFALSE

   s" the coverage is two of three passes and one of three, in the code" T-LABEL
   s" WORKLOAD:MIX66-OLD" s" HOT-ENGINE:COUNT-CH" CALLS-IN 2 T=
   s" WORKLOAD:MIX66-OLD" s" HOT-FIXED:COUNT-CH" CALLS-IN 1 T=
   s" WORKLOAD:MIX66-NEW" s" HOT-CHAIN:COUNT-CH" CALLS-IN 2 T=
   s" WORKLOAD:MIX66-NEW" s" HOT-FIXED:COUNT-CH" CALLS-IN 1 T=
   s" WORKLOAD:MIX33-OLD" s" HOT-ENGINE:COUNT-CH" CALLS-IN 1 T=
   s" WORKLOAD:MIX33-OLD" s" HOT-FIXED:COUNT-CH" CALLS-IN 2 T=
   s" WORKLOAD:MIX33-NEW" s" HOT-CHAIN:COUNT-CH" CALLS-IN 1 T=
   s" WORKLOAD:MIX33-NEW" s" HOT-FIXED:COUNT-CH" CALLS-IN 2 T=

   s" so every mixed arm holds three calls and not one folded together" T-LABEL
   s" WORKLOAD:MIX66-OLD" BLS-IN 3 T=
   s" WORKLOAD:MIX66-NEW" BLS-IN 3 T=
   s" WORKLOAD:MIX33-OLD" BLS-IN 3 T=
   s" WORKLOAD:MIX33-NEW" BLS-IN 3 T= ;

;using

\ ---- the answers the new rows compute ---------------------------------------
\ Three passes over the buffer, so three times the count row's answer, whichever
\ publications the passes went through.
: MIX-ANSWER-CASES ( -- )
   s" the two arms of each mixed workload compute the same answer" T-LABEL
   CODEGEN-RUN:MIX66-OLD-SUM CODEGEN-RUN:MIX66-NEW-SUM T=
   CODEGEN-RUN:MIX33-OLD-SUM CODEGEN-RUN:MIX33-NEW-SUM T=

   s" and it is three passes' worth of the count row's pinned answer" T-LABEL
   CODEGEN-RUN:MIX66-OLD-SUM 141 T=
   CODEGEN-RUN:MIX33-OLD-SUM 141 T=

   s" every publication in a placement sweep computes its family's answer" T-LABEL
   CODEGEN-RUN:SCAN-F1-SUM CODEGEN-RUN:SCAN-OLD-SUM T=
   CODEGEN-RUN:SCAN-F2-SUM CODEGEN-RUN:SCAN-OLD-SUM T=
   CODEGEN-RUN:SCAN-F3-SUM CODEGEN-RUN:SCAN-OLD-SUM T=
   CODEGEN-RUN:SCAN-F4-SUM CODEGEN-RUN:SCAN-OLD-SUM T=
   CODEGEN-RUN:COUNT-F1-SUM CODEGEN-RUN:COUNT-OLD-SUM T=
   CODEGEN-RUN:COUNT-F4-SUM CODEGEN-RUN:COUNT-OLD-SUM T=
   CODEGEN-RUN:TERM-F1-SUM CODEGEN-RUN:TERM-OLD-SUM T=
   CODEGEN-RUN:TERM-F4-SUM CODEGEN-RUN:TERM-OLD-SUM T=
   CODEGEN-RUN:MIX66-F1-SUM CODEGEN-RUN:MIX66-OLD-SUM T=
   CODEGEN-RUN:MIX33-F4-SUM CODEGEN-RUN:MIX33-OLD-SUM T= ;

\ ---- the bar behind a verdict ------------------------------------------------
\ A verdict is a delta held against the largest delta this harness produced when
\ nothing changed, and the rows that measure that are the family's null rows.
\ Before this, the report named those rows by hand and a name that matched no row
\ scored as a bar of nothing: renaming one row made every verdict read REAL and
\ the run still exited zero. The bar now comes out of the recorded rows, and a
\ family with none of them throws.
\
\ None of this reads a clock. Whether a bar is large or small is a timing; that
\ every judged row HAS one, and that no null row exceeds the bar its own family's
\ null rows set, are facts about the store.
: BAR-CASES ( -- )
   s" a family with no null row has no bar, and asking for one throws" T-LABEL
   s" CODEGEN-WORKLOAD-TEST:NO-SUCH-FAMILY" CODEGEN-CLOCK:NULLS 0 T=
   [: s" CODEGEN-WORKLOAD-TEST:NO-SUCH-FAMILY" CODEGEN-CLOCK:BAR-PERMILLE drop ;]
      CODEGEN-CLOCK:E-WLTIME-BAR TTHROWSQ

   s" the compile-shaped row's own family carries four null draws" T-LABEL
   s" check" CODEGEN-CLOCK:NULLS 4 T=

   s" every row the report judges has null rows behind its bar" T-LABEL
   CODEGEN-CLOCK:ROWS 0 ?do
      i CODEGEN-CLOCK:REAL? if
         i CODEGEN-CLOCK:FAM$ CODEGEN-CLOCK:NULLS 0 > TTRUE
      then
   loop

   s" and no null row clears the bar its own family's draws set" T-LABEL
   CODEGEN-CLOCK:ROWS 0 ?do
      i CODEGEN-CLOCK:NULL? if
         i CODEGEN-CLOCK:OVER-BAR? TFALSE
      then
   loop ;

\ ---- the compile-shaped family, real row and null draws alike ----------------
\ Every one of them compiles the same generated text the same number of times, so
\ every one of them must publish the same number of records on both arms. A draw
\ whose two sequences compiled different amounts is not a null draw and its delta
\ is not a bar.
: DRIFT-ROW-CASES ( -- )
   CODEGEN-HOT:BATCH-DEFS 1+ CODEGEN-RUN:CHECK-ROUNDS 1+ * {: want:n :}
   s" each null draw compiled the same amount in both of its sequences" T-LABEL
   CODEGEN-CLOCK:ROWS 0 ?do
      i CODEGEN-CLOCK:FAM$ s" check" STR= if
         i CODEGEN-CLOCK:SAME-ANSWER? TTRUE
         i CODEGEN-CLOCK:OLD-SUM want T=
         i CODEGEN-CLOCK:NEW-SUM want T=
         i CODEGEN-CLOCK:INTERLEAVED? TFALSE
      then
   loop ;


\ ---- the one arm question that does need a clock ------------------------------
\ ARM-CASES above tells the two columns apart by their ANSWERS, which is all a
\ scheduled suite can do. It leaves one thing unsaid: a store that swapped the
\ two arms' TIMES while leaving their answers alone, or a delta that read the two
\ times in the other order, inverts every verdict the report prints and nothing
\ clock-free can see it. Two recorded times are two numbers from the same clock,
\ and which arm each came from is not recoverable from them unless the arms did
\ measurably different amounts of work.
\
\ So the row below is deliberately lopsided - its old arm does thousands of times
\ the work of its new arm - and the two assertions are about ORDER, at a margin
\ no host can close. They still read a clock, which is why they live behind
\ TIMED and are reached only from tools/codegen-workload-timed-test.f, run by
\ hand, exactly as tools/codegen-compare-timed-test.f holds the cost-direction
\ assertions its own scheduled suite may not make.
private

64 constant TIMED-REPS
5 constant TIMED-ROUNDS
4096 constant TIMED-WORK

variable SPIN

: HEAVY ( -- )
   0 SPIN !
   TIMED-WORK 0 ?do SPIN @ 1+ SPIN ! loop ;

: LIGHT ( -- )
   0 SPIN ! ;

public

: TIMED ( -- )
   T-RESET
   s" timed-arms" s" timed-arms" CODEGEN-CLOCK:OPEN-REAL
   TIMED-REPS TIMED-ROUNDS 1 1
      [: HEAVY ;] [: LIGHT ;] CODEGEN-CLOCK:PAIR

   s" the column a row calls old holds the arm it was handed as old" T-LABEL
   s" timed-arms" CODEGEN-CLOCK:ROW-OF {: k:n :}
   k 0 >= TTRUE
   k CODEGEN-CLOCK:OLD-NS  k CODEGEN-CLOCK:NEW-NS  > TTRUE

   s" and a new arm that ran faster is reported as a saving" T-LABEL
   k CODEGEN-CLOCK:DELTA-PERMILLE 0 > TTRUE
   T-REPORT
   s" codegen-workload-timed-test: ok" type cr ;

: MAIN ( -- )
   T-RESET
   SIZE-CASES
   SLOT-CASES
   CLAUSE-CASES
   SPAN-CASES
   RULE-CASES
   SURVEY-CASES
   SUBJECT-CASES
   WIRING-CASES
   PLACE-WIRING-CASES
   MIX-CASES
   BODY-CASES
   ANSWER-CASES
   MIX-ANSWER-CASES
   CHECK-ROW-CASES
   DRIFT-ROW-CASES
   DELTA-CASES
   ARM-CASES
   BAR-CASES
   STORE-CASES
   T-REPORT
   s" codegen-workload-test: ok" type cr ;

;package

CODEGEN-WORKLOAD-TEST:MAIN
