\ codegen-tail-probe.f - reading a routine's calls and its exit off the emitted
\ code. One concern: tools/codegen-tail-probe.f, and the two mistakes a tool
\ that answers "does this routine end in a call" is easy to make.
\
\ WHY THE TOOL EXISTS AND WHAT WOULD MAKE IT USELESS. A tail-call lane may only
\ act on routines whose emitted code really ends in a call, and the source token
\ is no evidence at all: both code generators copy a small callee into its caller
\ instead of calling it, so a body that WRITES a final call may emit none. The
\ probe therefore counts branch instructions in the published code. Two ways that
\ can go wrong, and this suite is built around both:
\
\   1. COUNTING A COPIED CALLEE AS A CALL. A body whose final call was inlined
\      has no branch-with-link in it, and a probe that answered from the source
\      would report a tail call the lane could then "optimise" into a branch to
\      nothing. The fixtures below include a caller of a callee small enough to
\      be copied, and its call count must be zero.
\
\   2. READING A LOOP'S BACK EDGE AS A TAIL BRANCH. This is the mistake the
\      first version of the tool actually made. A routine that returns nothing
\      and needs no frame can have an empty exit block, and then the instruction
\      before its trailing return is the PREVIOUS block's terminator - an
\      unconditional branch, the loop's own back edge. A predicate that asked
\      only "is the last body instruction a `b`" reported two ordinary loop rows
\      as leaving by a tail branch. The fixture LOOPY-N below is exactly that
\      shape, built to fool the tool, and TAIL-BRANCH? must answer false on it:
\      the question is settled by where the branch GOES, not by its opcode.
\      LOOPY-N's last body instruction really is a `b` - that is checked by the
\      shape of the fixture rather than assumed, because a fixture that stopped
\      reproducing the layout would leave this suite green and proving nothing.
\
\ AND ONE OF THEM REALLY DOES LEAVE BY A BRANCH NOW, which is the third thing
\ this suite has to be able to tell apart. The chain lowers a final call whose
\ results are already the routine's own results to a plain `b` to the callee, so
\ TAIL-BRANCH? has a true case as well as the two false ones - and the false case
\ built to fool it, LOOPY-N's back edge, is still false beside it. Without the
\ true case the predicate could answer false always and this suite would not
\ notice; with it, the distinction the tool exists to make is pinned in both
\ directions by two routines whose LAST BODY INSTRUCTION IS A `b` EITHER WAY.
\
\ THE TAIL ROUTINE ALSO HAS NO TRAILING RETURN, and that is the record convention
\ every reader of a word's code depends on, so it is asserted rather than left to
\ be discovered: src/compiler/native/publish.f records the WHOLE emission for
\ such a routine, because there is no return to leave out of it, and TRAILER-RET?
\ is therefore false where it is true for every other row here.
\
\ THE FIXTURES ARE COMPILED BOTH WAYS ON PURPOSE. The copying and call-counting
\ cases are ordinary definitions the ENGINE compiles, because the copying rule
\ under test is the engine's. The back-edge case is migrated through the native
\ CHAIN, because that layout is the chain's and the same body compiled by the
\ engine does not reproduce it. The probe itself reads a dictionary record and a
\ code span, which every published word has however it was compiled.

require lib/prelude.f
require lib/test.f
require src/compiler/native/dict.f
require src/compiler/native/migrate.f
require tools/codegen-tail-probe.f

package NTP-FIXTURE

public

\ A callee small enough that the engine copies it into any caller: one literal
\ and one binary operation, the shape tools/codegen-compare-corpus4.f measures
\ the copying rule with.
: TINY ( n -- n )
   1 + ;

\ And one it will not copy: eleven operations, well past the forty body bytes
\ the engine's INL-MAX allows.
: BIG ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

\ A leaf: no call of any kind.
: PLAIN ( n -- n )
   dup * 3 + ;

\ A caller of the copied callee. It writes a call and emits none, which is the
\ first mistake above.
: COPIES ( n -- n )
   TINY TINY ;

\ A caller of the sized one. This really does emit a call instruction.
: CALLS-BIG ( n -- n )
   BIG ;

;package

\ ---- the fixture built to fool the tool --------------------------------------
\ A counted loop that returns nothing. Its exit block has nothing to do - no
\ result to store, no frame to release - so the last instruction of the recorded
\ body is the LOOP'S BACK EDGE, an unconditional branch, and the trailing return
\ stands alone after it. That is the shape a predicate reading only the opcode
\ calls a tail branch.
\
\ IT HAS TO BE THE CHAIN'S CODE AND NOT THE ENGINE'S. The engine's emitter does
\ not lay a routine out this way: the same body compiled by it ends its recorded
\ span on ordinary work. The false positive was found on the chain's columns
\ (VEC-COPY-CELLS-N and T-SGD!-N of the codegen-compare corpora), so the fixture
\ is migrated through the real chain rather than written as an ordinary
\ definition that would not reproduce it.

package NTP-MIGRATED

private

8 constant REGS

\ Where a published word's code starts, which is what a call site is staged
\ with. The spelling is the qualified one, because the routines below are
\ published inside the fixture's package.
: ENTRY-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-NPUB-NAME throw then
   XREF-START ;

\ The callee the tail row leaves through: eleven operations, which is past the
\ engine's forty bytes of body and past the chain's own bound for a routine of
\ arity one to one, so neither generator copies it and the caller really does
\ have a branch in it.
: BIG ( -- )
   s" : NTP-BIG-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ AND THE CALLER WHOSE CALLEE IS OUTSIDE THE REGION, which is the case the
\ lowering DECLINES rather than refuses. `abs` is an engine primitive: its code
\ is in the loaded image's text and not in the JIT region, so a branch to it does
\ not keep its distance when a snapshot is restored somewhere else, and
\ src/compiler/native/publish.f has no way to record one (dot
\ habu-relocate-a-tail-96d571af). The routine is a perfectly ordinary program and
\ has to stay one: it keeps its call and its return, exactly as it did before
\ there was a tail lowering at all. An optimisation that turned it into a
\ compilation failure would not be an optimisation.
: OUTSIDE ( -- )
   s" abs" NDICT:SPELL-START {: e:n :}
   s" abs" e 1 1 NMIGRATE:CALLEE
   s" : OUTSIDE-N ( n -- n ) abs ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ And the caller whose whole body is that call. Its emitted code is one
\ instruction: the branch.
: TAILED ( -- )
   s" NTP-BIG-N" s" NTP-FIXTURE:NTP-BIG-N" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : TAILED-N ( n -- n ) NTP-BIG-N ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

public

: RUN ( -- )
   s" : LOOPY-N ( ptr n n -- ) {: base:ptr len:n :} len 0 ?do i base i cells + ! loop ;"
   2 0 REGS NMIGRATE:DEFINE
   BIG
   TAILED
   OUTSIDE ;

;package

package NTP-FIXTURE
public

NTP-MIGRATED:RUN

create SINK 8 cells allot

: SINK-AT ( -- ptr n )
   SINK ;

;package

package NTP-TEST

using NTAILPROBE

public

: CASES ( -- )
   s" a routine that calls nothing reports no call" T-LABEL
   s" NTP-FIXTURE:PLAIN" CALLS 0 T=
   s" NTP-FIXTURE:PLAIN" LAST-CALL-IX -1 T=
   s" NTP-FIXTURE:PLAIN" AFTER-LAST-CALL -1 T=

   s" a body whose callee was copied into it reports no call either" T-LABEL
   s" NTP-FIXTURE:COPIES" CALLS 0 T=

   s" and a caller of a callee too big to copy reports the call it makes" T-LABEL
   s" NTP-FIXTURE:CALLS-BIG" CALLS 1 T=
   s" NTP-FIXTURE:CALLS-BIG" LAST-CALL-IX 0 >= TTRUE

   s" what follows the last call is counted, and it is the epilogue" T-LABEL
   s" NTP-FIXTURE:CALLS-BIG" AFTER-LAST-CALL 0 > TTRUE

   s" every one that returns ends in the trailing return the record promises" T-LABEL
   s" NTP-FIXTURE:PLAIN" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:COPIES" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:CALLS-BIG" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:LOOPY-N" TRAILER-RET? TTRUE

   s" the back-edge fixture really does end its body on a branch" T-LABEL
   s" NTP-FIXTURE:LOOPY-N" LAST-BODY NBR:B? TTRUE

   s" and so does the routine that LEAVES through its callee" T-LABEL
   s" NTP-FIXTURE:TAILED-N" LAST-BODY NBR:B? TTRUE

   s" but only one of the two goes anywhere outside itself" T-LABEL
   s" NTP-FIXTURE:PLAIN" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:COPIES" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:CALLS-BIG" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:LOOPY-N" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:TAILED-N" TAIL-BRANCH? TTRUE

   s" and the one that does has no trailing return and makes no call" T-LABEL
   s" NTP-FIXTURE:TAILED-N" TRAILER-RET? TFALSE
   s" NTP-FIXTURE:TAILED-N" CALLS 0 T=
   s" NTP-FIXTURE:TAILED-N" INSNS 1 T=

   s" while the routine it leaves through is a whole body of its own" T-LABEL
   s" NTP-FIXTURE:NTP-BIG-N" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:NTP-BIG-N" TRAILER-RET? TTRUE

   s" and a routine whose callee is outside the region keeps its call" T-LABEL
   s" NTP-FIXTURE:OUTSIDE-N" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:OUTSIDE-N" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:OUTSIDE-N" CALLS 1 T=

   s" a name nothing published is a refusal and not a quiet zero" T-LABEL
   [: s" NTP-FIXTURE:NO-SUCH-WORD" CALLS drop ;]
      E-CODEGEN-COMPARE-SUBJECT TTHROWSQ ;

;using

;package

T-RESET
NTP-TEST:CASES
T-REPORT
