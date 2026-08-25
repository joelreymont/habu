\ codegen-tail-probe.f - reading a routine's calls and its exit off the emitted

require lib/prelude.f
require lib/test.f
require src/compiler/native/dict.f
require src/compiler/native/compiler.f
require tools/codegen-tail-probe.f

package NTP-FIXTURE

public

\ A callee small enough that the engine copies it into any caller.
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

: OUTSIDE ( n -- n )
   abs ;

: TAILED ( n -- n )
   BIG ;

: EMPTY ( -- ) ;

: LOOPY ( ptr n n -- ) {: base:ptr len:n :}
   len 0 ?do i base i cells + ! loop ;

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
\ is compiled through the real chain rather than written as an ordinary
\ definition that would not reproduce it.

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
   s" NTP-FIXTURE:LOOPY" TRAILER-RET? TTRUE

   \ THE FIXTURE BUILT TO FOOL THIS TOOL IS NO LONGER THE SHAPE THAT FOOLS IT, AND
   \ SAYING SO IS THE POINT OF ASSERTING THE PRECONDITION. The emitter's collapse
   \ (src/compiler/native/emit.f, ORDER-BLOCKS) branches past any block that emits
   \ nothing before its terminator, and a counted loop's latch is exactly such a
   \ block - so the conditional at the bottom of the body now names the header
   \ itself and the loop's back edge is a `b.cc`. No row of any corpus ends its
   \ recorded body on an unconditional branch any more; that was measured across
   \ all 54 compiled rows, not assumed. So this case asserts what is now true
   \ instead of a hazard that cannot be built, and the hazard itself is carried by
   \ dot habu-hand-built-fixture-a6a4efe7, which assembles the shape by hand
   \ rather than waiting for a code generator to emit one again.
   s" the loop row no longer ends its body on an unconditional branch" T-LABEL
   s" NTP-FIXTURE:LOOPY" LAST-BODY NBR:B? TFALSE
   s" NTP-FIXTURE:LOOPY" LAST-BODY NBR:COND? TTRUE

   s" and so does the routine that LEAVES through its callee" T-LABEL
   s" NTP-FIXTURE:TAILED" LAST-BODY NBR:B? TTRUE

   s" but only one of the two goes anywhere outside itself" T-LABEL
   s" NTP-FIXTURE:PLAIN" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:COPIES" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:CALLS-BIG" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:LOOPY" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:TAILED" TAIL-BRANCH? TTRUE

   s" and the one that does has no trailing return and makes no call" T-LABEL
   s" NTP-FIXTURE:TAILED" TRAILER-RET? TFALSE
   s" NTP-FIXTURE:TAILED" CALLS 0 T=
   s" NTP-FIXTURE:TAILED" INSNS 1 T=

   s" while the routine it leaves through is a whole body of its own" T-LABEL
   s" NTP-FIXTURE:BIG" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:BIG" TRAILER-RET? TTRUE

   s" and a routine whose callee is outside the region keeps its call" T-LABEL
   s" NTP-FIXTURE:OUTSIDE" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:OUTSIDE" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:OUTSIDE" CALLS 1 T=

   \ ---- and how many bytes of code each of them really is --------------------
   \ The recorded length is the span a CALLER may copy, so for everything that
   \ returns it is the routine less its trailing return, and for the routine that
   \ leaves by a branch it is the whole emission. CODE-BYTES has to put the four
   \ bytes back in the first case and NOT in the second, and the fixtures above
   \ are already the two shapes plus the one built to fool the distinction.
   s" a returning routine is its recorded body and the return after it" T-LABEL
   s" NTP-FIXTURE:PLAIN" CODE-BYTES
      s" NTP-FIXTURE:PLAIN" INSNS 1+ NBR:INSN-BYTES * T=
   s" NTP-FIXTURE:CALLS-BIG" CODE-BYTES
      s" NTP-FIXTURE:CALLS-BIG" INSNS 1+ NBR:INSN-BYTES * T=
   s" NTP-FIXTURE:BIG" CODE-BYTES
      s" NTP-FIXTURE:BIG" INSNS 1+ NBR:INSN-BYTES * T=

   s" a loop's back edge does not make its routine one that leaves by a branch"
   T-LABEL
   s" NTP-FIXTURE:LOOPY" CODE-BYTES
      s" NTP-FIXTURE:LOOPY" INSNS 1+ NBR:INSN-BYTES * T=

   s" but a routine that really leaves by one gets nothing added" T-LABEL
   s" NTP-FIXTURE:TAILED" CODE-BYTES
      s" NTP-FIXTURE:TAILED" INSNS NBR:INSN-BYTES * T=
   s" NTP-FIXTURE:TAILED" CODE-BYTES NBR:INSN-BYTES T=

   s" an empty routine records no length at all, and is not a word of no size"
   T-LABEL
   s" NTP-FIXTURE:EMPTY" INSNS 0 T=
   s" NTP-FIXTURE:EMPTY" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:EMPTY" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:EMPTY" CODE-BYTES NBR:INSN-BYTES T=

   s" a name nothing published is a refusal and not a quiet zero" T-LABEL
   [: s" NTP-FIXTURE:NO-SUCH-WORD" CALLS drop ;]
      E-CODEGEN-PROBE-SUBJECT TTHROWSQ
   [: s" NTP-FIXTURE:NO-SUCH-WORD" CODE-BYTES drop ;]
      E-CODEGEN-PROBE-SUBJECT TTHROWSQ ;

;using

;package

T-RESET
NTP-TEST:CASES
T-REPORT
