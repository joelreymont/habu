\ codegen-tail-probe.f - reading calls and exits from production emission.

require lib/prelude.f
require lib/test.f
require src/compiler/native/dict.f
require src/compiler/native/compiler.f
require tools/codegen-tail-probe.f

package NTP-FIXTURE

public

\ A small callee. Calls stay calls in the native compiler.
: TINY ( n -- n )
   1 + ;

\ A larger callee used by the tail-call cases below.
: BIG ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

\ A leaf: no call of any kind.
: PLAIN ( n -- n )
   dup * 3 + ;

\ A caller with two real call sites.
: CALLS-TWICE ( n -- n )
   TINY TINY 1 + ;

\ A caller with work after its call.
: CALLS-THEN ( n -- n )
   BIG 1 + ;

: OUTSIDE ( n -- n )
   abs ;

: TAILED ( n -- n )
   BIG ;

: EMPTY ( -- ) ;

: LOOPY ( ptr n n -- ) {: base:ptr len:n :}
   len 0 ?do i base i cells + ! loop ;

;package

package NTP-TEST

using NTAILPROBE

private

\ Where one instruction of the body branches to, or -1 for one that branches
\ nowhere. A conditional carries imm19 and an unconditional imm26, so the two
\ are decoded apart and answered the same way.
: BRANCH-TARGET ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   a u k INSN@ {: word:n :}
   k NBR:INSN-BYTES * {: at:n :}
   word NBR:COND? if at word NBR:COND-TARGET exit then
   word NBR:B? if at word NBR:B-TARGET exit then
   -1 ;

\ A back edge is a branch to an earlier byte of the same body, of EITHER kind.
\ The lowering decides which: a counted loop leaves through a forward
\ conditional and returns on one unconditional `b`, which is the shape
\ TAIL-BRANCH? is written against (it asks where the last instruction goes
\ rather than what it is, precisely because a back edge is a `b` too).
: BACKEDGE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u INSNS 0 ?do
      a u i BRANCH-TARGET {: target:n :}
      target 0 >= target i NBR:INSN-BYTES * < and if true unloop exit then
   loop
   false ;

public

: CASES ( -- )
   s" a routine that calls nothing reports no call" T-LABEL
   s" NTP-FIXTURE:PLAIN" CALLS 0 T=
   s" NTP-FIXTURE:PLAIN" LAST-CALL-IX -1 T=
   s" NTP-FIXTURE:PLAIN" AFTER-LAST-CALL -1 T=

   s" a body with two calls reports both" T-LABEL
   s" NTP-FIXTURE:CALLS-TWICE" CALLS 2 T=

   s" and a caller with work after its callee reports the call it makes" T-LABEL
   s" NTP-FIXTURE:CALLS-THEN" CALLS 1 T=
   s" NTP-FIXTURE:CALLS-THEN" LAST-CALL-IX 0 >= TTRUE
   0 NTP-FIXTURE:CALLS-THEN 50 T=

   s" what follows the last call is counted, and it is the epilogue" T-LABEL
   s" NTP-FIXTURE:CALLS-THEN" AFTER-LAST-CALL 0 > TTRUE

   s" every one that returns ends in the trailing return the record promises" T-LABEL
   s" NTP-FIXTURE:PLAIN" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:CALLS-TWICE" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:CALLS-THEN" TRAILER-RET? TTRUE
   s" NTP-FIXTURE:LOOPY" TRAILER-RET? TTRUE

   \ The guarded store call needs an epilogue after the loop's back edge.
   s" the counted loop has a back edge within its own body" T-LABEL
   s" NTP-FIXTURE:LOOPY" BACKEDGE? TTRUE

   s" the tail caller ends in a branch to its callee" T-LABEL
   s" NTP-FIXTURE:TAILED" LAST-BODY NBR:B? TTRUE

   s" but only one of the two goes anywhere outside itself" T-LABEL
   s" NTP-FIXTURE:PLAIN" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:CALLS-TWICE" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:CALLS-THEN" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:LOOPY" TAIL-BRANCH? TFALSE
   s" NTP-FIXTURE:TAILED" TAIL-BRANCH? TTRUE

   \ Bytes after this complete tail routine belong to the next definition.
   \ The full-span bit prevents TRAILER-RET? from reading EMPTY's return.
   s" NTP-FIXTURE:TAILED" TRAILER-RET? TFALSE
   s" the tail routine consists of one branch and makes no call" T-LABEL
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
   \ The recorded length excludes the trailing return, so for everything that
   \ returns it is the routine less that instruction, and for the routine that
   \ leaves by a branch it is the whole emission. CODE-BYTES has to put the four
   \ bytes back in the first case and NOT in the second, and the fixtures above
   \ are already the two shapes plus the one built to fool the distinction.
   s" a returning routine is its recorded body and the return after it" T-LABEL
   s" NTP-FIXTURE:PLAIN" CODE-BYTES
      s" NTP-FIXTURE:PLAIN" INSNS 1+ NBR:INSN-BYTES * T=
   s" NTP-FIXTURE:CALLS-THEN" CODE-BYTES
      s" NTP-FIXTURE:CALLS-THEN" INSNS 1+ NBR:INSN-BYTES * T=
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
   [: s" NTP-FIXTURE:TAILED" TRAILER drop ;]
      E-CODEGEN-PROBE-EXTENT TTHROWSQ

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
