\ codegen-tail-probe.f - which published routines end in a call followed by a
\ return, read off the emitted code rather than off the source.
\ One concern: the last two instructions of a word's compiled code.
\
\ WHY IT READS THE CODE AND NOT THE BODY. Source tokens are not machine
\ instructions: lowering can add, remove or turn a call into a tail branch. The
\ question a tail-call lane may act on is the machine's - is the instruction
\ before the trailing return a branch-with-link - and this tool is where that
\ question is asked.
\
\ Legacy body lengths exclude the final slot; explicit full spans include the
\ terminal branch or trap. The record bit decides whether a trailer exists.
\
\ WHY IT DOES NOT WALK TO THE FIRST RETURN. JITDUMP:JD (tools/jitdump-core.f) does, which
\ is right for a routine somebody is reading and wrong for this question twice
\ over: a routine with a guarded early exit stops at the first of its returns, and
\ a routine that ends in a tail branch has no trailing return to stop at. The
\ length in the dictionary record is the routine's own statement of its extent,
\ so it is what is walked here.
\
\ THE NAMES ARE ARGUMENTS. A caller hands the tool the words it wants read, so a
\ corpus lane names its own rows and nothing here has a list to keep in step with
\ a case file.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/arch/arm64/disasm.f
require src/compiler/native/branch.f
require src/compiler/native/codewalk.f

package NTAILPROBE

private

\ The branch forms and the displacement they carry are read through
\ src/compiler/native/branch.f, which is the chain's one reader of that
\ arithmetic. A probe with masks of its own would be a fourth copy of it, and
\ the file that owns it exists because there were three.
NBR:INSN-BYTES constant INSN-BYTES

\ The return. It used to be named here, because it is a whole word with no field
\ in it and the branch reader owned only displacements. A second walk over
\ emitted code now needs it too (tools/codegen-loop-inventory.f follows control
\ through a span), so it moved to src/compiler/native/branch.f beside the branch
\ forms rather than being spelled out twice.

variable CODE-AT

: CODE-PTR ( -- ptr u8 )
   CODE-AT 0 ptr-field @ ;

\ The instruction word at an address. The engine's code region is bytes, so the
\ four are read and assembled little-endian, which is the order the machine the
\ chain targets stores them in.
: W@ ( n -- n ) {: at:n :}
   at CODE-AT !
   CODE-PTR c@
   CODE-PTR 1 + c@ 8 lshift or
   CODE-PTR 2 + c@ 16 lshift or
   CODE-PTR 3 + c@ 24 lshift or ;

: BL? ( n -- bool )
   NBR:BL? ;

: B? ( n -- bool )
   NBR:B? ;

: RET? ( n -- bool )
   NBR:RET? ;

\ The record of a word this tool was asked about. A name nothing published is a
\ refusal rather than a row of zeroes, because a silent miss would read as "this
\ routine does not end in a call" and that is the answer the lane acts on.
: REC ( ptr u8 n -- ptr n ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-CODEGEN-PROBE-SUBJECT throw then ;

: START ( ptr u8 n -- n )
   REC XREF-START ;

: LEN ( ptr u8 n -- n )
   REC XREF-LEN ;

\ A record too short to hold one instruction has no last instruction to read.
\ Every predicate below asks this first and answers false, so the tool reports
\ what such a record is - not a tail call - instead of throwing at a reader.
: READABLE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u LEN INSN-BYTES >= ;

public

\ The instruction word at the end of the word's recorded body.
: LAST-BODY ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u START  a u LEN +  INSN-BYTES -  W@ ;

\ And the one after it, which is where the trailing return lives.
: TRAILER ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u REC XREF-RAW-LEN CODE-SPAN:FULL? if E-CODEGEN-PROBE-EXTENT throw then
   a u START  a u LEN +  W@ ;

\ Read a final slot only when the record owns one. A complete body may be
\ followed immediately by another word's RET, which says nothing about it.
: TRAILER-RET? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u REC XREF-RAW-LEN CODE-SPAN:FULL? if false exit then
   a u TRAILER RET? ;

\ How many instructions the recorded body holds.
: INSNS ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LEN INSN-BYTES / ;

\ The instruction at an index of the body.
: INSN@ ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   a u START k INSN-BYTES * + W@ ;

\ The engine's stack guards in the body (src/compiler/native/codewalk.f): each
\ is eleven instructions around one BL to the engine, and none of them is a
\ call the routine makes or work of its own. The readers below step over them.
: GUARDS ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u START a u LEN NWALK:SPAN-GUARDS ;

: GUARDED? ( ptr u8 n n -- bool ) {: a:ptr u:n k:n :}
   a u START a u LEN k NWALK:SPAN-GUARDED? ;

\ How many instructions are the routine's own: the body less its guards.
: OWN-INSNS ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u INSNS  a u GUARDS NWALK:GUARD-INSNS *  - ;

\ A call the routine makes: a BL of its own, not a guard's.
: CALL-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n k:n :}
   a u k INSN@ BL? 0= if false exit then
   a u k GUARDED? 0= ;

\ How many calls the routine really makes, counted from code rather than source.
: CALLS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   a u INSNS 0 ?do  a u i CALL-AT? if 1+ then  loop ;

\ Where the last of them is, or -1 for a routine that makes none. The lane's
\ question is what stands between this instruction and the return.
: LAST-CALL-IX ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   a u INSNS 0 ?do  a u i CALL-AT? if drop i then  loop ;

\ And how many instructions do stand there. A call in tail position is followed
\ by the routine's own teardown and nothing else, so this number is what a lane
\ reads before and after its change: it is the epilogue the tail form deletes.
: AFTER-LAST-CALL ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LAST-CALL-IX {: k:n :}
   k 0 < if -1 exit then
   a u INSNS 1- k - ;

\ Whether the routine leaves through a branch to somewhere that is not itself,
\ which is what the lowering makes of a tail call. The target decides it, not
\ the opcode: a loop's back edge is an unconditional branch too, and asking only
\ whether the last instruction is a `b` reports one as a tail branch.
: LAST-BODY-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u START  a u INSNS 1- INSN-BYTES *  + ;

: TAIL-BRANCH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u READABLE? 0= if false exit then
   a u LAST-BODY B? 0= if false exit then
   a u LAST-BODY-AT  a u LAST-BODY  NBR:B-TARGET {: t:n :}
   t a u START < if true exit then
   t a u START a u LEN + > ;

\ The shared dictionary contract owns extents; branch analysis above describes
\ control flow and never supplies a missing length or borrows adjacent bytes.
: CODE-BYTES ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u REC XREF-CODE-BYTES ;

\ One line per word: its recorded length, the calls it makes, what follows the
\ last of them, and whether it leaves through a branch.
: REPORT1 ( ptr u8 n -- ) {: a:ptr u:n :}
   a u type
   s"  bytes " type a u LEN .
   s"  calls " type a u CALLS .
   a u CALLS 0<> if s"  after-last-call " type a u AFTER-LAST-CALL . then
   a u TAIL-BRANCH? if s"  leaves by branch" type then
   a u TRAILER-RET? 0= if s"  no trailing return" type then
   cr ;

\ The whole of a word's compiled code with its trailing return, so a report can
\ show what moved rather than assert it.
: DUMP ( ptr u8 n -- ) {: a:ptr u:n :}
   s" --- " type a u type s"  bytes " type a u LEN . cr
   a u CODE-BYTES INSN-BYTES / 0 ?do
      a u START i INSN-BYTES * + W@ DIS1
   loop ;

;package
