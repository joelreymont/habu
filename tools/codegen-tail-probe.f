\ codegen-tail-probe.f - which published routines end in a call followed by a
\ return, read off the emitted code rather than off the source.
\ One concern: the last two instructions of a word's compiled code.
\
\ WHY IT READS THE CODE AND NOT THE BODY. A Habu body that ends in a call does
\ not necessarily emit one: both code generators copy a small callee into its
\ caller instead of calling it, so a source token is no evidence at all about
\ whether the emitted routine ends `bl; ret`. The question a tail-call lane may
\ act on is the machine's - is the instruction before the trailing return a
\ branch-with-link - and this tool is where that question is asked.
\
\ WHERE THE TRAILING RETURN IS. A word's recorded code length EXCLUDES its
\ trailing return: the engine stores the span its inliner may copy, and
\ src/habu/habu2.f's C-CALL-REQUIRE-RET-SLOT reads the return at entry+length to
\ confirm the record still ends in one. So the last instruction of the BODY is at
\ entry+length-4 and the return itself is at entry+length, and both are read here
\ the way that inliner reads them.
\
\ WHY IT DOES NOT WALK TO THE FIRST RETURN. tools/jitdump-core.f's JD does, which
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

package NTAILPROBE

private

\ The branch forms and the displacement they carry are read through
\ src/compiler/native/branch.f, which is the chain's one reader of that
\ arithmetic. A probe with masks of its own would be a fourth copy of it, and
\ the file that owns it exists because there were three.
NBR:INSN-BYTES constant INSN-BYTES

\ The return, which is the one encoding this file does have to name: it is a
\ whole word with no field in it, and no branch reader owns it.
$D65F03C0 constant RET-WORD

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
   RET-WORD = ;

\ The record of a word this tool was asked about. A name nothing published is a
\ refusal rather than a row of zeroes, because a silent miss would read as "this
\ routine does not end in a call" and that is the answer the lane acts on.
: REC ( ptr u8 n -- ptr a ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-CODEGEN-COMPARE-SUBJECT throw then ;

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
   a u START  a u LEN +  W@ ;

\ Whether that trailer really is a return. It is the record convention every
\ reader of a word's code depends on - src/habu/habu2.f reads it before copying
\ a callee - so a lane that changes how a routine leaves has to be able to say
\ what stands there now.
: TRAILER-RET? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TRAILER RET? ;

\ How many instructions the recorded body holds.
: INSNS ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LEN INSN-BYTES / ;

\ The instruction at an index of the body.
: INSN@ ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   a u START k INSN-BYTES * + W@ ;

\ How many calls the routine really makes. A body whose final call was copied
\ into it answers zero, which is the whole reason the count is taken from the
\ code.
: CALLS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   a u INSNS 0 ?do  a u i INSN@ BL? if 1+ then  loop ;

\ Where the last of them is, or -1 for a routine that makes none. The lane's
\ question is what stands between this instruction and the return.
: LAST-CALL-IX ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   a u INSNS 0 ?do  a u i INSN@ BL? if drop i then  loop ;

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
   a u LEN INSN-BYTES / 1+ 0 ?do
      a u START i INSN-BYTES * + W@ DIS1
   loop ;

;package
