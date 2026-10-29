\ codegen-tail-probe.f - which published routines end in a call followed by a
\ return, read off the emitted code: lowering can add, remove or turn a call into
\ a tail branch. The words to read are arguments. The dictionary record states
\ each extent, as for JITDUMP:JD: a legacy body length excludes the final slot, a
\ full span (CODE-SPAN:FULL?) includes the terminal branch or trap.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/string.f
require src/habu/code-span.f
require src/arch/arm64/disasm.f
require src/compiler/native/branch.f
require src/habu/code-bytes.f
require src/lib/fmt.f                        \ FMT:.INT

package NTAILPROBE

private

\ src/compiler/native/branch.f is the one reader of branch forms and their
\ displacements; this probe keeps no masks of its own.
NBR:INSN-BYTES constant INSN-BYTES


\ Little-endian, the order the target stores. CODE-BYTES:AT bounds the address
\ against the running image's code, so a record claiming a span the engine never
\ emitted is refused.
: W@ ( n -- n ) {: at:n :}
   at INSN-BYTES CODE-BYTES:AT drop {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

: BL? ( n -- bool )
   NBR:BL? ;

: B? ( n -- bool )
   NBR:B? ;

: RET? ( n -- bool )
   NBR:RET? ;

\ An unpublished name is refused: a row of zeroes would read as "no tail call",
\ the answer a lane acts on.
: REC ( ptr u8 n -- ptr n ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if drop E-CODEGEN-PROBE-SUBJECT throw then ;

: START ( ptr u8 n -- n )
   REC XREF-START ;

: LEN ( ptr u8 n -- n )
   REC XREF-LEN ;

\ The predicates answer false on a record shorter than one instruction instead of
\ throwing at a reader.
: READABLE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u LEN INSN-BYTES >= ;

public

: LAST-BODY ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u START  a u LEN +  INSN-BYTES -  W@ ;

: TRAILER ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u REC XREF-RAW-LEN CODE-SPAN:FULL? if E-CODEGEN-PROBE-EXTENT throw then
   a u START  a u LEN +  W@ ;

\ Read a final slot only when the record owns one. A complete body may be
\ followed immediately by another word's RET, which says nothing about it.
: TRAILER-RET? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u REC XREF-RAW-LEN CODE-SPAN:FULL? if false exit then
   a u TRAILER RET? ;

: INSNS ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LEN INSN-BYTES / ;

: INSN@ ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   a u START k INSN-BYTES * + W@ ;

: CALLS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   a u INSNS 0 ?do  a u i INSN@ BL? if 1+ then  loop ;

: LAST-CALL-IX ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   a u INSNS 0 ?do  a u i INSN@ BL? if drop i then  loop ;

\ The epilogue a tail form deletes: a lane reads this before and after its
\ change.
: AFTER-LAST-CALL ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LAST-CALL-IX {: k:n :}
   k 0 < if -1 exit then
   a u INSNS 1- k - ;

: LAST-BODY-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u START  a u INSNS 1- INSN-BYTES *  + ;

\ The target decides, not the opcode: a loop's back edge is a `b` too.
: TAIL-BRANCH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u READABLE? 0= if false exit then
   a u LAST-BODY B? 0= if false exit then
   a u LAST-BODY-AT  a u LAST-BODY  NBR:B-TARGET {: t:n :}
   t a u START < if true exit then
   t a u START a u LEN + > ;

\ The record owns the extent; branch analysis never supplies a missing length or
\ borrows adjacent bytes.
: CODE-BYTES ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u REC XREF-CODE-BYTES ;

: REPORT1 ( ptr u8 n -- ) {: a:ptr u:n :}
   a u type
   s"  bytes " type a u LEN FMT:.INT
   s"  calls " type a u CALLS FMT:.INT
   a u CALLS 0<> if s"  after-last-call " type a u AFTER-LAST-CALL FMT:.INT then
   a u TAIL-BRANCH? if s"  leaves by branch" type then
   a u TRAILER-RET? 0= if s"  no trailing return" type then
   cr ;

\ With the trailing return, so a report shows what moved rather than asserting
\ it.
: DUMP ( ptr u8 n -- ) {: a:ptr u:n :}
   s" --- " type a u type s"  bytes " type a u LEN FMT:.INT cr
   a u CODE-BYTES INSN-BYTES / 0 ?do
      a u START i INSN-BYTES * + W@ DIS1
   loop ;

;package
