\ x86-64-kernel-pure.f - the pure-op rows of the x86-64 kernel
\ (src/habu/kernel-x64.f PURE,) in the booted harness, cross-built for an
\ x86-64 peer. Each image is one case, and the peer that runs it must see its
\ status:
\
\    hb-x64-kernel-pure                 0  every integer case set of
\                                          test/prim-cases.f, and ?dup
\    hb-x64-kernel-pure-negative       21  the same, its first check expecting
\                                          a wrong answer: `x64k-pure: dup
\                                          overload 0 case 1` on fd 2
\    hb-x64-kernel-pure-store-armed    83  ! into a band cell
\    hb-x64-kernel-pure-cstore-armed   83  c! into a band cell
\    hb-x64-kernel-pure-addstore-armed 83  +! into a band cell
\
\ THE CASES ARE THE PARITY GATE'S. test/prim-cases.f is included into the
\ open image, so each `<overload> CASES <name>` line stages its checks against
\ the kernel row of that name; an overload is the same row. A case pushes its
\ inputs, calls the row, pops each answer against the case's column and
\ checks the data stack is back where the case found it. The checks outnumber
\ the statuses the harness numbers from FIRST-CASE, so each one dies through
\ the kernel's `die` row naming its subject and case (X64HARNESS:CHECK-RCX,).
\
\ Every image seals the friend latch first, so each store row's guard walks
\ its whole span test. A pointer column is a byte offset into the fixture, the
\ harness's scratch cells in DATA; a flag column's 1 is the all-ones cell.
\ The host checks each image's ELF header; running them is the peer's.
require test/x86-64-boot-harness.f
require lib/errors.f                    \ E-DIV-ZERO, the dividing rows' refusal
require lib/fmt.f
require lib/string.f

package X64K-PURE
using X64ASM
using X64CODE
using X64RT

\ What test/prim-cases.f's header asks an includer for, beside the shapes.
X64HARNESS:MAX-CELL constant MAX-N
X64HARNESS:MIN-CELL constant MIN-N
3 constant BUMP                         \ the addend the `+!` scenario applies

1 constant SETUP-RC                     \ a malformed case set ends the build
$40 constant SUBJ-CAP

SUBJ-CAP BUFFER: SUBJ-BUF
variable SUBJ-U
variable OVERLOAD
variable CASE-N
\ A refusing case's operands: the routine `catch` runs stages them, and a
\ quotation captures nothing.
variable DV-A
variable DV-B

: DATA-REG ( -- r64 ) ENGINE-GPR:X64-RBASE >R64 ;
: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;
: RCX! ( n -- ) {: v:n :}  RCX v >IMM64 ASM-SINK ENC-MOV-RI64 ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: AT, ( n -- ) X64HARNESS:PUSH-SCRATCH, ;
: CALL ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;

: SEAL, ( -- ) FRIEND-ARENA-LEN FRIEND-LATCH-CELL X64HARNESS:CELL!, ;

\ ---- the subject and its case text -------------------------------------------
: SUBJ$ ( -- ptr u8 n ) SUBJ-BUF SUBJ-U @ ;

: SUBJ! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if s" x64k-pure: CASES needs a primitive name" SETUP-RC die then
   u SUBJ-CAP > if s" x64k-pure: primitive name too long" SETUP-RC die then
   a SUBJ-BUF u BYTE-COPY
   u SUBJ-U ! ;

\ `x64k-pure: <name> overload <n> case <k>`, the text a failing check writes,
\ in the string builder: PUSH-TEXT, copies it into the image.
: CASE$ ( -- ptr u8 n )
   SB-RESET
   s" x64k-pure: " SB-APPEND  SUBJ$ SB-APPEND
   s"  overload " SB-APPEND  OVERLOAD @ FMT:SB-INT
   s"  case " SB-APPEND  CASE-N @ FMT:SB-INT
   SB$ ;

: CASE+ ( -- ) CASE-N @ 1+ CASE-N ! ;

\ ---- the checks --------------------------------------------------------------
: CHECK, ( -- ) CASE$ X64HARNESS:CHECK-RCX, ;

\ Pop a cell and check it is n.
: WANT ( n -- ) {: v:n :}  0 G-POP  v RCX!  CHECK, ;

\ Pop an address and check it lies n bytes into the fixture.
: WANT-AT ( n -- ) {: off:n :}  off AT,  1 G-POP  0 G-POP  CHECK, ;

\ A case's 0/1 flag column as the cell a flag is.
: FLAG ( n -- n ) negate ;

\ Check the case left the data stack where it found it: empty.
: SETTLED ( -- )
   RAX DSP ASM-SINK ENC-MOV-RR
   RAX DATA-REG STACK-ABI:BASE-CELL MEM-OFF ASM-SINK ENC-SUB-RM
   0 RCX!  CHECK, ;

\ Pop the window's cells against the digit spelling, top cell first.
: DIGITS ( n -- )
   begin dup 0<> while  dup 10 mod WANT  10 /  repeat drop ;

: ROW ( -- ) SUBJ$ CALL ;

\ ---- the memory scenarios ----------------------------------------------------
\ Each passes the case value through a store row and reads it back through its
\ fetch row, at the fixture's first cell, as prim-parity.f MEM-PRIM does; count
\ also answers the address past the byte.
: STORED ( n -- ) {: v:n :}  v N, 0 AT, s" !" CALL ;

: MEM-SCENARIO ( n -- ) {: v:n :}
   SUBJ$ s" !" STR= if  v STORED  0 AT, s" @" CALL  exit then
   SUBJ$ s" c!" STR= if  v N, 0 AT, s" c!" CALL  0 AT, s" c@" CALL  exit then
   SUBJ$ s" +!" STR= if
      v STORED  BUMP N, 0 AT, s" +!" CALL  0 AT, s" @" CALL  exit then
   SUBJ$ s" count" STR= if
      v N, 0 AT, s" c!" CALL  0 AT, s" count" CALL
      2 G-POP  1 WANT-AT  2 G-PUSH  exit then
   SUBJ$ s" byte-view" STR= if
      v STORED  0 AT, s" byte-view" CALL s" c@" CALL  exit then
   SUBJ$ s" cell-view" STR= if
      v N, 0 AT, s" cell-view" CALL s" !" CALL
      0 AT, s" cell-view" CALL s" @" CALL  exit then
   s" x64k-pure: no memory scenario for " type SUBJ$ type cr
   s" x64k-pure: unwired memory row" SETUP-RC die ;

public

\ ---- test/prim-cases.f's vocabulary ------------------------------------------
: CASES ( n -- )
   OVERLOAD !
   parse-name SUBJ!
   0 CASE-N ! ;

: ;CASES ( -- )
   CASE-N @ 0= if
      s" x64k-pure: empty case set for " type SUBJ$ type cr
      s" x64k-pure: empty case set" SETUP-RC die
   then
   0 SUBJ-U ! ;

: SHUF ( n -- ) {: want:n :}
   CASE+  1 N, 2 N, 3 N, 4 N,  ROW  want DIGITS  SETTLED ;

: NN-N ( n n n -- ) {: a:n b:n want:n :}
   CASE+  a N, b N,  ROW  want WANT  SETTLED ;

: NN-F ( n n n -- ) {: a:n b:n want:n :}
   CASE+  a N, b N,  ROW  want FLAG WANT  SETTLED ;

: FF-F ( n n n -- ) {: a:n b:n want:n :}
   CASE+  a FLAG N, b FLAG N,  ROW  want FLAG WANT  SETTLED ;

: N-N ( n n -- ) {: a:n want:n :}
   CASE+  a N,  ROW  want WANT  SETTLED ;

: N-F ( n n -- ) {: a:n want:n :}
   CASE+  a N,  ROW  want FLAG WANT  SETTLED ;

: NN-NN ( n n n n -- ) {: a:n b:n w1:n w2:n :}
   CASE+  a N, b N,  ROW  w2 WANT  w1 WANT  SETTLED ;

\ The row runs under `catch` in a routine that stages its operands, so a throw
\ leaves the code alone on the stack the case found empty.
: NN-THROWS ( n n n -- ) {: a:n b:n want:n :}
   CASE+  a DV-A !  b DV-B !
   [: DV-A @ N,  DV-B @ N,  ROW ;] X64HARNESS:ROUTINE,
   0 X64HARNESS:PUSH-LABEL,  s" catch" CALL
   want WANT  SETTLED ;

: MEM ( n n -- ) {: v:n want:n :}
   CASE+  v MEM-SCENARIO  want WANT  SETTLED ;

: PN-P ( n n n -- ) {: a:n b:n want:n :}
   CASE+  a AT, b N,  ROW  want WANT-AT  SETTLED ;

: NP-P ( n n n -- ) {: a:n b:n want:n :}
   CASE+  a N, b AT,  ROW  want WANT-AT  SETTLED ;

: PP-N ( n n n -- ) {: a:n b:n want:n :}
   CASE+  a AT, b AT,  ROW  want WANT  SETTLED ;

: P-P ( n n -- ) {: a:n want:n :}
   CASE+  a AT,  ROW  want WANT-AT  SETTLED ;

: PP-F ( n n n -- ) {: a:n b:n want:n :}
   CASE+  a AT, b AT,  ROW  want FLAG WANT  SETTLED ;

private

\ ---- the images --------------------------------------------------------------
\ ?dup has no case set: checked Habu cannot name it, so prim-cases.f cannot
\ hold one. Zero stays alone; anything else is doubled.
: QDUP-CASES ( -- )
   s" ?dup" SUBJ!  0 OVERLOAD !  0 CASE-N !
   CASE+  0 N,  ROW  0 WANT  SETTLED
   CASE+  5 N,  ROW  5 WANT  5 WANT  SETTLED ;

: OPEN-IMAGE ( bool -- ) X64HARNESS:BOOT-OPEN,  SEAL, ;

: CLOSE-IMAGE ( ptr u8 n -- )
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   X64HARNESS:BOOT-CLOSE, ;

\ A store row whose address is the band cell TIER-PROV:N-CELL: its guard exits
\ 83 before the store.
: ARMED ( ptr u8 n ptr u8 n -- ) {: row:ptr rowu:n path:ptr pathu:n :}
   false OPEN-IMAGE
   7 N,  TIER-PROV:N-CELL X64HARNESS:PUSH-DATA,  row rowu CALL
   path pathu CLOSE-IMAGE ;

T-RESET
X64HARNESS:INIT

false OPEN-IMAGE
s" test/prim-cases.f" included
QDUP-CASES
s" hb-x64-kernel-pure" TMP-PATH CLOSE-IMAGE

true OPEN-IMAGE
s" test/prim-cases.f" included
QDUP-CASES
s" hb-x64-kernel-pure-negative" TMP-PATH CLOSE-IMAGE

s" !" s" hb-x64-kernel-pure-store-armed" TMP-PATH ARMED
s" c!" s" hb-x64-kernel-pure-cstore-armed" TMP-PATH ARMED
s" +!" s" hb-x64-kernel-pure-addstore-armed" TMP-PATH ARMED

X64HARNESS:DISPOSE
T-REPORT

;using
;using
;using
;package
