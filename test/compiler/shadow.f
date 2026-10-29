\ shadow.f - a second target compiled beside the engine's own through NSHADOW
\ (src/compiler/native/shadow.f), read from the host's side. The words below
\ are compiled by the tier-1 driver with an x86-64 shadow open, from one frozen
\ HIR module: host words run unchanged, and the map holds an x86-64 routine per
\ committed record publication, measured from no slot.

require src/lib/test.f
require src/lib/errors.f
require src/habu/xref.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/target/model.f
require src/compiler/target/native-binding.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
require src/compiler/session/emission.f
require src/compiler/native/publish.f
require src/compiler/native/shadow.f
require src/compiler/native/x64ir.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f
require src/habu/kernel-hir-x64.f

package SHADOW-TEST
private

variable REC-LEAF
variable REC-CALLER
variable REC-CONST
variable REC-QUOT
variable REC-SEVEN
variable REC-PX
variable BAD-RC                      \ what refusing the definition threw
variable BAD-NDICT                   \ the record count before it was tried
variable BAD-MOVED                   \ and how far trying it moved the count
variable BAD-RECS                    \ what the refusing shadow had filed
variable BAD-EMS

: OPEN-X64 ( -- )
   X64ABI:BINDING NSHADOW:OPEN ;

: OPEN-A64 ( -- )
   NABI:BINDING NSHADOW:OPEN ;

\ Definers made before the open file only their children; PB's empty clause
\ replaces PA's.
: SH-PA ( -- ) create 5 , does> ( -- n ) @ 100 + ;
: SH-PB ( -- ) SH-PA does> ( -- ptr n ) ;

OPEN-X64
1 set-tier
ndict@ REC-LEAF !
: SH-LEAF ( n n -- n ) + ;
ndict@ REC-CALLER !
: SH-CALLER ( n -- n ) dup SH-LEAF 1 + ;
ndict@ REC-CONST !
: SH-CONST ( n -- ) create , does> ( -- n ) @ ;
ndict@ REC-QUOT !
: SH-QUOT ( -- [ n -- n ] ) [: 1 + ;] ;
0 set-tier
\ The definer's generated child has its own fixed shadow publication.
ndict@ REC-SEVEN !
7 SH-CONST SH-SEVEN
ndict@ REC-PX !
SH-PB SH-PX

\ A leaf is one function ending in `ret` with no rows, since an x86-64 span is
\ exact. A call is a row at the `call` naming the callee's host entry, its rel32
\ zero for the linker. A `does>` definer is two records over one routine: the
\ companion enters at the clause function, whose offset the definer's literal
\ holds as an unplaced `codeaddr` does.
: ROW-OF ( n -- n ) {: idx:n :}
   -1
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:RECORD@ idx = if drop i leave then
   loop ;

: EM-OF ( n -- n )
   ROW-OF NSHADOW:EMISSION@ ;

: BYTE@ ( n n -- n ) {: e:n off:n :}
   e NSHADOW:BYTES off + c@ ;

: LE64@ ( n n -- n ) {: e:n off:n :}
   0  8 0 ?do  8 lshift  e  off 7 + i -  BYTE@ or  loop ;

\ The immediate of the `mov r64, imm64` address row k of emission e loads.
: IMM@ ( n n -- n ) {: e:n k:n :}
   e  e k NSHADOW:ADDR-SITE@ X64ASM:MOV-RI64-IMM-OFF +  LE64@ ;

: HOST-ENTRY ( n -- n )
   XREF-REC XREF-START ;

: HOST-CASE ( -- )
   s" every word compiled with the shadow open runs on the host as before" T-LABEL
   2 3 SH-LEAF 5 T=
   4 SH-CALLER 9 T=
   SH-SEVEN 7 T=
   SH-PX @ 5 T=
   5 SH-QUOT execute 6 T= ;

: FILED-CASE ( -- )
   s" each record publication committed is filed once, a definer's companion beside it, and nothing else was" T-LABEL
   NSHADOW:RECORDS 7 T=
   NSHADOW:EMISSIONS 6 T=
   REC-LEAF @ ROW-OF 0 T=
   REC-CALLER @ ROW-OF 1 T=
   REC-CONST @ ROW-OF 2 T=
   REC-CONST @ 1+ ROW-OF 3 T=
   REC-QUOT @ ROW-OF 4 T=
   REC-SEVEN @ ROW-OF 5 T=
   REC-PX @ ROW-OF 6 T= ;

: LEAF-CASE ( -- )
   s" a leaf is one x86-64 function entered at its start and ending in ret, with no rows and no trailing return split off" T-LABEL
   REC-LEAF @ EM-OF {: e:n :}
   REC-LEAF @ ROW-OF NSHADOW:ENTRY@ 0 T=
   e  e NSHADOW:SIZE 1-  BYTE@ $C3 T=
   e NSHADOW:RET-BYTES 0 T=
   e NSHADOW:FUNCTIONS 1 T=
   e 0 NSHADOW:FUNCTION-OFFSET@ 0 T=
   e NSHADOW:CALL-SITES 0 T=
   e NSHADOW:ADDR-SITES 0 T= ;

: CALL-CASE ( -- )
   s" a call is a row at the call itself naming the callee's host entry, and its rel32 is left zero for the linker" T-LABEL
   REC-CALLER @ EM-OF {: e:n :}
   e NSHADOW:CALL-SITES 1 T=
   e 0 NSHADOW:CALL-KIND@ NEMIT:CALL T=
   e 0 NSHADOW:CALL-TARGET@  REC-LEAF @ HOST-ENTRY  T=
   e 0 NSHADOW:CALL-SITE@ {: at:n :}
   e at BYTE@ $E8 T=
   e at 1+ BYTE@  e at 2 + BYTE@ or  e at 3 + BYTE@ or  e at 4 + BYTE@ or  0 T= ;

: DOES-CASE ( -- )
   s" a does> definer is two records over one routine: the companion enters at the clause function, whose offset the definer's own literal holds" T-LABEL
   REC-CONST @ ROW-OF {: r:n :}
   r NSHADOW:EMISSION@ {: e:n :}
   r 1+ NSHADOW:EMISSION@ e T=
   r NSHADOW:ENTRY@ 0 T=
   e NSHADOW:FUNCTIONS 2 T=
   e 1 NSHADOW:FUNCTION-OFFSET@ {: clause:n :}
   clause 0 > TTRUE
   r 1+ NSHADOW:ENTRY@ clause T=
   -1
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-CODE = if drop i leave then
   loop {: k:n :}
   k 0 >= TTRUE
   e k IMM@ clause T=
   s" the child's slot reports one tail call to the clause; an empty replacement clears it" T-LABEL
   REC-SEVEN @ EM-OF NSHADOW:CALL-SITES 1 T=
   REC-SEVEN @ EM-OF 0 NSHADOW:CALL-KIND@ NEMIT:TAIL T=
   REC-SEVEN @ EM-OF NSHADOW:ROUTINE? TTRUE
   REC-PX @ EM-OF NSHADOW:CALL-SITES 0 T=
   REC-PX @ EM-OF NSHADOW:ROUTINE? TTRUE ;

: QUOT-CASE ( -- )
   s" a quotation's address is a code row whose immediate is that function's offset in the emission" T-LABEL
   REC-QUOT @ EM-OF {: e:n :}
   e NSHADOW:FUNCTIONS 2 T=
   e NSHADOW:ADDR-SITES 1 T=
   e 0 NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-CODE T=
   e 0 IMM@  e 1 NSHADOW:FUNCTION-OFFSET@  T= ;

: ROW-CASE ( -- )
   s" the map refuses a row or an emission it does not hold" T-LABEL
   [: NSHADOW:RECORDS NSHADOW:RECORD@ drop ;] E-NSHADOW-ROW TTHROWSQ
   [: -1 NSHADOW:SIZE drop ;] E-NSHADOW-ROW TTHROWSQ
   [: NSHADOW:EMISSIONS NSHADOW:BYTES drop ;] E-NSHADOW-ROW TTHROWSQ
   [: REC-LEAF @ EM-OF 1 NSHADOW:CALL-SITE@ drop ;] E-NSHADOW-ROW TTHROWSQ
   [: OPEN-X64 ;] E-NSHADOW-STATE TTHROWSQ ;

\ Refused: an emission another machine's backend sealed before its window
\ (X64KHIR shows one); a backend with no unplaced emission (nothing published,
\ the engine compiles on); a native shadow for another system's binding, or for
\ an unplaced emission under the engine's own. A taken emission no publication
\ claims files nothing.
\ X64KHIR compiles a kernel row through the x86-64 rows and runs `use` while its
\ unplaced emission stands sealed in NEMIT, which is the only way this host ever
\ holds one outside a shadow's own context.
: ADD-STAGE ( -- )
   0 X64KHIR:ARG  1 X64KHIR:ARG  HIR-OPCODE:ADD X64KHIR:OP2  X64KHIR:RESULT ;

: SEALED-X64 ( [ NART:emission -- ] -- )
   {: use :}
   s" shadow-add" 2 1 [: ADD-STAGE ;] use X64KHIR:COMPILE ;

: FOREIGN-USE ( NART:emission -- )
   drop
   cp@ ndict@ {: cp0:n nd0:n :}
   [: NPUB:PUBLISH-PENDING ;] E-NPUB-TARGET TTHROWSQ
   cp@ cp0 - 0 T=
   ndict@ nd0 - 0 T= ;

: UNCLAIMED-USE ( NART:emission -- )
   drop
   NSHADOW:RECORDS NSHADOW:EMISSIONS {: recs:n ems:n :}
   NSHADOW:TAKE
   NSHADOW:ABANDON
   ndict@ NSHADOW:PUBLISH
   [: 0 NSHADOW:TAKE-DOES ;] E-NSHADOW-ROW TTHROWSQ
   ndict@ NSHADOW:PUBLISH
   [: 1 NSHADOW:TAKE-DOES ;] E-NSHADOW-ROW TTHROWSQ
   ndict@ NSHADOW:PUBLISH
   NSHADOW:RECORDS recs T=
   NSHADOW:EMISSIONS ems T= ;

: WRONG-TARGET-USE ( NART:emission -- )
   drop
   [: NSHADOW:TAKE ;] E-NSHADOW-TARGET TTHROWSQ ;

: CLOSED-USE ( NART:emission -- )
   drop
   [: NSHADOW:TAKE ;] E-NSHADOW-STATE TTHROWSQ ;

: FOREIGN-CASE ( -- )
   s" publication refuses an emission sealed for another machine before its window, and neither CP nor NDICT moves" T-LABEL
   [: FOREIGN-USE ;] SEALED-X64 ;

: UNCLAIMED-CASE ( -- )
   s" an emission taken and abandoned, or refused for a does> clause it lacks or that starts where it enters, files nothing" T-LABEL
   [: UNCLAIMED-USE ;] SEALED-X64 ;

: WRONG-TARGET-CASE ( -- )
   s" a shadow refuses an emission for another machine than its binding names" T-LABEL
   NSHADOW:CLOSE
   OPEN-A64
   [: WRONG-TARGET-USE ;] SEALED-X64
   NSHADOW:CLOSE ;

public

: RUN-OPEN ( -- )
   T-RESET
   HOST-CASE
   FILED-CASE
   LEAF-CASE
   CALL-CASE
   DOES-CASE
   QUOT-CASE
   ROW-CASE
   FOREIGN-CASE
   UNCLAIMED-CASE
   WRONG-TARGET-CASE ;

;package

SHADOW-TEST:RUN-OPEN

\ Values their fixed register cannot be kept for: idiv and shl pin rax and rcx
\ (src/compiler/native/x64ir.f DEF-IDIV, DEF-SHIFT-CL). A pinned value live
\ across a call or another divide goes to the frame (regalloc.f MB-PIN).
\ SH-QUOT-STORE, a local holding the quotient over the call `!` makes, stopped
\ the engine's window compile at MSEEN-ALLOC. The dividend and the count are
\ held over the call by the copy the selector made for the form, coalesced into
\ them.
package SHADOW-TEST
private

variable PIN-CELL
variable REC-PIN                     \ the first record compiled below

OPEN-X64
ndict@ REC-PIN !
1 set-tier
: PIN-NOP ( -- ) ;
: SH-QUOT-STORE ( n n -- n ) / {: k:n :} 1 PIN-CELL ! k ;
: SH-QUOT-CALL ( n n -- n ) / {: k:n :} PIN-NOP k ;
: SH-QUOT-DIV ( n n n n -- n )
   {: a:n b:n c:n d:n :}
   a b / c d / + ;
: SH-DIVIDEND-CALL ( n n -- n )
   {: a:n b:n :}
   PIN-NOP a b / ;
: SH-COUNT-CALL ( n n -- n )
   {: a:n b:n :}
   PIN-NOP a b lshift ;
0 set-tier

: PINNED-HOST-CASE ( -- )
   s" a word holding a value its fixed register cannot be kept for runs on the host as before" T-LABEL
   0 PIN-CELL !
   7 2 SH-QUOT-STORE 3 T=
   PIN-CELL @ 1 T=
   -7 2 SH-QUOT-CALL -3 T=
   7 2 9 3 SH-QUOT-DIV 6 T=
   7 2 SH-DIVIDEND-CALL 3 T=
   1 4 SH-COUNT-CALL 16 T= ;

: PINNED-FILED-CASE ( -- )
   s" and the shadow files an x86-64 routine for each, the first record of the section first" T-LABEL
   NSHADOW:RECORDS 6 T=
   6 0 ?do REC-PIN @ i + ROW-OF i T= loop ;

public

: RUN-PINNED ( -- )
   PINNED-HOST-CASE
   PINNED-FILED-CASE ;

;package

SHADOW-TEST:RUN-PINNED
NSHADOW:CLOSE

\ A load run longer than the nine-register pool: an x86-64 routine loads its
\ register arguments in one run at entry (src/compiler/native/select-x64.f
\ OPEN-DARGS), stored to the frame after the whole run (regalloc.f MB-ANCHOR); a
\ run the registers cannot hold is divided, not refused (E-A64RA-POOL).
\ SH-TEN-READ reads ten arguments and calls nothing; SH-TEN-CROSS passes ten to
\ a call and reads them after it. SH-FIELD-ADD is TYPE-FIELD-OWNER:ADD
\ (src/core/type-family.f): twelve locals, ten of them read, over two calls.
package SHADOW-TEST
private

variable RUN-CELL
variable OVERLAP-CELL
variable REC-RUN                     \ the first record compiled below

OPEN-X64
ndict@ REC-RUN !
1 set-tier
: SH-TEN-READ ( n n n n n n n n n n -- n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n i:n j:n :}
   a b - c + d - e + f - g + h - i + j - ;
: SH-TEN-SINK ( n n n n n n n n n n -- ) SH-TEN-READ RUN-CELL ! ;
: SH-TEN-CROSS ( n n n n n n n n n n -- n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n i:n j:n :}
   a b c d e f g h i j SH-TEN-SINK
   a b + c - d + e - f + g - h + i - j + ;
: FIELD-LAYOUT ( n n n n n n n n -- ) - - - - - - - RUN-CELL ! ;
: FIELD-OVERLAP? ( n n n n n n -- bool ) + + + + + dup OVERLAP-CELL ! 0< ;
: SH-FIELD-ADD ( n n n ptr u8 n n n n n n n n -- n )
   {: tx:n fam:n var:n na:ptr nu:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   fam sch slot cellsn boff bytesn al flags FIELD-LAYOUT
   fam var slot cellsn boff bytesn FIELD-OVERLAP? drop
   tx ;
0 set-tier

: RUN-HOST-CASE ( -- )
   s" a word reading more arguments than the x86-64 pool runs on the host as before" T-LABEL
   1 2 4 8 16 32 64 128 256 512 SH-TEN-READ -341 T=
   0 RUN-CELL !
   1 2 4 8 16 32 64 128 256 512 SH-TEN-CROSS 343 T=
   RUN-CELL @ -341 T=
   0 RUN-CELL !  0 OVERLAP-CELL !
   7 1 2 s" na" 4 8 16 32 64 128 256 SH-FIELD-ADD 7 T=
   RUN-CELL @ -171 T=
   OVERLAP-CELL @ 123 T= ;

: RUN-FILED-CASE ( -- )
   s" and the shadow files an x86-64 routine for each, the first record of the section first" T-LABEL
   NSHADOW:RECORDS 6 T=
   6 0 ?do REC-RUN @ i + ROW-OF i T= loop ;

public

: RUN-LOAD-RUN ( -- )
   RUN-HOST-CASE
   RUN-FILED-CASE ;

;package

SHADOW-TEST:RUN-LOAD-RUN
NSHADOW:CLOSE

\ A reload in front of a division: idiv takes its dividend in rax and writes
\ rdx, so a divisor read there may have neither. SH-DIV-RELOAD's divisor lives
\ over the call `!` makes and is reloaded in front of the idiv while seven sums
\ hold the other seven registers. A reload is never put away, so a sum goes to
\ the frame; putting the reload away repeats every fixpoint turn until the
\ modules hold every builder slot (E-IR-BUILD-SLOTS). It is RD-FIND
\ (src/compiler/ir/arena.f) reduced.
package SHADOW-TEST
private

variable DIV-CELL
variable REC-DIV                     \ the record compiled below

OPEN-X64
ndict@ REC-DIV !
1 set-tier
: SH-DIV-RELOAD ( n n -- n )
   {: x:n k:n :}
   x DIV-CELL !
   x 1+ {: a:n :}  x 2 + {: b:n :}  x 3 + {: c:n :}  x 4 + {: d:n :}
   x 5 + {: e:n :}  x 6 + {: f:n :}  x 7 + {: g:n :}
   x k / a + b + c + d + e + f + g + ;
0 set-tier

public

: RUN-DIV-RELOAD ( -- )
   s" a divisor reloaded in front of idiv runs on the host as before" T-LABEL
   0 DIV-CELL !
   100 7 SH-DIV-RELOAD 742 T=
   DIV-CELL @ 100 T=
   s" and the shadow files its x86-64 routine" T-LABEL
   NSHADOW:RECORDS 1 T=
   REC-DIV @ ROW-OF 0 T= ;

;package

SHADOW-TEST:RUN-DIV-RELOAD
NSHADOW:CLOSE

\ A frame lane a dominated block forwards: the spill pass threads the frame's
\ memory order through block arguments, and a one-successor branch forwards a
\ lane in every block the value dominates, not only the lane's own block.
\ SH-LANE-FIND's second turn puts `used` away in a block reached that way;
\ following only a block's own terminator mints a second lane and leaves the
\ first read by nobody (E-A64RAV-ORDER; spill.f F-READS!). It is RD-FIND
\ (src/compiler/ir/arena.f) less its first guard.
package SHADOW-TEST
private

create LANE-TABLE 8 cells allot
PTR-VARIABLE LANE-BASE
LANE-TABLE LANE-BASE !
variable LANE-MASK
1 cells constant LANE-USED
2 cells constant LANE-DATA
variable REC-LANE                    \ the first record compiled below

OPEN-X64
ndict@ REC-LANE !
1 set-tier
: LANE-SLOT@ ( ptr u8 n -- n )
   {: base:ptr k:n :}
   base k cells + CELL-VIEW @ ;
: SH-LANE-FIND ( n n n n n -- n )
   {: t:n first:n stride:n count:n want:n :}
   LANE-BASE @ {: base:ptr :}
   base t LANE-MASK @ and + {: d:ptr :}
   first 0 < stride 1 < or if E-IR-ARENA-BOUND throw then
   d LANE-USED + CELL-VIEW @ {: used:n :}
   first used >= if E-IR-ARENA-BOUND throw then
   count 1- {: steps:n :}
   steps 0 > if used 1- first - steps / stride < if E-IR-ARENA-BOUND throw then then
   base d LANE-DATA + CELL-VIEW @ + {: dbase:ptr :}
   count 0 ?do
      dbase first i stride * + LANE-SLOT@ want = if i unloop exit then
   loop
   -1 ;
0 set-tier

\ Cell 1 counts the four data cells, cell 2 is their byte offset from the base.
: LANE-FILL ( -- )
   0 LANE-MASK !
   4 LANE-TABLE LANE-USED + !
   3 cells LANE-TABLE LANE-DATA + !
   5 0 ?do i 1+ 10 * LANE-TABLE 3 i + cells + ! loop ;

public

: RUN-LANE ( -- )
   LANE-FILL
   s" a word whose frame lane a dominated block forwards runs on the host as before" T-LABEL
   0 0 1 4 30 SH-LANE-FIND 2 T=
   0 1 1 3 40 SH-LANE-FIND 2 T=
   0 0 1 4 99 SH-LANE-FIND -1 T=
   s" and the shadow files an x86-64 routine for each" T-LABEL
   NSHADOW:RECORDS 2 T=
   2 0 ?do REC-LANE @ i + ROW-OF i T= loop ;

;package

SHADOW-TEST:RUN-LANE
NSHADOW:CLOSE

\ A call's results taken back around frame operations: the loads that take back
\ a call's cells are one run in place order, and the run admits any operation
\ that neither touches the data stack nor branches, as the stores in front of a
\ call do (regalloc-verify.f DLOAD-RUN, DSTORE-RUN); ending it at anything but a
\ frame store refused the routine (E-A64RAV-CALL). SH-PICK takes fourteen back
\ from either of two calls, with frame reloads and stores between. It is ROUTINE
\ (src/arch/x86-64/passes.f) reduced.
package SHADOW-TEST
private

variable PICK-ARM
variable REC-PICK                    \ the first record compiled below

OPEN-X64
ndict@ REC-PICK !
1 set-tier
: SH-FOURTEEN ( n n n n -- n n n n n n n n n n n n n n )
   {: a:n b:n c:n d:n :}
   a b c d a b c d a b c d a b ;
: SH-FOURTEEN-TOO ( n n n n -- n n n n n n n n n n n n n n ) SH-FOURTEEN ;
: SH-PICK ( -- n n n n n n n n n n n n n n )
   PICK-ARM @ 0<> if 1 2 3 4 SH-FOURTEEN else 5 6 7 8 SH-FOURTEEN-TOO then ;
0 set-tier

\ The fourteen cells as the digits of one number, the deepest leading.
: PICK-DIGITS ( n n n n n n n n n n n n n n -- n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n i:n j:n k:n l:n m:n o:n :}
   a 10 * b + 10 * c + 10 * d + 10 * e + 10 * f + 10 * g + 10 * h +
   10 * i + 10 * j + 10 * k + 10 * l + 10 * m + 10 * o + ;

public

: RUN-PICK ( -- )
   s" a word taking fourteen cells back from either of two calls runs on the host as before" T-LABEL
   1 PICK-ARM !
   SH-PICK PICK-DIGITS 12341234123412 T=
   0 PICK-ARM !
   SH-PICK PICK-DIGITS 56785678567856 T=
   s" and the shadow files an x86-64 routine for each" T-LABEL
   NSHADOW:RECORDS 3 T=
   3 0 ?do REC-PICK @ i + ROW-OF i T= loop ;

;package

SHADOW-TEST:RUN-PICK
NSHADOW:CLOSE

\ A value an edge passes on keeps its slot: the x86-64 rows of
\ test/compiler/native-loop.f SLOT-CASE, whose comment states the shapes;
\ select-x64.f DSTORED holds the rule. SH-SLOT-LETTER adds a call's result held
\ in a local across the backedge of a loop that leaves early with a value.
package SHADOW-TEST
private

4 constant SLOT-CAP
create SLOT-KEY SLOT-CAP cells allot
variable SLOT-N
variable REC-SLOT                    \ the first record compiled below

OPEN-X64
ndict@ REC-SLOT !
1 set-tier
: SH-SLOT-MIX ( n n -- n ) swap 10 * + ;
: SH-SLOT-ONE ( n -- n ) 1 + ;
: SH-SLOT-QDO ( n n -- n ) {: a:n k:n :} k 0 ?do loop a k SH-SLOT-MIX ;
: SH-SLOT-DO ( n n -- n ) {: a:n k:n :} k 0 do loop a k SH-SLOT-MIX ;
: SH-SLOT-JOIN ( n n bool -- n ) {: a:n k:n f:bool :}
   f if k else 7 then drop a k SH-SLOT-MIX ;
: SH-SLOT-BACK ( n n -- n ) {: a:n k:n :}
   a k SH-SLOT-MIX {: m:n :} k 0 ?do loop m SH-SLOT-ONE ;
: SH-SLOT-EXIT ( n n -- n n ) {: a:n k:n :} k 0 ?do loop a k ;
: SH-SLOT-LETTER ( n -- n ) {: t:n :}
   t SH-SLOT-ONE {: id:n :}
   0 BEGIN dup SLOT-N @ < WHILE
      dup cells SLOT-KEY + @ id = IF 65 + EXIT THEN
      1 +
   REPEAT drop
   SLOT-N @ SLOT-CAP >= IF 63 EXIT THEN
   id SLOT-N @ cells SLOT-KEY + !
   SLOT-N @ 65 +  SLOT-N @ 1 + SLOT-N ! ;
0 set-tier

public

: RUN-SLOT ( -- )
   s" a word passing a value on across an edge to the slot it holds runs on the host as before" T-LABEL
   3 4 SH-SLOT-QDO 34 T=
   3 0 SH-SLOT-QDO 30 T=
   3 4 SH-SLOT-DO 34 T=
   3 4 true SH-SLOT-JOIN 34 T=
   3 4 false SH-SLOT-JOIN 34 T=
   3 4 SH-SLOT-BACK 35 T=
   3 0 SH-SLOT-BACK 31 T=
   3 4 SH-SLOT-EXIT 4 T= 3 T=
   0 SLOT-N !
   7 SH-SLOT-LETTER 65 T=
   9 SH-SLOT-LETTER 66 T=
   7 SH-SLOT-LETTER 65 T=
   s" and the shadow files an x86-64 routine for each" T-LABEL
   NSHADOW:RECORDS 8 T=
   8 0 ?do REC-SLOT @ i + ROW-OF i T= loop ;

;package

SHADOW-TEST:RUN-SLOT
NSHADOW:CLOSE

\ A shadow whose backend states no unplaced emission: the AArch64 row has none,
\ so the definition is refused at that stage, after the shadow's earlier stages
\ bound their passes in its context.
package SHADOW-TEST
private

TYPED-VARIABLE TRY-A ptr u8
variable TRY-U

: TRY-GO ( -- )
   TRY-A @ TRY-U @ INCLUDE-EVALUATE ;

: TRY-DEFINE ( ptr u8 n -- n )
   TRY-U ! TRY-A !
   [: TRY-GO ;] catch ;

OPEN-A64
ndict@ BAD-NDICT !
1 set-tier
s" : SH-BAD ( n -- n ) 1 + ;" TRY-DEFINE BAD-RC !
0 set-tier
ndict@ BAD-NDICT @ - BAD-MOVED !
NSHADOW:RECORDS BAD-RECS !
NSHADOW:EMISSIONS BAD-EMS !
NSHADOW:CLOSE
1 set-tier
: SH-AFTER ( n -- n ) 2 + ;
0 set-tier

\ A native shadow's own refusals: it opens only for the engine's own binding
\ (src/compiler/native/shadow.f OPEN-NATIVE) and takes only a placed emission
\ (TARGET-CK). An AArch64 binding for another system names the engine's machine,
\ so only the binding comparison refuses it; the unplaced emission is the host
\ machine's, sealed with no placement, so only the placement refuses it.
1 BUFFER: UNPLACED-CODE

: HOST-BINDING ( -- CBIND:binding )
   RTARGET:HOST-TARGET RTARGET:NATIVE-BINDING ;

: SEAL-UNPLACED ( -- )
   UNPLACED-CODE 1 1 HOST-BINDING CBIND:TARGET@ CTARGET:ARCH@ NEMIT:OPEN
   0 NEMIT:FUNCTION+
   NEMIT:SEAL ;

: REFUSED-CASE ( -- )
   s" a shadow whose backend has no unplaced emission refuses the definition and publishes nothing, and the next definition compiles" T-LABEL
   BAD-RC @ E-CTGT-UNLOADED T=
   BAD-MOVED @ 0 T=
   BAD-RECS @ 0 T=
   BAD-EMS @ 0 T=
   40 SH-AFTER 42 T= ;

\ An open that fails to refuse is closed again, so the next case starts closed.
: BINDING-CASE ( -- )
   s" a native shadow refuses to open for another system's binding" T-LABEL
   [: HB-TARGET-MACOS? if
         s" aarch64-unknown-linux-gnu"
      else
         s" aarch64-apple-darwin"
      then RTARGET:RESOLVE RTARGET:NATIVE-BINDING NSHADOW:OPEN-NATIVE
      NSHADOW:CLOSE ;] E-NSHADOW-TARGET TTHROWSQ ;

: UNPLACED-CASE ( -- )
   s" a native shadow under the engine's own binding refuses an unplaced emission" T-LABEL
   HOST-BINDING NSHADOW:OPEN-NATIVE
   SEAL-UNPLACED
   [: NSHADOW:TAKE ;] E-NSHADOW-TARGET TTHROWSQ
   NEMIT:CLEAR
   NSHADOW:CLOSE ;

: CLOSED-CASE ( -- )
   s" a closed shadow answers nothing, and opening one again starts an empty map" T-LABEL
   NSHADOW:OPEN? TFALSE
   [: NSHADOW:RECORDS drop ;] E-NSHADOW-STATE TTHROWSQ
   [: NSHADOW:BINDING CBIND:TARGET@ CTARGET:ARCH@ drop ;] E-NSHADOW-STATE TTHROWSQ
   [: CLOSED-USE ;] SEALED-X64
   OPEN-X64
   NSHADOW:OPEN? TTRUE
   NSHADOW:RECORDS 0 T=
   NSHADOW:EMISSIONS 0 T=
   NSHADOW:CLOSE ;

public

: RUN-CLOSED ( -- )
   REFUSED-CASE
   BINDING-CASE
   UNPLACED-CASE
   CLOSED-CASE
   T-REPORT ;

;package

SHADOW-TEST:RUN-CLOSED
