\ shadow.f - a second target compiled beside the engine's own through NSHADOW
\ (src/compiler/native/shadow.f), read from the host's side.
\
\ WHAT IS DRIVEN. The words below are compiled by the engine's own tier-1
\ driver (src/compiler/native/compiler.f) while an x86-64 shadow is open, so
\ each one runs through the x86-64 rows in a context nested in its definition's
\ and then through the engine's own rows, from the one frozen HIR module. The
\ host words must still run as they always did; the map must hold, for each
\ record publication committed, an x86-64 routine measured from no slot.
\
\ WHAT THE MAP IS ASKED. A leaf is one function ending in `ret`, with no rows
\ and no trailing return split off, because an x86-64 span is exact. A call is a
\ row naming the callee's entry on the host, at the `call` itself, whose rel32
\ is the linker's to write and is zero. A `does>` definer is two records over
\ one routine: the companion enters where the clause function starts, and the
\ definer's own literal of that address holds the same offset, as an unplaced
\ `codeaddr` does. A quotation's address is that function's offset too.
\
\ WHAT IS REFUSED. Publication refuses an emission another machine's backend
\ sealed before its window, which X64KHIR's sealed x86-64 emission shows on
\ this host. A shadow whose backend has no unplaced emission refuses the
\ definition, publishes nothing, and leaves the engine compiling the next one.
\ A taken emission no publication claims files nothing, and the shadow's own
\ refusals are each asserted by code.

require lib/test.f
require lib/errors.f
require src/habu/xref.f
require src/compiler/target.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
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
variable BAD-RC                      \ what refusing the definition threw
variable BAD-NDICT                   \ the record count before it was tried
variable BAD-MOVED                   \ and how far trying it moved the count
variable BAD-RECS                    \ what the refusing shadow had filed
variable BAD-EMS

: OPEN-X64 ( -- )
   X64ABI:BINDING NSHADOW:OPEN ;

: OPEN-A64 ( -- )
   NABI:BINDING NSHADOW:OPEN ;

\ ---- the words compiled with the shadow open ---------------------------------
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
7 SH-CONST SH-SEVEN

\ ---- reading the map ----------------------------------------------------------
\ The row a record was filed under, or -1.
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
   5 SH-QUOT execute 6 T= ;

: FILED-CASE ( -- )
   s" each record publication committed is filed once, a definer's companion beside it, and nothing else was" T-LABEL
   NSHADOW:RECORDS 5 T=
   NSHADOW:EMISSIONS 4 T=
   REC-LEAF @ ROW-OF 0 T=
   REC-CALLER @ ROW-OF 1 T=
   REC-CONST @ ROW-OF 2 T=
   REC-CONST @ 1+ ROW-OF 3 T=
   REC-QUOT @ ROW-OF 4 T= ;

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
      e i NSHADOW:ADDR-SITE-KIND@ X64IR:ADDR-CODE = if drop i leave then
   loop {: k:n :}
   k 0 >= TTRUE
   e k IMM@ clause T= ;

: QUOT-CASE ( -- )
   s" a quotation's address is a code row whose immediate is that function's offset in the emission" T-LABEL
   REC-QUOT @ EM-OF {: e:n :}
   e NSHADOW:FUNCTIONS 2 T=
   e NSHADOW:ADDR-SITES 1 T=
   e 0 NSHADOW:ADDR-SITE-KIND@ X64IR:ADDR-CODE T=
   e 0 IMM@  e 1 NSHADOW:FUNCTION-OFFSET@  T= ;

: ROW-CASE ( -- )
   s" the map refuses a row or an emission it does not hold" T-LABEL
   [: NSHADOW:RECORDS NSHADOW:RECORD@ drop ;] E-NSHADOW-ROW TTHROWSQ
   [: -1 NSHADOW:SIZE drop ;] E-NSHADOW-ROW TTHROWSQ
   [: NSHADOW:EMISSIONS NSHADOW:BYTES drop ;] E-NSHADOW-ROW TTHROWSQ
   [: REC-LEAF @ EM-OF 1 NSHADOW:CALL-SITE@ drop ;] E-NSHADOW-ROW TTHROWSQ
   [: OPEN-X64 ;] E-NSHADOW-STATE TTHROWSQ ;

\ ---- an x86-64 emission sealed on this host -----------------------------------
\ X64KHIR compiles a kernel row through the x86-64 rows and runs `use` while its
\ unplaced emission stands sealed in NEMIT, which is the only way this host ever
\ holds one outside a shadow's own context.
: ADD-STAGE ( -- )
   0 X64KHIR:ARG  1 X64KHIR:ARG  HIR-OPCODE:ADD X64KHIR:OP2  X64KHIR:RESULT ;

: SEALED-X64 ( [ -- ] -- )
   {: use :}
   s" shadow-add" 2 1 [: ADD-STAGE ;] use X64KHIR:COMPILE ;

: FOREIGN-USE ( -- )
   cp@ ndict@ {: cp0:n nd0:n :}
   [: NPUB:PUBLISH-PENDING ;] E-NPUB-TARGET TTHROWSQ
   cp@ cp0 - 0 T=
   ndict@ nd0 - 0 T= ;

: UNCLAIMED-USE ( -- )
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

: WRONG-TARGET-USE ( -- )
   [: NSHADOW:TAKE ;] E-NSHADOW-TARGET TTHROWSQ ;

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

\ ---- values their fixed register cannot be kept for ---------------------------
\ `idiv` reads its dividend from rax and leaves its quotient there, and `shl`
\ reads its count from rcx (src/compiler/native/x64ir.f DEF-IDIV, DEF-SHIFT-CL).
\ A value so pinned that lives across a call, which may destroy every register
\ of the x86-64 pool, or across another divide cannot keep that register over
\ its range, and the allocator puts it in the frame (regalloc.f MB-PIN) where it
\ used to refuse the definition. SH-QUOT-STORE is the shape that stopped the
\ whole engine's window compile at MSEEN-ALLOC: a local holds the quotient over
\ the call `!` makes. The dividend and the count are held over the call by the
\ copy the selector made for the form and coalesced into them.
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

\ ---- a load run longer than the pool -----------------------------------------
\ An x86-64 routine takes every argument it reads out of a register in one run
\ of loads at its entry (src/compiler/native/select-x64.f OPEN-DARGS), and the
\ pool is nine registers. The values of a load run are stored to the frame after
\ the whole run, which keeps the loads together (regalloc.f MB-ANCHOR) and holds
\ each value the run has loaded in a register up to its end, so a run of ten was
\ refused (E-A64RA-POOL) whatever could be put away. A run the registers cannot
\ hold is divided instead. SH-TEN-READ reads ten arguments and calls nothing.
\ SH-TEN-CROSS passes ten to a call and reads them after it, so all ten go to
\ the frame. SH-FIELD-ADD is TYPE-FIELD-OWNER:ADD (src/core/type-family.f), the
\ word that stopped the engine's window compile: twelve locals, ten of them read,
\ over two calls.
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

\ ---- a reload in front of a division ------------------------------------------
\ idiv takes its dividend in rax and writes rdx, so a divisor read there may
\ have neither. SH-DIV-RELOAD's divisor lives over the call `!` makes, so the
\ fixpoint's first turn puts it away and the second reloads it in front of the
\ idiv, where the seven sums live over the division hold the other seven
\ registers. Putting that reload away again brought it back the same in front of
\ the idiv, turn after turn, until the turns' modules held every builder slot
\ (E-IR-BUILD-SLOTS). A reload is never put away, so a sum goes to the frame
\ instead (regalloc.f MB-RELOAD?). It is RD-FIND (src/compiler/ir/arena.f)
\ reduced, the word that stopped the engine's window compile after
\ TYPE-FIELD-OWNER:ADD.
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

\ ---- a frame lane a dominated block forwards ----------------------------------
\ The spill pass threads the frame's memory order through block arguments, and
\ a block's lane is the argument a frame access reads. A one-successor branch
\ forwards it, and not only the branch ending the lane's own block: a value is
\ read in every block it dominates. SH-LANE-FIND's first turn threads a lane
\ through two blocks ending in a two-way branch, and the block each one
\ dominates forwards it by its own branch to a block whose store reads it. The
\ pass followed a block's own terminator only, so when the second turn put
\ `used` away in one of the two blocks it found no lane there and minted a
\ second, leaving the first one read by nobody (E-A64RAV-ORDER; spill.f
\ F-READS!). It is RD-FIND (src/compiler/ir/arena.f) less its first guard.
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

\ ---- a call's results taken back around frame operations ----------------------
\ The loads that take back the cells a call answers are one run, read in place
\ order. SH-PICK takes fourteen back from either of two calls, and the
\ fixpoint's later turns put frame reloads and stores between them. The
\ validator ended the run at anything but a store to the frame, then took the
\ next load for a call site and refused the routine (E-A64RAV-CALL). Those
\ operations leave the callee's cells alone, so the run admits any operation
\ that neither touches the data stack nor branches, as the stores in front of a
\ call do (regalloc-verify.f DLOAD-RUN, DSTORE-RUN). It is ROUTINE
\ (src/arch/x86-64/passes.f) reduced, the word that stopped the engine's window
\ compile after RD-FIND.
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

\ ---- a shadow whose backend states no unplaced emission -----------------------
\ The AArch64 row has none, so the definition is refused at that stage, after
\ the shadow's earlier stages bound their passes in its context.
package SHADOW-TEST
private

TYPED-VARIABLE TRY-A ptr u8
variable TRY-U

: TRY-GO ( -- )
   TRY-A @ TRY-U @ INCLUDE-EVALUATE ;

\ Define from source and answer what refusing the definition threw, or zero.
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

: REFUSED-CASE ( -- )
   s" a shadow whose backend has no unplaced emission refuses the definition and publishes nothing, and the next definition compiles" T-LABEL
   BAD-RC @ E-CTGT-UNLOADED T=
   BAD-MOVED @ 0 T=
   BAD-RECS @ 0 T=
   BAD-EMS @ 0 T=
   40 SH-AFTER 42 T= ;

: CLOSED-CASE ( -- )
   s" a closed shadow answers nothing, and opening one again starts an empty map" T-LABEL
   NSHADOW:OPEN? TFALSE
   [: NSHADOW:RECORDS drop ;] E-NSHADOW-STATE TTHROWSQ
   [: NSHADOW:BINDING CBIND:TARGET@ CTARGET:ARCH@ drop ;] E-NSHADOW-STATE TTHROWSQ
   [: NSHADOW:TAKE ;] E-NSHADOW-STATE TTHROWSQ
   OPEN-X64
   NSHADOW:OPEN? TTRUE
   NSHADOW:RECORDS 0 T=
   NSHADOW:EMISSIONS 0 T=
   NSHADOW:CLOSE ;

public

: RUN-CLOSED ( -- )
   REFUSED-CASE
   CLOSED-CASE
   T-REPORT ;

;package

SHADOW-TEST:RUN-CLOSED
