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
