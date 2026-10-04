\ backend.f - WBACK and WPASS, src/arch/wasm/backend.f and passes.f, on the
\ product engine: the Wasm backend loaded at run time, installed, and driven by
\ the engine's own tier-1 driver through an open Wasm shadow
\ (src/compiler/native/shadow.f), as test/compiler/shadow.f drives x86-64.
\
\ WHAT IS PROVED. INSTALL registers the provider, which serves only the
\ little-endian ptr32 habu-wasm-cell64-v1 contract. Real source compiled with
\ the shadow open still runs on the host, and every record publication
\ committed is filed with one Wasm emission whose header names its one body -
\ offset, size, lanes, frame variant - and whose call and address rows sit on
\ the padded fields after `call` and `i64.const`, each holding what the row
\ says: a zero index and the callee's host entry or its own body offset, an
\ address of its kind. The placed emission row is refused by name. A
\ definition the selector refuses publishes nothing, leaves NEMIT and the
\ encoder empty and the next definition compiling on both sides. A definition
\ of the engine's own machine comes out byte for byte as it did before the
\ backend loaded.

require lib/test.f
require lib/errors.f
require src/habu/xref.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/context.f
require src/compiler/ir/build.f
require src/compiler/session/lease.f
require src/compiler/native/backend.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f

\ The engine's own machine's routine for this body, before any Wasm source is
\ loaded; BK-SAME-AFTER repeats it with the Wasm shadow open.
package WBACK-TEST
private
variable REC-BEFORE
ndict@ REC-BEFORE !
1 set-tier
: BK-SAME-BEFORE ( n n -- n ) 2dup < if swap then - 3 lshift ;
0 set-tier
;package

require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/select.f
require src/arch/wasm/encode.f
require src/arch/wasm/backend.f

package WBACK-TEST
private

\ A Wasm contract with one field changed, assembled by the generated
\ constructor, so a field the target table does not yet pair with Wasm can
\ still be asked of SERVES?.
: CONTRACT ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width -- CTARGET:contract )
   CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH CTARGET-CONTRACT:MAKE ;

: WASM32 ( -- CTARGET:contract )
   WBACK:BINDING CBIND:TARGET@ ;

: WASM64 ( -- CTARGET:contract )
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 CONTRACT ;

: SERVES-CASE ( -- )
   s" the backend serves the little-endian ptr32 habu-wasm-cell64-v1 contract its binding names" T-LABEL
   WASM32 WBACK:SERVES? TTRUE
   s" and no contract that differs in its architecture, ABI, byte order or pointer width" T-LABEL
   CTARGET-ARCH:X86-64 CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32 CONTRACT WBACK:SERVES? TFALSE
   CTARGET-ARCH:WASM CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32 CONTRACT WBACK:SERVES? TFALSE
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:BIG
   CTARGET-PTR--WIDTH:BITS32 CONTRACT WBACK:SERVES? TFALSE
   WASM64 WBACK:SERVES? TFALSE ;

\ The provider row INSTALL published, by its id.
: ROW-ID ( -- n )
   WBACK:ID NBACK:ID-ROW NBACK:BACKEND@
   CTARGET-BACKEND:UNMAKE 2drop drop CTARGET:ID-CODE ;

: INSTALL-CASE ( -- )
   s" INSTALL registers the Wasm provider under its id" T-LABEL
   CTARGET-ARCH:WASM NBACK:REGISTERED? TTRUE
   ROW-ID  WBACK:ID CTARGET:ID-CODE  T=
   s" the registry lowers and emits the ptr32 contract and refuses ptr64" T-LABEL
   WASM32 NBACK:LOWERS? TTRUE
   WASM32 NBACK:EMITS? TTRUE
   WASM64 NBACK:LOWERS? TFALSE
   WASM64 NBACK:EMITS? TFALSE ;

\ An address as the number a field holds.
: >N ( ptr u8 -- n )
   NULL-PTR BYTE-VIEW - ;

\ ---- the words compiled with the Wasm shadow open ----------------------------
variable REC-LEAF
variable REC-CALLER
variable REC-SELF
variable REC-ADDR
variable REC-BOOM
variable REC-TRAP
variable REC-SAME

: OPEN-WASM ( -- )
   WBACK:BINDING NSHADOW:OPEN ;

T-RESET
WBACK:INSTALL
OPEN-WASM
create BK-BUF 16 allot
1 set-tier
ndict@ REC-LEAF !
: BK-LEAF ( n n -- n ) + ;
ndict@ REC-CALLER !
: BK-CALLER ( n -- n ) dup BK-LEAF 1 + ;
ndict@ REC-SELF !
: BK-SELF ( n -- n ) dup 0= if drop 1 exit then 1 - RECURSE ;
ndict@ REC-ADDR !
: BK-ADDR ( -- ptr u8 ) BK-BUF ;
ndict@ REC-BOOM !
: BK-BOOM ( n -- ) throw ;
ndict@ REC-TRAP !
: BK-TRAP ( n -- ) BK-BOOM ;
ndict@ REC-SAME !
: BK-SAME-AFTER ( n n -- n ) 2dup < if swap then - 3 lshift ;
0 set-tier

: HOST-CASE ( -- )
   s" every word compiled with the Wasm shadow open runs on the host as before" T-LABEL
   2 3 BK-LEAF 5 T=
   4 BK-CALLER 9 T=
   5 BK-SELF 1 T=
   BK-ADDR >N  BK-BUF >N  T=
   [: 7 BK-TRAP ;] 7 TTHROWSQ
   9 2 BK-SAME-AFTER 56 T= ;

\ ---- reading the map ----------------------------------------------------------
\ Wasm's own opcodes for the bytes a row sits after, and the body's last.
$10 constant OP-CALL
$42 constant OP-I64-CONST
$0B constant OP-END
\ The header: magic and function count, then twelve bytes per function.
8 constant HEAD-BYTES
12 constant ROW-BYTES

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

\ The len-byte little-endian number at off in emission e.
: LE@ ( n n n -- n ) {: e:n off:n len:n :}
   0  len 0 ?do  8 lshift  e  off len + 1- i -  BYTE@ or  loop ;

: HOST-ENTRY ( n -- n )
   XREF-REC XREF-START ;

\ Record r's emission: entered at its first byte, no trailing return split
\ off, the magic, one function whose header row names the one body filling
\ the rest, ending in `end`, with in and out lanes and the lane variant.
: ONE-BODY ( n n n -- ) {: r:n in:n out:n :}
   r EM-OF {: e:n :}
   r ROW-OF NSHADOW:ENTRY@ 0 T=
   e NSHADOW:RET-BYTES 0 T=
   e 0 4 LE@ WENC:MAGIC T=
   e 4 4 LE@ 1 T=
   e NSHADOW:FUNCTIONS 1 T=
   e 0 NSHADOW:FUNCTION-OFFSET@  HEAD-BYTES ROW-BYTES +  T=
   e HEAD-BYTES 4 LE@  HEAD-BYTES ROW-BYTES +  T=
   e HEAD-BYTES 4 + 4 LE@  HEAD-BYTES ROW-BYTES + +  e NSHADOW:SIZE T=
   e HEAD-BYTES 8 + BYTE@ in T=
   e HEAD-BYTES 9 + BYTE@ out T=
   e HEAD-BYTES 10 + BYTE@ 0 T=
   e  e NSHADOW:SIZE 1-  BYTE@ OP-END T= ;

\ Call row k of emission e: a `call` before a padded zero index, a call that
\ comes back; answers its target.
: CALL-ROW ( n n -- n ) {: e:n k:n :}
   e k NSHADOW:CALL-SITE@ {: at:n :}
   e at 1- BYTE@ OP-CALL T=
   e NSHADOW:BYTES e NSHADOW:SIZE at WLEB:U32-PAD@ 0 T=
   e k NSHADOW:CALL-KIND@ NEMIT:CALL T=
   e k NSHADOW:CALL-TARGET@ ;

\ Address row k of emission e: an `i64.const` before a padded field; answers
\ its kind and the value it holds.
: ADDR-ROW ( n n -- n n ) {: e:n k:n :}
   e k NSHADOW:ADDR-SITE@ {: at:n :}
   e at 1- BYTE@ OP-I64-CONST T=
   e k NSHADOW:ADDR-SITE-KIND@
   e NSHADOW:BYTES e NSHADOW:SIZE at WLEB:S64-PAD@ ;

: FILED-CASE ( -- )
   s" each record publication committed is filed once, with an emission of its own" T-LABEL
   NSHADOW:RECORDS 7 T=
   NSHADOW:EMISSIONS 7 T=
   REC-LEAF @ ROW-OF 0 T=
   REC-CALLER @ ROW-OF 1 T=
   REC-SELF @ ROW-OF 2 T=
   REC-ADDR @ ROW-OF 3 T=
   REC-BOOM @ ROW-OF 4 T=
   REC-TRAP @ ROW-OF 5 T=
   REC-SAME @ ROW-OF 6 T=
   7 0 ?do  i NSHADOW:EMISSION@ i T=  loop ;

: BODY-CASE ( -- )
   s" each emission's header names its one body, its lanes the word's arity" T-LABEL
   REC-LEAF @ 2 1 ONE-BODY
   REC-CALLER @ 1 1 ONE-BODY
   REC-SELF @ 1 1 ONE-BODY
   REC-ADDR @ 0 1 ONE-BODY
   REC-BOOM @ 1 0 ONE-BODY
   REC-TRAP @ 1 0 ONE-BODY
   REC-SAME @ 2 1 ONE-BODY ;

: CALL-CASE ( -- )
   s" a call row sits on the padded index after `call`, naming the callee's host entry" T-LABEL
   REC-CALLER @ EM-OF {: caller:n :}
   caller NSHADOW:CALL-SITES 1 T=
   caller 0 CALL-ROW  REC-LEAF @ HOST-ENTRY  T=
   REC-TRAP @ EM-OF {: trap:n :}
   trap NSHADOW:CALL-SITES 1 T=
   trap 0 CALL-ROW  REC-BOOM @ HOST-ENTRY  T=
   s" a call to the definition itself names its own body" T-LABEL
   REC-SELF @ EM-OF {: self:n :}
   self NSHADOW:CALL-SITES 1 T=
   self 0 CALL-ROW  self 0 NSHADOW:FUNCTION-OFFSET@  T=
   s" throw is a call row too" T-LABEL
   REC-BOOM @ EM-OF {: boom:n :}
   boom NSHADOW:CALL-SITES 1 T=
   boom 0 CALL-ROW 0<> TTRUE
   s" a word that calls nothing has no call row" T-LABEL
   REC-LEAF @ EM-OF NSHADOW:CALL-SITES 0 T=
   REC-ADDR @ EM-OF NSHADOW:CALL-SITES 0 T=
   REC-SAME @ EM-OF NSHADOW:CALL-SITES 0 T= ;

: ADDR-CASE ( -- )
   s" an address row sits on the padded field after `i64.const` and holds the host address" T-LABEL
   REC-ADDR @ EM-OF {: addr:n :}
   addr NSHADOW:ADDR-SITES 1 T=
   addr 0 ADDR-ROW  BK-BUF >N T=  WSTRUCT:ADDR-DATA T=
   s" a trap's message is a data address row" T-LABEL
   REC-TRAP @ EM-OF {: trap:n :}
   trap NSHADOW:ADDR-SITES 1 T=
   trap 0 ADDR-ROW drop  WSTRUCT:ADDR-DATA T=
   s" a word that names no address has no address row" T-LABEL
   REC-LEAF @ EM-OF NSHADOW:ADDR-SITES 0 T=
   REC-CALLER @ EM-OF NSHADOW:ADDR-SITES 0 T=
   REC-SELF @ EM-OF NSHADOW:ADDR-SITES 0 T=
   REC-BOOM @ EM-OF NSHADOW:ADDR-SITES 0 T=
   REC-SAME @ EM-OF NSHADOW:ADDR-SITES 0 T= ;

: RETIRED-CASE ( -- )
   s" after a definition NEMIT holds no rows and the encoder no emission" T-LABEL
   [: NEMIT:SIZE drop ;] E-NEMIT-STATE TTHROWSQ
   [: WENC:SIZE drop ;] E-WENC-STATE TTHROWSQ ;

\ ---- the engine's own routine -------------------------------------------------
CAST: >CODE ( n -- ptr u8 )

\ Whether records a and b hold the same code bytes.
: CODE-SAME? ( n n -- bool ) {: a:n b:n :}
   a XREF-REC XREF-CODE-BYTES {: len:n :}
   b XREF-REC XREF-CODE-BYTES len <> if false exit then
   a XREF-REC XREF-START >CODE {: pa:ptr :}
   b XREF-REC XREF-START >CODE {: pb:ptr :}
   len 0 ?do
      pa i + c@  pb i + c@  <> if false unloop exit then
   loop
   true ;

: SAME-CASE ( -- )
   s" the engine's own routine is byte for byte the one compiled before the Wasm backend loaded" T-LABEL
   REC-BEFORE @ XREF-REC XREF-CODE-BYTES 0 > TTRUE
   REC-BEFORE @ REC-SAME @ CODE-SAME? TTRUE ;

\ ---- the placed row -----------------------------------------------------------
\ NBACK:EMIT asked of a Wasm-bound session, with an empty module frozen there.
: EMIT-WORK ( IR-CTX:ctx NSESSION:session -- )
   {: c:IR-CTX:ctx s:NSESSION:session :}
   IR-BUILD:PLAN-DEFAULT
   c WSTRUCT:NEW-BUILDER {: b:IR-BUILD:builder :}
   s  c b WSTRUCT:FREEZE  0 NBACK:EMIT ;

: EMIT-CONTEXT ( NLEASE:lease IR-CTX:ctx -- )
   {: l:NLEASE:lease c:IR-CTX:ctx :}
   c  c l NSESSION:NEW  [: EMIT-WORK ;] NSESSION:WITH-WORK ;

: EMIT-LEASE ( NLEASE:lease -- )
   WBACK:BINDING [: EMIT-CONTEXT ;] IR-CTX:WITH-CONTEXT ;

: EMIT-RUN ( -- )
   [: EMIT-LEASE ;] NLEASE:WITH ;

: EMIT-CASE ( -- )
   s" the placed emission row is refused: there is no placed Wasm emission" T-LABEL
   [: EMIT-RUN ;] E-CTGT-UNLOADED TTHROWSQ ;

public

: RUN-OPEN ( -- )
   SERVES-CASE
   INSTALL-CASE
   HOST-CASE
   FILED-CASE
   BODY-CASE
   CALL-CASE
   ADDR-CASE
   RETIRED-CASE
   SAME-CASE
   EMIT-CASE ;

;package

WBACK-TEST:RUN-OPEN

\ ---- a definition the Wasm side refuses ---------------------------------------
\ One holding a quotation, whose descriptor the selector leaves to a sibling,
\ tried with the shadow open; then a definition that both sides compile.
package WBACK-TEST
private

variable SEL-RC
variable NDICT0
variable MOVED
variable RECS0
variable EMS0
variable RECS1
variable EMS1
variable REC-AFTER

\ Define from source and answer what refusing the definition threw, or zero.
\ The text evaluated is a copy, since a throw restores the depth catch began
\ with, and both cells are dropped, so a refusal leaves only its code.
: TRY-DEFINE ( ptr u8 n -- n )
   [: 2dup INCLUDE-EVALUATE ;] catch
   {: rc:n :}
   2drop rc ;

ndict@ NDICT0 !
NSHADOW:RECORDS RECS0 !
NSHADOW:EMISSIONS EMS0 !
1 set-tier
s" : BK-BAD-SEL ( -- [ n -- n ] ) [: 1 + ;] ;" TRY-DEFINE SEL-RC !
0 set-tier
ndict@ NDICT0 @ - MOVED !
NSHADOW:RECORDS RECS1 !
NSHADOW:EMISSIONS EMS1 !
1 set-tier
ndict@ REC-AFTER !
: BK-AFTER ( n -- n ) 2 + ;
0 set-tier

: REFUSED-CASE ( -- )
   s" a definition holding a quotation, which the selector does not lower, is refused by its code" T-LABEL
   SEL-RC @ E-WSEL-REFUSED T=
   s" it publishes no word, files no record, and leaves NEMIT and the encoder holding no rows" T-LABEL
   MOVED @ 0 T=
   RECS1 @ RECS0 @ T=
   EMS1 @ EMS0 @ T=
   [: NEMIT:SIZE drop ;] E-NEMIT-STATE TTHROWSQ
   [: WENC:SIZE drop ;] E-WENC-STATE TTHROWSQ
   s" the next definition compiles on both sides and is filed with its own emission" T-LABEL
   40 BK-AFTER 42 T=
   NSHADOW:RECORDS RECS1 @ 1+ T=
   REC-AFTER @ ROW-OF RECS1 @ T=
   REC-AFTER @ 1 1 ONE-BODY ;

public

: RUN-REFUSED ( -- )
   REFUSED-CASE
   NSHADOW:CLOSE
   T-REPORT ;

;package

WBACK-TEST:RUN-REFUSED
