\ wasm-target.f - sealed product acceptance for the declarable Wasm target.
require lib/test.f
require src/compiler/native/hir.f
require src/compiler/native/elaborate.f
require src/compiler/native/abi.f
require src/compiler/native/backend.f
require test/compiler/native-source-fixture.f

package WASM-TARGET-TEST
private

variable SELECTED
variable EMITTED
TYPED-VARIABLE W-SESSION NSESSION:session

: POLICY ( -- CNUM:numeric-policy )
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY ;

: TARGET ( CTARGET:ptr-width -- CTARGET:contract )
   {: p:CTARGET:ptr-width :}
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
   p CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH CTARGET:CONTRACT ;

: BINDING ( -- CBIND:binding )
   CTARGET-PTR--WIDTH:BITS32 TARGET POLICY CBIND:BIND ;

: UNLOADED-LOWER ( -- ) CTARGET-PTR--WIDTH:BITS32 TARGET NBACK:LOWERS? drop ;
: UNLOADED-EMIT ( -- ) CTARGET-PTR--WIDTH:BITS32 TARGET NBACK:EMITS? drop ;
: UNLOADED-ROW ( -- ) CTARGET-ARCH:WASM NBACK:ROW drop ;

: NO-BACKEND ( -- )
   CTARGET-ARCH:WASM NBACK:REGISTERED? TFALSE
   [: UNLOADED-LOWER ;] E-CTGT-UNLOADED TTHROWSQ
   [: UNLOADED-EMIT ;] E-CTGT-UNLOADED TTHROWSQ
   [: UNLOADED-ROW ;] E-CTGT-UNLOADED TTHROWSQ ;

: MODULE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:BINDING@ BINDING CBIND:SAME? TTRUE
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:FADD HIR:ENSURE-OP {: op:IR-ID:ir-symbol-id :}
   c b HIR:CELL-TYPE {: cell:IR-ID:ir-type-id :}
   c b IR--TYPE-SPACE:GENERIC cell IR-BUILD:INTERN-POINTER drop
   c b HIR:REAL-TYPE {: real:IR-ID:ir-type-id :}
   c b IR--TYPE-FMT:SINGLE IR-BUILD:INTERN-FLT {: single:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   HIR:CELL-BYTES 8 T=
   m IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FMINOR@ 7 T=
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FARCH@
      CTARGET-ARCH:WASM CTARGET-ARCH:EQ TTRUE
   m IR-BUILD:FSCHEMA-ROWS op IR-SCHEMA:FFEATURES@
      CTARGET:F-SCALAR-FP CTARGET:HAS? TTRUE
   m IR-BUILD:FTYPE-ROWS single IR-TYPE:FFLT@
      IR--TYPE-FMT:SINGLE IR--TYPE-FMT:EQ TTRUE
   m IR-BUILD:FTYPE-ROWS real IR-TYPE:FFLT@
      IR--TYPE-FMT:DOUBLE IR--TYPE-FMT:EQ TTRUE
   m IR-BUILD:FTYPE-ROWS cell IR-TYPE:FINT@ drop
      IR--TYPE-WIDTH:W64 IR--TYPE-WIDTH:EQ TTRUE ;


: OPS ( IR-BUILD:module ptr u8 n -- n )
   {: m:IR-BUILD:module name:ptr nameu:n :}
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FKEY
      m IR-BUILD:FKEY 0 IR-ID:PACK-FUN 0 IR-FUN:FBLOCK@
   {: blk:IR-ID:ir-block-id :}
   0
   m IR-BUILD:FBLOCK-ROWS blk IR-FUN:FOP-COUNT 0 ?do
      m IR-BUILD:FBLOCK-ROWS m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY
         blk i IR-FUN:FOP@ {: op:IR-ID:ir-op-id :}
      m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS
         m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY op IR-OP:FOPCODE@
          name nameu IR-SYM:FEQ? if 1+ then
   loop ;


: BAD-SPACE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b IR--TYPE-SPACE:GLOBAL c b HIR:CELL-TYPE
      IR-BUILD:INTERN-POINTER drop ;

: BAD-SPACE-RUN ( -- )
   BINDING [: BAD-SPACE ;] IR-CTX:WITH-CONTEXT ;

: SOURCE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" : WQ ( -- [ -- ] ) [: 1 drop ;] ;" evaluate-closed
   s" WQ [: 1 drop ;]" NSRC:TEXT!
   c NSRC:HIR-BUILDER {: b:IR-BUILD:builder :}
   c b 4 NSRC:MODEL-ROOM {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b NSRC:TAPE {: tp:IR-ARENA:arena :}
   c NSRC:LEX
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   c b v p r 0 1 NELAB:COLON drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FFUN-ROWS IR-FUN:FFUNS 2 T=
    m s" hir.quot" OPS 1 T=
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY 0 IR-ID:PACK-FUN
      IR-FUN:FCONVENTION@ IR--FUN-CONVENTION:WASM IR--FUN-CONVENTION:EQ TTRUE
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY 1 IR-ID:PACK-FUN
      IR-FUN:FCONVENTION@ IR--FUN-CONVENTION:WASM IR--FUN-CONVENTION:EQ TTRUE ;


: DOES-SOURCE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" : WDEF ( -- ) create 0 , does> ( -- n ) @ ;" evaluate-closed
   s" WDEF create 0 , does> @" NSRC:TEXT!
   c NSRC:HIR-BUILDER {: b:IR-BUILD:builder :}
   c b 4 NSRC:MODEL-ROOM {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b NSRC:TAPE {: tp:IR-ARENA:arena :}
   c NSRC:LEX
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   c b v p r 0 0 4 1 1 0 0 s" ( -- n )" NELAB:DOES drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FFUN-ROWS IR-FUN:FFUNS 2 T=
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY 0 IR-ID:PACK-FUN
      IR-FUN:FCONVENTION@ IR--FUN-CONVENTION:WASM IR--FUN-CONVENTION:EQ TTRUE
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY 1 IR-ID:PACK-FUN
      IR-FUN:FCONVENTION@ IR--FUN-CONVENTION:WASM IR--FUN-CONVENTION:EQ TTRUE ;

: OBSERVE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODULE
   c SOURCE
   c DOES-SOURCE ;


: IMPORTED ( IR-CTX:ctx IR-FUN:convention -- )
   {: c:IR-CTX:ctx cv:IR-FUN:convention :}
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b s" foreign" IR-BUILD:ADD-SOURCE {: src:IR-ID:ir-source-id :}
   c b c b s" foreign" IR-BUILD:INTERN-SYMBOL IR-BUILD:BEGIN-FUN
   IR-TYPE:FN-BEGIN
   c b c b IR-BUILD:INTERN-CODE-REF IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:IMPORTED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b cv IR-BUILD:SET-CONVENTION
   c b b src 0 7 IR-BUILD:ADD-SPAN IR-BUILD:SET-FUN-SPAN
   c b IR-BUILD:END-FUN drop ;

: WASM-IMPORT ( IR-CTX:ctx -- )
   IR--FUN-CONVENTION:WASM IMPORTED ;
: HABU-IMPORT ( IR-CTX:ctx -- )
   IR--FUN-CONVENTION:HABU IMPORTED ;
: C-IMPORT ( IR-CTX:ctx -- )
   IR--FUN-CONVENTION:C-ABI IMPORTED ;
: KERNEL-IMPORT ( IR-CTX:ctx -- )
   IR--FUN-CONVENTION:KERNEL IMPORTED ;

: SELECT-WASM ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   c IR-CTX:BINDING@ BINDING CBIND:SAME? TTRUE
   m IR-BUILD:FFUN-ROWS IR-FUN:FFUNS 1 T=
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY 0 IR-ID:PACK-FUN
      IR-FUN:FCONVENTION@ IR--FUN-CONVENTION:WASM IR--FUN-CONVENTION:EQ TTRUE
   m s" hir.fadd" OPS 1 T=
   1 SELECTED +!
   m ;

: DECLINE-EMIT ( IR-CTX:ctx IR-BUILD:module n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module at:n :}
   c IR-CTX:BINDING@ BINDING CBIND:SAME? TTRUE
   m s" hir.fadd" OPS 1 T=
   1 EMITTED +!
   E-CTGT-UNLOADED throw ;

: FLOAT-SOURCE ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   s" : WF ( r r -- r ) f+ ;" evaluate-closed
   s" WF f+" NSRC:TEXT!
   c NSRC:HIR-BUILDER {: b:IR-BUILD:builder :}
   c b 4 NSRC:MODEL-ROOM {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b NSRC:TAPE {: tp:IR-ARENA:arena :}
   c NSRC:LEX
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   c b v p r 2 1 NELAB:COLON drop
   W-SESSION @ b NBACK:FREEZE {: m:IR-BUILD:module :}
   W-SESSION @ m NBACK:SELECT {: selected:IR-BUILD:module :}
   W-SESSION @ selected 0 NBACK:EMIT ;

: FLOAT-CLEAN ( -- )
   W-SESSION @ NBACK:RELEASE
   W-SESSION @ NBACK:RETIRE ;

: FLOAT-WORK ( IR-CTX:ctx NSESSION:session -- )
   W-SESSION !
   [: FLOAT-SOURCE ;] [: FLOAT-CLEAN ;] finally ;

: FLOAT-CONTEXT ( NLEASE:lease IR-CTX:ctx -- )
   {: l:NLEASE:lease c:IR-CTX:ctx :}
   c c l NSESSION:NEW [: FLOAT-WORK ;] NSESSION:WITH-WORK ;

: FLOAT-LEASE ( NLEASE:lease -- )
   BINDING [: FLOAT-CONTEXT ;] IR-CTX:WITH-CONTEXT ;

: FLOAT-REFUSAL ( -- )
   [: FLOAT-LEASE ;] NLEASE:WITH ;

: WRONG-CONVENTIONS ( -- )
   BINDING [: WASM-IMPORT ;] IR-CTX:WITH-CONTEXT
   [: BINDING [: HABU-IMPORT ;] IR-CTX:WITH-CONTEXT ;]
      E-IR-FUN-TARGET TTHROWSQ
   [: BINDING [: C-IMPORT ;] IR-CTX:WITH-CONTEXT ;]
      E-IR-FUN-TARGET TTHROWSQ
   [: BINDING [: KERNEL-IMPORT ;] IR-CTX:WITH-CONTEXT ;]
      E-IR-FUN-TARGET TTHROWSQ
   [: NABI:BINDING [: WASM-IMPORT ;] IR-CTX:WITH-CONTEXT ;]
      E-IR-FUN-TARGET TTHROWSQ ;

: ALWAYS ( CTARGET:contract -- bool ) drop true ;
: NEVER ( CTARGET:contract -- bool ) drop false ;
: NO-DECLARE ( n n NBACK:linkage -- ) 2drop drop ;
: SAME-MODULE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) nip ;
: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- ) E-CTGT-UNLOADED throw ;
: NO-STAGE ( -- ) ;
: NO-PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   2drop 2drop ;

: REGISTER-REFUSING ( -- )
   50 CTARGET:ID CTARGET-ARCH:WASM [: ALWAYS ;] [: NEVER ;]
      CTARGET-BACKEND:MAKE
   NBACK-MODE:EXCLUSIVE-SESSION
   [: NO-DECLARE ;] [: SELECT-WASM ;] [: SAME-MODULE ;] [: SAME-MODULE ;]
   [: DECLINE-EMIT ;] [: NO-UNPLACED ;] [: NO-STAGE ;] [: NO-STAGE ;]
   [: NO-PROTOTYPE ;] [: NO-STAGE ;] [: NO-STAGE ;]
      NBACK-PASS:MAKE
   NBACK:REGISTER
   CTARGET-PTR--WIDTH:BITS32 TARGET NBACK:LOWERS? TTRUE
   CTARGET-PTR--WIDTH:BITS32 TARGET NBACK:EMITS? TFALSE ;

public
: RUN ( -- )
   T-RESET
   s" Wasm's coherent ptr32 and ptr64 contracts keep eight-byte Habu cells" T-LABEL
   CTARGET-PTR--WIDTH:BITS32 TARGET CTARGET:PTR-BITS 32 T=
   CTARGET-PTR--WIDTH:BITS64 TARGET CTARGET:PTR-BITS 64 T=
   s" before registration a coherent Wasm target reports its missing backend" T-LABEL
   NO-BACKEND
   s" a source-loaded observer owns a row and explicitly declines emission" T-LABEL
   REGISTER-REFUSING
   s" a sealed product builds and freezes scalar floating HIR and quoted source" T-LABEL
    BINDING [: OBSERVE ;] IR-CTX:WITH-CONTEXT
    s" source-registered backend reads folded floating HIR and declines emission" T-LABEL
    [: FLOAT-REFUSAL ;] catch E-CTGT-UNLOADED T=
    SELECTED @ 1 T=
    EMITTED @ 1 T=
   WRONG-CONVENTIONS
   [: BAD-SPACE-RUN ;] E-IR-TYPE-TARGET TTHROWSQ
   T-REPORT ;
;package

WASM-TARGET-TEST:RUN
