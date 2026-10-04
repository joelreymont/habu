\ select-lib.f - the fixture WSEL's row files share (test/wasm/select.f,
\ select-f64.f): a definition compiled through the front end and NBACK:FREEZE,
\ or built straight in HIR, selected by the rows a Wasm backend installs, and
\ read back from the frozen WSTRUCT module. A row file reopens WSEL-TEST and
\ installs the profile its rows are selected under, once in its process
\ (src/arch/wasm/profile.f).
\
\ A shape lists per block its argument count and its operations, each with what
\ it carries past its opcode - a number's value, an address's kind, whose value
\ is the linker's, and a branch's destinations as block indices - and a run of
\ one operation written once with its count. The rows are structural; executing
\ them is the rows dot's.

require lib/test.f
require lib/string.f
require lib/fmt.f
require src/compiler/ir/type.f
require src/compiler/ir/op.f
require src/compiler/ir/attr.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/dict.f
require src/compiler/native/frozen.f
require src/compiler/native/hir.f
require src/compiler/native/elaborate.f
require src/compiler/native/backend.f
require src/arch/wasm/profile.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/select.f
require test/compiler/native-source-fixture.f

package WSEL-TEST
private

\ ---- the binding --------------------------------------------------------------
: POLICY ( CNUM:overflow -- CNUM:numeric-policy )
   CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY ;

: BINDING ( CNUM:overflow -- CBIND:binding )
   {: o:CNUM:overflow :}
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH
   CTARGET:CONTRACT  o POLICY  CBIND:BIND ;

: ALWAYS ( CTARGET:contract -- bool ) drop true ;
: NEVER ( CTARGET:contract -- bool ) drop false ;

: NO-REWRITE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) nip ;
: NO-EMIT ( IR-CTX:ctx IR-BUILD:module n -- ) 2drop drop ;
: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- ) E-CTGT-UNLOADED throw ;
: NO-STAGE ( -- ) ;
: NO-PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   2drop 2drop ;

\ The profile, and the row the Wasm backend registers, with WSEL's own declare
\ and select as NBACK reaches them; it lowers every Wasm contract and emits
\ none.
: INSTALL ( WPROF:profile -- )
   WPROF:CURRENT!
   71 CTARGET:ID CTARGET-ARCH:WASM [: ALWAYS ;] [: NEVER ;]
   CTARGET-BACKEND:MAKE
   [: WSEL:DECLARE ;] [: WSEL:SELECT ;] [: NO-REWRITE ;] [: NO-REWRITE ;]
   [: NO-EMIT ;] [: NO-UNPLACED ;] [: NO-STAGE ;] [: NO-STAGE ;]
   [: NO-PROTOTYPE ;] [: NO-STAGE ;] [: NO-STAGE ;] NBACK-PASS:MAKE
   NBACK:REGISTER ;

\ ---- the session a row's stages run in ----------------------------------------
TYPED-VARIABLE SES NSESSION:session

: SES-WORK ( [ IR-CTX:ctx -- ] IR-CTX:ctx NSESSION:session -- )
   {: q c:IR-CTX:ctx s:NSESSION:session :}
   s SES !
   c q execute ;

: SES-CONTEXT ( [ IR-CTX:ctx -- ] NLEASE:lease IR-CTX:ctx -- )
   {: q l:NLEASE:lease c:IR-CTX:ctx :}
   q c  c l NSESSION:NEW  [: SES-WORK ;] NSESSION:WITH-WORK ;

: SES-LEASE ( CNUM:overflow [ IR-CTX:ctx -- ] NLEASE:lease -- )
   {: o:CNUM:overflow q l:NLEASE:lease :}
   q l  o BINDING  [: SES-CONTEXT ;] IR-CTX:WITH-CONTEXT ;

\ The body run in a context bound under the overflow policy, in a session of
\ its own, as the compiler runs a definition's stages.
: BOUND ( CNUM:overflow [ IR-CTX:ctx -- ] -- )
   [: SES-LEASE ;] NLEASE:WITH ;

\ ---- one definition, compiled and selected -------------------------------------
variable IN-N                        \ what the definition takes and leaves
variable OUT-N
variable DECL-IN                     \ and what its DECLARE states
variable DECL-OUT

\ The source, and the arity both the definition and its DECLARE state.
: SOURCE! ( ptr u8 n n n -- )
   {: src:ptr sn:n in:n out:n :}
   src sn NSRC:TEXT!
   in IN-N !  out OUT-N !  in DECL-IN !  out DECL-OUT ! ;

\ The front end: the source elaborated to HIR and frozen.
: FROZEN ( IR-CTX:ctx -- IR-BUILD:module )
   {: c:IR-CTX:ctx :}
   c NSRC:HIR-BUILDER {: b:IR-BUILD:builder :}
   c b 8 NSRC:MODEL-ROOM {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b NSRC:TAPE {: tp:IR-ARENA:arena :}
   c NSRC:LEX
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   c b v p r IN-N @ OUT-N @ NELAB:COLON drop
   SES @ b NBACK:FREEZE ;

: DECLARED ( -- )
   SES @ DECL-IN @ DECL-OUT @ NBACK:L-CALLED NBACK:DECLARE ;

\ The shadow route's order (src/compiler/native/compiler.f SHADOW-WORK): the
\ frozen module declared, then selected.
: COMPILE ( IR-CTX:ctx -- IR-BUILD:module )
   {: c:IR-CTX:ctx :}
   c FROZEN {: m:IR-BUILD:module :}
   DECLARED
   SES @ m NBACK:SELECT ;

: WRAPPED ( [ IR-CTX:ctx -- ] -- )
   CNUM-OVERFLOW:WRAP swap BOUND ;

\ ---- the selected module, read back -------------------------------------------
256 BUFFER: NB

: SYM$ ( IR-ID:ir-symbol-id -- ptr u8 n )
   {: s:IR-ID:ir-symbol-id :}
   NFROZEN:V-SYMP NFROZEN:VW NFROZEN:V-SYMR NFROZEN:VW s NB 256 IR-SYM:FCOPY
   NB swap ;

\ Which of the operation's attributes is under the key spelled, or -1.
: KEY-AT ( IR-ID:ir-op-id ptr u8 n -- n )
   {: o:IR-ID:ir-op-id a:ptr u:n :}
   -1
   o NFROZEN:ATTRS-OF 0 ?do
      o i NFROZEN:ATTR-KEY-AT SYM$ a u STR= if drop i leave then
   loop ;

\ The address kind an i64.const states; no other operation states one.
: KIND ( IR-ID:ir-op-id -- n )
   {: o:IR-ID:ir-op-id :}
   o s" wstruct.addr" KEY-AT {: at:n :}
   at 0 < if WSTRUCT:ADDR-NONE exit then
   o at NFROZEN:ATTR-INT-AT ;

\ A constant that holds a number, not an address, and the value it holds.
: NUMBER? ( IR-ID:ir-op-id -- bool )
   {: o:IR-ID:ir-op-id :}
   o s" wstruct.value" KEY-AT 0 >=  o KIND WSTRUCT:ADDR-NONE =  and ;

: VALUE ( IR-ID:ir-op-id -- n )
   {: o:IR-ID:ir-op-id :}
   o  o s" wstruct.value" KEY-AT  NFROZEN:ATTR-INT-AT ;

1 TYPED-BUFFER RUN-OP IR-ID:ir-op-id   \ the operation a run repeats
variable RUN-N                         \ and how many times
variable FUN-B0                        \ the function's first block, module-local

: SAME? ( IR-ID:ir-op-id IR-ID:ir-op-id -- bool )
   {: a:IR-ID:ir-op-id b:IR-ID:ir-op-id :}
   a NFROZEN:OPCODE-AT b NFROZEN:OPCODE-AT NFROZEN:SAME-SYM?
   a KIND b KIND = and 0= if false exit then
   a NUMBER? 0= if true exit then
   a VALUE b VALUE = ;

\ A branch's destinations: `>` and their indices in the function, by commas.
: SUCCS+ ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o NFROZEN:SUCCS-OF 0 ?do
      i 0= if s" >" else s" ," then SB-APPEND
      o i NFROZEN:SUCC-AT IR-ID:BLOCK-LOCAL FUN-B0 @ - FMT:SB-U
   loop ;

\ The operation without the dialect's `wstruct.` prefix, `=value` after a
\ number, `@data` or `@code` after an address.
: NAME+ ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   o NFROZEN:OPCODE-AT SYM$ {: a:ptr u:n :}
   a 8 +  u 8 -  SB-APPEND
   o NUMBER? if  s" =" SB-APPEND  o VALUE FMT:SB-INT  then
   o KIND WSTRUCT:ADDR-DATA = if s" @data" SB-APPEND then
   o KIND WSTRUCT:ADDR-CODE = if s" @code" SB-APPEND then ;

\ The run's operation, its destinations, and `*count` after a run longer than
\ one.
: FLUSH ( -- )
   RUN-N @ 0= if exit then
   0 RUN-OP @ {: o:IR-ID:ir-op-id :}
   o NAME+
   o SUCCS+
   RUN-N @ 1 > if  s" *" SB-APPEND  RUN-N @ FMT:SB-U  then
   s"  " SB-APPEND
   0 RUN-N ! ;

: OP+ ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   RUN-N @ 0<> if
      0 RUN-OP @ o SAME? if  RUN-N @ 1+ RUN-N !  exit  then
   then
   FLUSH
   o 0 RUN-OP !
   1 RUN-N ! ;

\ A block as its argument count and its operations: `2: i64.add return`.
: BLOCK+ ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk NFROZEN:ARG-COUNT FMT:SB-U  s" : " SB-APPEND
   0 RUN-N !
   bk NFROZEN:OP-COUNT 0 ?do  bk i NFROZEN:OP-AT OP+  loop
   FLUSH
   s" | " SB-APPEND ;

\ Function k of the module, every block in order, each written by w.
: FUN$ ( IR-BUILD:module n [ IR-ID:ir-block-id -- ] -- ptr u8 n )
   {: m:IR-BUILD:module k:n w :}
   m NFROZEN:VIEWS!
   NFROZEN:MKEY k IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f 0 NFROZEN:BLOCK-AT IR-ID:BLOCK-LOCAL FUN-B0 !
   SB-RESET
   f NFROZEN:BLOCK-COUNT 0 ?do  f i NFROZEN:BLOCK-AT w execute  loop
   SB$ ;

\ ---- the rows -------------------------------------------------------------------
2048 constant GOT-CAP
GOT-CAP BUFFER: GOT
variable GOT-N

\ A text past the buffer is cut, so it still differs from every row's.
: KEEP ( ptr u8 n -- )
   {: a:ptr u:n :}
   u GOT-CAP min {: k:n :}
   a GOT k BYTE-COPY  k GOT-N ! ;

: KEPT ( ptr u8 n -- )
   {: want:ptr wn:n :}
   GOT GOT-N @ want wn T$= ;

: SHAPE-RUN ( IR-CTX:ctx -- )    COMPILE 0 [: BLOCK+ ;] FUN$ KEEP ;

TYPED-VARIABLE READER [ IR-CTX:ctx -- ]   \ what a row reads off its module

: READ-RUN ( -- )
   READER @ WRAPPED ;

\ A row's run, a throw kept as `throw <code>`: the row that meets one fails by
\ its label, and the rows after it still run.
: OUTCOME ( [ IR-CTX:ctx -- ] -- )
   READER !
   [: READ-RUN ;] catch {: code:n :}
   code 0= if exit then
   SB-RESET  s" throw " SB-APPEND  code FMT:SB-INT  SB$ KEEP ;

\ The source, its arity and the shape function zero must select to.
: ROW ( ptr u8 n n n ptr u8 n -- )
   {: src:ptr sn:n in:n out:n want:ptr wn:n :}
   src sn in out SOURCE!
   [: SHAPE-RUN ;] OUTCOME
   want wn KEPT ;

\ ---- the refusals -------------------------------------------------------------
: REFUSED ( ptr u8 n n n n -- )
   {: src:ptr sn:n in:n out:n code:n :}
   src sn in out SOURCE!
   [: [: COMPILE drop ;] WRAPPED ;] code TTHROWSQ ;

\ ---- a function built straight in HIR -----------------------------------------
\ For a shape the source fixture cannot write: built through the staged builder
\ and frozen by NBACK:FREEZE, whose interim freeze checks no operation's shape.
1 TYPED-BUFFER Z-CTX IR-CTX:ctx
1 TYPED-BUFFER Z-BLD IR-BUILD:builder
1 TYPED-BUFFER Z-SPAN IR-SOURCE:span

: ZC ( -- IR-CTX:ctx )              0 Z-CTX @ ;
: ZB ( -- IR-BUILD:builder )        0 Z-BLD @ ;

: Z-OPEN ( HIR:opcode -- )
   {: o:HIR:opcode :}
   ZC ZB  ZC ZB o HIR:OPCODE  IR-BUILD:BEGIN-OP
   ZC ZB  0 Z-SPAN @  IR-BUILD:SET-OP-SPAN ;

: Z-INT ( IR-ID:ir-symbol-id n -- )
   {: k:IR-ID:ir-symbol-id v:n :}
   ZC ZB k  ZC ZB v IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR ;

\ A function of in cells to out cells, in the Wasm convention the elaborator
\ gives a definition under this binding, opened on its entry block.
: Z-BEGIN ( ptr u8 n n n -- )
   {: nm:ptr nn:n in:n out:n :}
   ZC ZB HIR:CELL-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do  t IR-TYPE:FN-PARAM  loop
   out 0 ?do  t IR-TYPE:FN-RESULT  loop
   ZC ZB IR-BUILD:INTERN-CODE-REF {: sig:IR-ID:ir-type-id :}
   ZC ZB  ZC ZB nm nn IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   ZC ZB sig IR-BUILD:SET-SIGNATURE
   ZC ZB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   ZC ZB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   ZC ZB IR--FUN-CONVENTION:WASM IR-BUILD:SET-CONVENTION
   ZC ZB  0 Z-SPAN @  IR-BUILD:SET-FUN-SPAN
   ZC ZB IR-BUILD:BEGIN-BLOCK
   ZC ZB  0 Z-SPAN @  IR-BUILD:SET-BLOCK-SPAN ;

: Z-SOURCE ( IR-CTX:ctx ptr u8 n -- )
   {: c:IR-CTX:ctx nm:ptr nn:n :}
   c 0 Z-CTX !
   c NSRC:HIR-BUILDER 0 Z-BLD !
   ZB  ZC ZB nm nn IR-BUILD:ADD-SOURCE  0 nn IR-BUILD:ADD-SPAN  0 Z-SPAN ! ;

\ The function closed, frozen by NBACK:FREEZE, declared and selected.
: Z-SELECT ( IR-CTX:ctx -- IR-BUILD:module )
   {: c:IR-CTX:ctx :}
   ZC ZB IR-BUILD:END-BLOCK drop
   ZC ZB IR-BUILD:END-FUN drop
   SES @ ZB NBACK:FREEZE {: m:IR-BUILD:module :}
   DECLARED
   SES @ m NBACK:SELECT ;

;package
