\ select.f - WSEL, src/arch/wasm/select.f: a definition compiled through the
\ front end and NBACK:FREEZE, selected by the rows a Wasm backend installs, and
\ read back from the frozen WSTRUCT module.
\
\ Each row compiles Habu source, selects it and compares what the module holds:
\ per block its argument count and its operations, each with what it carries
\ past its opcode - a number's value, an address's kind, whose value is the
\ linker's, and a branch's destinations as block indices - and a run of one
\ operation written once with its count; or a function's signature, or a
\ call's callee. The rows are structural; executing them is the rows dot's.

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

\ Host words the rows call, named globally as source text names them.
: SEL-INC ( n -- n ) 1 + ;

\ One input past WPROF's sixteen lanes, so a call to it takes the frame.
: SEL-SUM17 ( n n n n n n n n n n n n n n n n n -- n )
   + + + + + + + + + + + + + + + + ;

\ Dead by its own body, so the elaborator stages a fault after a call to it.
: SEL-BOOM ( n -- ) throw ;

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

\ The row the Wasm backend registers, with WSEL's own declare and select as
\ NBACK reaches them; it lowers every Wasm contract and emits none.
: INSTALL ( -- )
   WPROF:V1 WPROF:CURRENT!
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

\ The run's operation without the dialect's `wstruct.` prefix, `=value` after a
\ number, `@data` or `@code` after an address, its destinations, and `*count`
\ after a run longer than one.
: FLUSH ( -- )
   RUN-N @ 0= if exit then
   0 RUN-OP @ {: o:IR-ID:ir-op-id :}
   o NFROZEN:OPCODE-AT SYM$ {: a:ptr u:n :}
   a 8 +  u 8 -  SB-APPEND
   o NUMBER? if  s" =" SB-APPEND  o VALUE FMT:SB-INT  then
   o KIND WSTRUCT:ADDR-DATA = if s" @data" SB-APPEND then
   o KIND WSTRUCT:ADDR-CODE = if s" @code" SB-APPEND then
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

\ Function k of the module, every block in order.
: FUN$ ( IR-BUILD:module n -- ptr u8 n )
   {: m:IR-BUILD:module k:n :}
   m NFROZEN:VIEWS!
   NFROZEN:MKEY k IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f 0 NFROZEN:BLOCK-AT IR-ID:BLOCK-LOCAL FUN-B0 !
   SB-RESET
   f NFROZEN:BLOCK-COUNT 0 ?do  f i NFROZEN:BLOCK-AT BLOCK+  loop
   SB$ ;

\ Function k's signature as the type table renders it.
: SIG$ ( IR-BUILD:module n -- ptr u8 n )
   {: m:IR-BUILD:module k:n :}
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY  m IR-BUILD:FKEY k IR-ID:PACK-FUN
   IR-FUN:FSIGNATURE@ {: sig:IR-ID:ir-type-id :}
   m IR-BUILD:FTYPE-POOL m IR-BUILD:FTYPE-ROWS sig NB 256 IR-TYPE:FRENDER
   NB swap ;

\ The first call function zero makes, its blocks read in order.
variable FOUND?
1 TYPED-BUFFER FOUND IR-ID:ir-op-id

: CALL-SEEN ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   FOUND? @ 0<> if exit then
   o NFROZEN:OPCODE-AT SYM$ s" wstruct.call" STR= 0= if exit then
   o 0 FOUND !
   1 FOUND? ! ;

: BLOCK-CALLS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk NFROZEN:OP-COUNT 0 ?do  bk i NFROZEN:OP-AT CALL-SEEN  loop ;

\ Its callee's spelling.
: CALLEE$ ( IR-BUILD:module -- ptr u8 n )
   {: m:IR-BUILD:module :}
   m NFROZEN:VIEWS!
   0 FOUND? !
   NFROZEN:MKEY 0 IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f NFROZEN:BLOCK-COUNT 0 ?do  f i NFROZEN:BLOCK-AT BLOCK-CALLS  loop
   FOUND? @ 0= if s" no call" exit then
   0 FOUND @ {: o:IR-ID:ir-op-id :}
   NFROZEN:V-OPP NFROZEN:VW NFROZEN:V-OPR NFROZEN:VW NFROZEN:MKEY
   o  o s" wstruct.callee" KEY-AT  IR-OP:FATTR@ {: at:IR-ID:ir-attr-id :}
   NFROZEN:V-ATTR NFROZEN:VW NFROZEN:MKEY at IR-ATTR:FSYM@ SYM$ ;

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

: SHAPE-RUN ( IR-CTX:ctx -- )    COMPILE 0 FUN$ KEEP ;
: SIG-RUN ( IR-CTX:ctx -- )      COMPILE 0 SIG$ KEEP ;
: CALLEE-RUN ( IR-CTX:ctx -- )   COMPILE CALLEE$ KEEP ;

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

\ The source, its arity and the signature function zero must declare.
: SIG-ROW ( ptr u8 n n n ptr u8 n -- )
   {: src:ptr sn:n in:n out:n want:ptr wn:n :}
   src sn in out SOURCE!
   [: SIG-RUN ;] OUTCOME
   want wn KEPT ;

\ What function zero of the last module selected takes and leaves, and whether
\ that is the frame.
: ARITY-IS ( n n bool -- )
   {: in:n out:n fr:bool :}
   0 WSEL:ARITY {: gi:n go:n :}
   gi in T=  go out T=
   in out WSEL:FRAMED?  fr if TTRUE else TFALSE then ;

: INTEGER-ROWS ( -- )
   s" wrapping add: the lanes, the entry block and status 0" T-LABEL
   s" PA +" 2 1
   s" 4: br>1 | 2: i64.add i32.const=0 return | " ROW
   s" a compare's mask is 0 - extend_u(p) and brz tests the whole cell" T-LABEL
   s" PE < if 1 else 2 then" 2 1
   s" 4: br>1 | 2: i64.lt_s i64.extend_i32_u i64.const=0 i64.sub i64.eqz brz>3,2 | 0: br>4 | 0: i64.const=1 br>5 | 0: i64.const=2 br>5 | 1: i32.const=0 return | " ROW
   s" invert, rshift and and" T-LABEL
   s" PI invert 3 rshift -1 and" 1 1
   s" 3: br>1 | 1: i64.const=-1 i64.xor i64.const=3 i64.shr_u i64.const=-1 i64.and i32.const=0 return | " ROW
   s" division tests zero, then -1, before any i64.div_s" T-LABEL
   s" PC 7 /" 1 1
   s" 3: br>1 | 1: i64.const=7 i64.eqz brz>3,2 | 0: i64.const=-6400 i64.store br>7 | 0: i64.const=-1 i64.eq brz>4,5 | 0: i64.div_s br>6 | 0: i64.const=0 i64.sub br>6 | 1: i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW
   s" mod is the division then elaborate.f's multiply and subtract" T-LABEL
   s" PD mod" 2 1
   s" 4: br>1 | 2: i64.eqz brz>3,2 | 0: i64.const=-6400 i64.store br>7 | 0: i64.const=-1 i64.eq brz>4,5 | 0: i64.div_s br>6 | 0: i64.const=0 i64.sub br>6 | 1: i64.mul i64.sub i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW ;

\ A loop header without a token argument, entered first from the block before
\ the loop and again by the backedge, whose division's zero path stores with
\ the token it enters with: a token read off the backedge, a block not yet
\ selected, is refused.
: LOOP-ROWS ( -- )
   s" a loop header's token is the one the loop is entered with" T-LABEL
   s" PL begin 7 / dup 0= until" 1 1
   s" 3: br>1 | 1: br>2 | 1: i64.const=7 i64.eqz brz>4,3 | 0: i64.const=-6400 i64.store br>10 | 0: i64.const=-1 i64.eq brz>5,6 | 0: i64.div_s br>7 | 0: i64.const=0 i64.sub br>7 | 1: i64.const=0 i64.eq i64.extend_i32_u i64.const=0 i64.sub i64.eqz brz>9,8 | 0: br>2 | 0: i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW ;

\ A call's host target is `host` and the entry the native call site records.
: HOST$ ( ptr u8 n -- ptr u8 n )
   NDICT:CALL-TARGET {: e:n :}
   SB-RESET  s" host " SB-APPEND  e FMT:SB-U  SB$ ;

: CALL-ROWS ( -- )
   s" a wordcall stores the kept row, tests status and reloads it" T-LABEL
   s" PB SEL-INC +" 3 2
   s" 5: br>1 | 3: i32.load i32.const=16 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*2 i32.store call brz>4,5 | 0: i64.load*2 i32.store i64.add i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW
   s" a wordcall names its host entry" T-LABEL
   s" PB SEL-INC +" 3 2 SOURCE!
   [: CALLEE-RUN ;] OUTCOME
   s" SEL-INC" HOST$ KEPT
   s" throw is a terminal: the row stored, status 1 propagated, a return a fault" T-LABEL
   s" PF throw" 1 0
   s" 3: br>1 | 1: i32.load i32.const=8 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store i32.store call brz>4,5 | 0: i32.const=88 i64.const=0 i32.store i64.store unreachable | 0: i32.const=1 return | " ROW
   s" a return from a dead callee is a trap: kind and message recorded, unreachable" T-LABEL
   s" PH SEL-BOOM" 1 0
   s" 3: br>1 | 1: call brz>2,3 | 0: i64.const@data i64.const=21 i64.const=88 i32.wrap_i64 i32.store i64.store unreachable | 0: i32.const=1 return | " ROW
   s" a tail RECURSE is a call then a return" T-LABEL
   s" PG dup 0= if drop 1 exit then 1 - RECURSE" 1 1
   s" 3: br>1 | 1: i64.const=0 i64.eq i64.extend_i32_u i64.const=0 i64.sub i64.eqz brz>3,2 | 0: br>4 | 0: i64.const=1 br>6 | 2: i64.const=1 i64.sub call brz>5,7 | 0: br>6 | 2: i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW
   s" RECURSE names the definition itself" T-LABEL
   s" PG dup 0= if drop 1 exit then 1 - RECURSE" 1 1 SOURCE!
   [: CALLEE-RUN ;] OUTCOME
   s" PG" KEPT ;

\ W06, the direct half: sixteen lanes each way, one more and the frame.
: FRAME-ROWS ( -- )
   s" sixteen inputs are lanes" T-LABEL
   s" PV + + + + + + + + + + + + + + +" 16 1
   s" 18: br>1 | 16: i64.add*15 i32.const=0 return | " ROW
   16 1 false ARITY-IS
   s" PV + + + + + + + + + + + + + + +" 16 1
   s" ( i32 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 -- i32 i64 )" SIG-ROW
   s" seventeen inputs come off the frame, the output goes onto it" T-LABEL
   s" PW + + + + + + + + + + + + + + + +" 17 1
   s" 2: i32.load i32.const=136 i32.sub i64.load*17 i32.store br>1 | 17: i64.add*16 i32.load i32.const=8 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store i32.store i32.const=0 return | " ROW
   17 1 true ARITY-IS
   s" PW + + + + + + + + + + + + + + + +" 17 1
   s" ( i32 -- i32 )" SIG-ROW
   s" seventeen outputs go onto the frame" T-LABEL
   s" PY dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup" 1 17
   s" 2: i32.load i32.const=8 i32.sub i64.load i32.store br>1 | 1: i32.load i32.const=136 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*17 i32.store i32.const=0 return | " ROW
   1 17 true ARITY-IS
   s" a call to a seventeen-input callee passes its arguments in the frame" T-LABEL
   s" PX 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 SEL-SUM17" 0 1
   s" 2: br>1 | 0: i64.const=1 i64.const=2 i64.const=3 i64.const=4 i64.const=5 i64.const=6 i64.const=7 i64.const=8 i64.const=9 i64.const=10 i64.const=11 i64.const=12 i64.const=13 i64.const=14 i64.const=15 i64.const=16 i64.const=17 i32.load i32.const=136 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*17 i32.store call brz>4,5 | 0: i64.load i32.store i32.const=0 return | 0: i32.const=1 i64.const=0 return | " ROW ;

\ Every store to the context stack first tests where its cells end against the
\ stack region's end, static data at $31000 (200704): past it, the block built
\ next records STACK-BOUNDS (102) and that end, and executes unreachable.
: STACK-ROWS ( -- )
   s" a kept row is stored only where it ends inside the stack region" T-LABEL
   s" PK 1 2 depth drop 2drop" 0 0
   s" 2: br>1 | 0: i64.const=1 i64.const=2 i32.load i32.const=16 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*2 i32.store call brz>4,5 | 0: i64.load*2 i32.store i32.const=0 return | 0: i32.const=1 return | " ROW
   s" a framed output is stored only where it ends inside the stack region" T-LABEL
   s" PU" 17 17
   s" 2: i32.load i32.const=136 i32.sub i64.load*17 i32.store br>1 | 17: i32.load i32.const=136 i32.add i64.extend_i32_u i64.const=200704 i64.gt_s brz>3,2 | 0: i32.const=102 i32.store i64.store unreachable | 0: i64.store*17 i32.store i32.const=0 return | " ROW ;

\ ---- the refusals -------------------------------------------------------------
: REFUSED ( ptr u8 n n n n -- )
   {: src:ptr sn:n in:n out:n code:n :}
   src sn in out SOURCE!
   [: [: COMPILE drop ;] WRAPPED ;] code TTHROWSQ ;

: TRAP-RUN ( -- )
   CNUM-OVERFLOW:TRAP [: COMPILE drop ;] BOUND ;

\ A second selection of one frozen module, with no DECLARE of its own.
: AGAIN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c FROZEN {: m:IR-BUILD:module :}
   DECLARED
   SES @ m NBACK:SELECT drop
   SES @ m NBACK:SELECT drop ;

\ The module a selection answers, handed to selection.
: RESELECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c COMPILE {: w:IR-BUILD:module :}
   DECLARED
   SES @ w NBACK:SELECT drop ;

\ `PZ ( n -- )`, a call the elaborator never writes: built through the staged
\ builder and frozen by NBACK:FREEZE, whose interim freeze checks no
\ operation's shape. Its wordcall's callee takes one cell and leaves Z-OUT, and
\ the call carries Z-OPS copies of the argument and Z-RES cell results, each
\ after its token.
variable Z-OPS
variable Z-RES
variable Z-OUT
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

\ The first token, the wordcall on it and the argument, and a bare return.
: Z-BODY ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:MEM Z-OPEN
   ZC ZB  ZC ZB HIR:MEM-TYPE  IR-BUILD:ADD-RESULT
   ZC ZB IR-BUILD:END-OP {: m:IR-ID:ir-op-id :}
   HIR-OPCODE:WORDCALL Z-OPEN
   ZC ZB  ZC ZB m 0 IR-BUILD:OP-RESULT@  IR-BUILD:ADD-OPERAND
   Z-OPS @ 0 ?do  ZC ZB a IR-BUILD:ADD-OPERAND  loop
   ZC ZB  ZC ZB HIR:MEM-TYPE  IR-BUILD:ADD-RESULT
   Z-RES @ 0 ?do  ZC ZB  ZC ZB HIR:CELL-TYPE  IR-BUILD:ADD-RESULT  loop
   ZC ZB HIR:KEY-ENTRY  s" SEL-INC" NDICT:CALL-TARGET  Z-INT
   ZC ZB HIR:KEY-IN  1  Z-INT
   ZC ZB HIR:KEY-OUT  Z-OUT @  Z-INT
   ZC ZB IR-BUILD:END-OP drop
   HIR-OPCODE:RETURN Z-OPEN
   ZC ZB IR-BUILD:END-OP drop ;

: Z-SOURCE ( IR-CTX:ctx ptr u8 n -- )
   {: c:IR-CTX:ctx nm:ptr nn:n :}
   c 0 Z-CTX !
   c NSRC:HIR-BUILDER 0 Z-BLD !
   ZB  ZC ZB nm nn IR-BUILD:ADD-SOURCE  0 nn IR-BUILD:ADD-SPAN  0 Z-SPAN ! ;

\ The function closed, frozen by NBACK:FREEZE, declared and selected.
: Z-SELECT ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   ZC ZB IR-BUILD:END-BLOCK drop
   ZC ZB IR-BUILD:END-FUN drop
   SES @ ZB NBACK:FREEZE {: m:IR-BUILD:module :}
   DECLARED
   SES @ m NBACK:SELECT drop ;

: SHAPED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c s" PZ" Z-SOURCE
   s" PZ" 1 0 Z-BEGIN
   ZC ZB  ZC ZB HIR:CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG  Z-BODY
   c Z-SELECT ;

\ PZ's call shaped as stated, refused by its code.
: SHAPED ( n n n -- )
   {: ops:n res:n out:n :}
   ops Z-OPS !  res Z-RES !  out Z-OUT !
   1 DECL-IN !  0 DECL-OUT !
   [: [: SHAPED-BODY ;] WRAPPED ;] E-WSEL-CALL TTHROWSQ ;

\ `PV ( -- 17 cells )`, framed, with a bare return: its signature states no
\ output lane, so only the selector can see the seventeen cells it never wrote.
: BARE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c s" PV" Z-SOURCE
   s" PV" 0 17 Z-BEGIN
   HIR-OPCODE:RETURN Z-OPEN
   ZC ZB IR-BUILD:END-OP drop
   c Z-SELECT ;

: REFUSAL-ROWS ( -- )
   s" a load is the checked-memory sibling's, refused by name" T-LABEL
   s" PR @" 1 1 E-WSEL-REFUSED REFUSED
   s" an f64 operation is the float sibling's, refused by name" T-LABEL
   s" PS s>f f>s" 1 1 E-WSEL-REFUSED REFUSED
   s" an add a trapping unit may trap on is refused, never wrapped" T-LABEL
   s" PT +" 2 1 SOURCE!
   [: TRAP-RUN ;] E-WSEL-TRAP TTHROWSQ
   s" a selection takes its own DECLARE: a second one without is refused" T-LABEL
   s" PA +" 2 1 SOURCE!
   [: [: AGAIN-BODY ;] WRAPPED ;] E-WSEL-DECLARE TTHROWSQ
   s" a DECLARE whose arity is not the definition's is refused" T-LABEL
   s" PA +" 2 1 SOURCE!
   1 DECL-IN !
   [: [: COMPILE drop ;] WRAPPED ;] E-WSEL-DECLARE TTHROWSQ
   s" a module that is not HIR is refused" T-LABEL
   s" PA +" 2 1 SOURCE!
   [: [: RESELECT-BODY ;] WRAPPED ;] E-WSEL-SOURCE TTHROWSQ
   s" a call whose results are not its token, kept row and outputs is refused" T-LABEL
   1 1 0 SHAPED
   s" a call with fewer operands than its token and arguments is refused" T-LABEL
   0 0 1 SHAPED
   s" a framed return that leaves none of its 17 output cells is refused" T-LABEL
   0 DECL-IN !  17 DECL-OUT !
   [: [: BARE-BODY ;] WRAPPED ;] E-WSEL-DECLARE TTHROWSQ ;

\ ---- the admission matrix -------------------------------------------------------
\ Every HIR opcode answers: selected here, or the sibling that selects it.
variable N-SEL
variable N-TAIL
variable N-MEM
variable N-FLT
variable N-DYN

: ADMIT+ ( WSEL:admission -- )
   MATCH WSEL:admission
      selected       OF 1 N-SEL +! ENDOF
      unbounded-tail OF 1 N-TAIL +! ENDOF
      memory         OF 1 N-MEM +! ENDOF
      float          OF 1 N-FLT +! ENDOF
      dynamic        OF 1 N-DYN +! ENDOF
   ;MATCH ;

: COUNTS-RESET ( -- )
   0 N-SEL !  0 N-TAIL !  0 N-MEM !  0 N-FLT !  0 N-DYN ! ;

: ADMISSION-ROWS ( -- )
   COUNTS-RESET
   HIR:OPCODES 0 ?do  i HIR:NTH WSEL:ADMISSION ADMIT+  loop
   s" every HIR opcode is selected or names its sibling" T-LABEL
   N-SEL @ N-TAIL @ + N-MEM @ + N-FLT @ + N-DYN @ +  HIR:OPCODES T=
   s" calls, status and integers: 25 selected here" T-LABEL
   N-SEL @ N-TAIL @ + 25 T=
   s" checked memory, f64 and the quotation descriptor are the siblings'" T-LABEL
   N-MEM @ 4 T=  N-FLT @ 17 T=  N-DYN @ 1 T=
   s" the matrix says a tail call, self or host, is not bounded space" T-LABEL
   N-TAIL @ 2 T=
   COUNTS-RESET
   HIR-OPCODE:CALL WSEL:ADMISSION ADMIT+
   HIR-OPCODE:WORDCALL WSEL:ADMISSION ADMIT+
   N-TAIL @ 2 T= ;

public
: RUN ( -- )
   INSTALL
   ADMISSION-ROWS
   INTEGER-ROWS
   LOOP-ROWS
   CALL-ROWS
   FRAME-ROWS
   STACK-ROWS
   REFUSAL-ROWS
   T-REPORT ;

;package

WSEL-TEST:RUN
