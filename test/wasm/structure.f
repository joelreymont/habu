\ structure.f - WCTL, src/arch/wasm/structure.f, on the product engine.
\
\ Proves W01 and W02 of docs/portability.md section 25 as structural rows (the
\ rows dot executes them): a loop edge that swaps two arguments, fans one out
\ and rotates three doubles is written as parallel copies through one
\ temporary per type, and an argument it hands back unchanged is not copied;
\ two loops nested in each other, with ifs whose arms meet and a branch out of
\ both loops, give the block, loop and if nesting and the label depth of every
\ branch, with the copies of each edge and none for a memory token, while a
\ block the entry never reaches is written nowhere; of two merges one block
\ dominates, the later in reverse postorder is the outer block; the second and
\ third functions of a module are structured in their own block windows;
\ dominators and loop headers read back per block; a function whose cycle the
\ entry enters at two blocks is refused by name with the span of the block it
\ was entered at; and a block, step or temporary outside what the last function
\ structured holds is refused.
\
\ THE TRACE. A structured function reads back as one line, a word per step:
\ bN the body of block N, loopN and blockN a label for block N, if:C an if on
\ the value C, else, end, D=S a copy, $K=S a save into temporary K, D=$K a
\ restore, brN:K a branch to the label of block N, K labels out, and fin a
\ return or a trap. A value is named by the fixture that made it.
\
\ ONE FIXTURE PER CONTEXT. A module holds about seventeen arenas and the live
\ arena registry holds sixty-four, so every module below is built in its own
\ context.

require lib/test.f
require lib/string.f
require src/compiler/numeric-policy.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/source.f
require src/compiler/ir/type.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f
require src/compiler/native/frozen.f
require src/compiler/native/backend.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/structure.f

package WASM-STRUCTURE-TEST
private

\ ---- bindings ----------------------------------------------------------------
: POLICY ( -- CNUM:numeric-policy )
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY ;

: BND ( -- CBIND:binding )
   CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH {: f:CTARGET:features :}
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32 f CTARGET:CONTRACT POLICY CBIND:BIND ;

\ The row the backend's INSTALL registers, standing in here: it lowers every
\ Wasm contract and emits none.
: ALWAYS ( CTARGET:contract -- bool ) drop true ;
: NEVER ( CTARGET:contract -- bool ) drop false ;
: NO-REWRITE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) nip ;
: NO-EMIT ( IR-CTX:ctx IR-BUILD:module n -- ) 2drop drop ;
: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- ) E-CTGT-UNLOADED throw ;
: NO-STAGE ( -- ) ;
: NO-PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   2drop 2drop ;
: NO-DECLARE ( n n NBACK:linkage -- ) 2drop drop ;

: REGISTER-WASM ( -- )
   71 CTARGET:ID CTARGET-ARCH:WASM [: ALWAYS ;] [: NEVER ;]
   CTARGET-BACKEND:MAKE
   [: NO-DECLARE ;] [: NO-REWRITE ;] [: NO-REWRITE ;] [: NO-REWRITE ;]
   [: NO-EMIT ;] [: NO-UNPLACED ;] [: NO-STAGE ;] [: NO-STAGE ;]
   [: NO-PROTOTYPE ;] [: NO-STAGE ;] [: NO-STAGE ;] NBACK-PASS:MAKE
   NBACK:REGISTER ;

\ ---- module rigging ----------------------------------------------------------
: BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   IR-BUILD:PLAN-DEFAULT
   WSTRUCT:NEW-BUILDER ;

\ Block N's span is the text bNN at byte 4N, so a refusal's span says which
\ block it names.
: SPAN ( IR-CTX:ctx IR-BUILD:builder n -- IR-SOURCE:span )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   b  c b s" b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11" IR-BUILD:ADD-SOURCE
   k 4 *  3  IR-BUILD:ADD-SPAN ;

: I32 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:I32-TYPE ;
: I64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:I64-TYPE ;
: F64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:F64-TYPE ;
: MEM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:MEM-TYPE ;

\ (ctx:i32, x:i64) -> (status:i32, out:i64), the Habu call's row.
: SIG ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b I32 {: w:IR-ID:ir-type-id :}
   c b I64 {: x:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   w IR-TYPE:FN-PARAM
   x IR-TYPE:FN-PARAM
   w IR-TYPE:FN-RESULT
   x IR-TYPE:FN-RESULT
   c b IR-BUILD:INTERN-CODE-REF ;

: FN-OPEN ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:ptr u:n :}
   c b  c b a u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   c b  c b SIG  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:WASM IR-BUILD:SET-CONVENTION
   c b  c b 0 SPAN  IR-BUILD:SET-FUN-SPAN ;

\ Block N of the module, opened in ordinal order.
: BLK-OPEN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b k SPAN  IR-BUILD:SET-BLOCK-SPAN ;

: ARG ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- IR-ID:ir-value-id )
   IR-BUILD:ADD-BLOCK-ARG ;

\ The entry block: the arguments SIG declares and the memory token.
: ENTRY ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b k BLK-OPEN
   c b  c b I32  ARG drop
   c b  c b I64  ARG
   c b  c b MEM  ARG ;

: SHUT ( IR-CTX:ctx IR-BUILD:builder -- )
   IR-BUILD:END-BLOCK drop ;

\ ---- appending operations ----------------------------------------------------
: OPEN ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode :}
   c b  c b o WSTRUCT:ENSURE-OP  IR-BUILD:BEGIN-OP
   c b  c b 0 SPAN  IR-BUILD:SET-OP-SPAN ;

: USE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- )
   IR-BUILD:ADD-OPERAND ;

: END1 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b t IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP {: o:IR-ID:ir-op-id :}
   c b o 0 IR-BUILD:OP-RESULT@ ;

: K ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-type-id n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode t:IR-ID:ir-type-id v:n :}
   c b o OPEN
   c b  c b WSTRUCT:KEY-VALUE  c b v IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   c b t END1 ;

: OP1 ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-value-id IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode v:IR-ID:ir-value-id
      t:IR-ID:ir-type-id :}
   c b o OPEN
   c b v USE
   c b t END1 ;

: OP2 ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode v:IR-ID:ir-value-id
      u:IR-ID:ir-value-id t:IR-ID:ir-type-id :}
   c b o OPEN
   c b v USE
   c b u USE
   c b t END1 ;

: I64-1 ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:n :}
   c b WSTRUCT-OPCODE:I64-CONST  c b I64  v K ;

: EQZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I64-EQZ v  c b I32  OP1 ;

\ A br is opened, handed its operands with USE, and closed by TO naming block N.
: BR ( IR-CTX:ctx IR-BUILD:builder -- )
   WSTRUCT-OPCODE:BR OPEN ;

: TO ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b  b IR-BUILD:MODULE-KEY k IR-ID:PACK-BLOCK  IR-BUILD:ADD-SUCCESSOR
   c b IR-BUILD:END-OP drop ;

\ To block Z when the i32 condition is zero, to block NZ otherwise.
: BRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id z:n nz:n :}
   c b WSTRUCT-OPCODE:BRZ OPEN
   c b v USE
   c b  b IR-BUILD:MODULE-KEY z IR-ID:PACK-BLOCK  IR-BUILD:ADD-SUCCESSOR
   c b nz TO ;

\ Return status 0 and the lane r.
: RET ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: s:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:RETURN OPEN
   c b s USE
   c b r USE
   c b IR-BUILD:END-OP drop ;

\ ---- reading back ------------------------------------------------------------
1 TYPED-BUFFER FIX IR-BUILD:module

: FUN ( IR-BUILD:module n -- IR-ID:ir-fun-id )
   {: m:IR-BUILD:module k:n :}
   m IR-BUILD:FKEY k IR-ID:PACK-FUN ;

: BLOCK ( IR-BUILD:module n -- IR-ID:ir-block-id )
   {: m:IR-BUILD:module k:n :}
   m IR-BUILD:FKEY k IR-ID:PACK-BLOCK ;

: IDOM# ( IR-BUILD:module n -- n )
   BLOCK WCTL:IDOM IR-ID:BLOCK-LOCAL ;

: HEADER# ( IR-BUILD:module n -- bool )
   BLOCK WCTL:HEADER? ;

\ ---- value names -------------------------------------------------------------
\ Up to two letters, packed into a cell indexed by the value's ordinal.
128 TYPED-BUFFER NAMES n

: NAMES-CLEAR ( -- )
   128 0 do 0 i NAMES ! loop ;

: NAMED ( IR-ID:ir-value-id ptr u8 n -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id a:ptr u:n :}
   a c@  u 1 > if a 1 + c@ 256 * + then
   v IR-ID:VALUE-LOCAL NAMES !
   v ;

\ ---- the trace ---------------------------------------------------------------
1024 constant CAP
CAP BUFFER: GOT-BUF
TYPED-VARIABLE GOT-U len
CAP BUFFER: WANT-BUF
TYPED-VARIABLE WANT-U len

: C+ ( n -- )
   GOT-BUF CAP GOT-U BUF-APPEND-C ;

: S+ ( ptr u8 n -- )
   GOT-BUF CAP GOT-U BUF-APPEND ;

: DEC+ ( n -- )
   {: v:n :}
   v 10 >= if v 10 / RECURSE then
   v 10 mod [char] 0 + C+ ;

: NAME+ ( IR-ID:ir-value-id -- )
   IR-ID:VALUE-LOCAL NAMES @ {: k:n :}
   k 0= if [char] ? C+ exit then
   k 256 mod C+
   k 256 / {: hi:n :}
   hi 0<> if hi C+ then ;

: BLK+ ( IR-ID:ir-block-id -- )
   IR-ID:BLOCK-LOCAL DEC+ ;

: STEP+ ( WCTL:step -- )
   MATCH WCTL:step
      open-block OF s" block" S+ BLK+ ENDOF
      open-loop OF s" loop" S+ BLK+ ENDOF
      open-if OF s" if:" S+ NAME+ ENDOF
      else-arm OF s" else" S+ ENDOF
      close OF s" end" S+ ENDOF
      body OF s" b" S+ BLK+ ENDOF
      final OF drop s" fin" S+ ENDOF
      copy OF {: d:IR-ID:ir-value-id s:IR-ID:ir-value-id :}
         d NAME+ [char] = C+ s NAME+ ENDOF
      save OF {: t:n s:IR-ID:ir-value-id :}
         [char] $ C+ t DEC+ [char] = C+ s NAME+ ENDOF
      restore OF {: d:IR-ID:ir-value-id t:n :}
         d NAME+ s" =$" S+ t DEC+ ENDOF
      branch OF {: y:IR-ID:ir-block-id k:n :}
         s" br" S+ y BLK+ [char] : C+ k DEC+ ENDOF
   ;MATCH ;

: TRACE$ ( -- ptr u8 n )
   GOT-U BUF-RESET
   WCTL:STEPS 0 ?do
      i 0 > if STR-SPACE C+ then
      i WCTL:STEP@ STEP+
   loop
   GOT-BUF GOT-U @ LEN>N ;

: WANT-RESET ( -- )
   WANT-U BUF-RESET ;

: W+ ( ptr u8 n -- )
   WANT-BUF CAP WANT-U BUF-APPEND ;

: WANT$ ( -- ptr u8 n )
   WANT-BUF WANT-U @ LEN>N ;

\ ---- W01: a swap-cycle edge --------------------------------------------------
\ b0(ctx x m): fx = convert x; br b1(x x x fx fx fx x x m)
\ b1(a b c:i64 f g h:f64 n lm:i64 m1): zn = eqz n; brz zn -> b2, b3
\ b2: n1 = n - 1; br b1(b a a g h f n1 lm m1)        lm comes back unchanged
\ b3: return 0 c
: SWAP-B0 ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b 0 ENTRY {: x:IR-ID:ir-value-id m:IR-ID:ir-value-id :}
   x s" x" NAMED drop
   c b WSTRUCT-OPCODE:F64-CONVERT-I64-S x  c b F64  OP1 s" fx" NAMED
   {: fx:IR-ID:ir-value-id :}
   c b BR
   c b x USE  c b x USE  c b x USE
   c b fx USE  c b fx USE  c b fx USE
   c b x USE  c b x USE  c b m USE
   c b 1 TO
   c b SHUT ;

: SWAP-LOOP ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b 1 BLK-OPEN
   c b  c b I64  ARG s" a" NAMED {: va:IR-ID:ir-value-id :}
   c b  c b I64  ARG s" b" NAMED {: vb:IR-ID:ir-value-id :}
   c b  c b I64  ARG s" c" NAMED {: vc:IR-ID:ir-value-id :}
   c b  c b F64  ARG s" f" NAMED {: vf:IR-ID:ir-value-id :}
   c b  c b F64  ARG s" g" NAMED {: vg:IR-ID:ir-value-id :}
   c b  c b F64  ARG s" h" NAMED {: vh:IR-ID:ir-value-id :}
   c b  c b I64  ARG s" n" NAMED {: vn:IR-ID:ir-value-id :}
   c b  c b I64  ARG s" lm" NAMED {: vl:IR-ID:ir-value-id :}
   c b  c b MEM  ARG {: m:IR-ID:ir-value-id :}
   c b  c b vn EQZ  s" zn" NAMED  2 3 BRZ
   c b SHUT
   c b 2 BLK-OPEN
   c b WSTRUCT-OPCODE:I64-SUB vn  c b 1 I64-1  c b I64  OP2 s" n1" NAMED
   {: n1:IR-ID:ir-value-id :}
   c b BR
   c b vb USE  c b va USE  c b va USE
   c b vg USE  c b vh USE  c b vf USE
   c b n1 USE  c b vl USE  c b m USE
   c b 1 TO
   c b SHUT
   vc ;

: SWAP-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" swap" FN-OPEN
   c b SWAP-B0
   c b SWAP-LOOP {: vc:IR-ID:ir-value-id :}
   c b 3 BLK-OPEN
   c b vc RET
   c b SHUT
   c b IR-BUILD:END-FUN drop ;

: SWAP-WANT ( -- ptr u8 n )
   WANT-RESET
   s" b0 a=x b=x c=x f=fx g=fx h=fx n=x lm=x loop1 b1 if:zn b3 fin else " W+
   s" b2 c=a n=n1 $0=a a=b b=$0 $1=f f=g g=h h=$1 br1:1 end end" W+
   WANT$ ;

: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c BUILDER {: b:IR-BUILD:builder :}
   NAMES-CLEAR
   c b SWAP-FN
   c b I64 {: ti:IR-ID:ir-type-id :}
   c b F64 {: tf:IR-ID:ir-type-id :}
   c b WSTRUCT:FREEZE {: m:IR-BUILD:module :}
   m  m 0 FUN  WCTL:STRUCTURE
   s" W01: the swap, the fan-out and the rotation copy through temporaries" T-LABEL
   TRACE$ SWAP-WANT T$=
   s" W01: one temporary per type, the i64 cycle's first" T-LABEL
   WCTL:TEMPS 2 T=
   0 WCTL:TEMP-TYPE ti NFROZEN:SAME-TYPE? TTRUE
   1 WCTL:TEMP-TYPE tf NFROZEN:SAME-TYPE? TTRUE
   s" W01: the loop's header is the block its back edge enters" T-LABEL
   m 1 HEADER# TTRUE
   m 2 HEADER# TFALSE ;

: SWAP-CASE ( -- )
   BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- W01: nested loops and ifs -----------------------------------------------
\ b0(ctx x m): one = 1; br b1(x x m)
\ b1(i s m1): zi = eqz i; brz zi -> b2, b9           the outer loop
\ b2: br b3(i s m1)
\ b3(j t m2): zj = eqz j; brz zj -> b4, b8           the inner loop
\ b4: zl = eqz (j and one); brz zl -> b5, b6         odd or even
\ b5: t1 = t + j; br b7(t1 m2)
\ b6: t2 = t - j; zt = eqz t2; brz zt -> b10, b9     out of both loops at zero
\ b7(u m3): j1 = j - 1; br b3(j1 u m3)               the arms meet
\ b8: i1 = i - 1; br b1(i1 t m2)
\ b9: return 0 s                                     both loops leave here
\ b10: br b7(t2 m2)
\ b11: br b8                                         the entry never reaches it
: NEST-B0 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b 0 ENTRY {: x:IR-ID:ir-value-id m:IR-ID:ir-value-id :}
   x s" x" NAMED drop
   c b 1 I64-1 {: one:IR-ID:ir-value-id :}
   c b BR  c b x USE  c b x USE  c b m USE  c b 1 TO
   c b SHUT
   one ;

: NEST-B1 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b 1 BLK-OPEN
   c b  c b I64  ARG s" i" NAMED {: i:IR-ID:ir-value-id :}
   c b  c b I64  ARG s" s" NAMED {: s:IR-ID:ir-value-id :}
   c b  c b MEM  ARG {: m:IR-ID:ir-value-id :}
   c b  c b i EQZ  s" zi" NAMED  2 9 BRZ
   c b SHUT
   i s m ;

: NEST-B2 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder i:IR-ID:ir-value-id s:IR-ID:ir-value-id
      m:IR-ID:ir-value-id :}
   c b 2 BLK-OPEN
   c b BR  c b i USE  c b s USE  c b m USE  c b 3 TO
   c b SHUT ;

: NEST-B3 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b 3 BLK-OPEN
   c b  c b I64  ARG s" j" NAMED {: j:IR-ID:ir-value-id :}
   c b  c b I64  ARG s" t" NAMED {: t:IR-ID:ir-value-id :}
   c b  c b MEM  ARG {: m:IR-ID:ir-value-id :}
   c b  c b j EQZ  s" zj" NAMED  4 8 BRZ
   c b SHUT
   j t m ;

: NEST-B4 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder j:IR-ID:ir-value-id one:IR-ID:ir-value-id :}
   c b 4 BLK-OPEN
   c b WSTRUCT-OPCODE:I64-AND j one  c b I64  OP2 {: lo:IR-ID:ir-value-id :}
   c b  c b lo EQZ  s" zl" NAMED  5 6 BRZ
   c b SHUT ;

: NEST-B5 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-value-id j:IR-ID:ir-value-id
      m:IR-ID:ir-value-id :}
   c b 5 BLK-OPEN
   c b WSTRUCT-OPCODE:I64-ADD t j  c b I64  OP2 s" t1" NAMED {: t1:IR-ID:ir-value-id :}
   c b BR  c b t1 USE  c b m USE  c b 7 TO
   c b SHUT ;

: NEST-B6 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-value-id j:IR-ID:ir-value-id :}
   c b 6 BLK-OPEN
   c b WSTRUCT-OPCODE:I64-SUB t j  c b I64  OP2 s" t2" NAMED {: t2:IR-ID:ir-value-id :}
   c b  c b t2 EQZ  s" zt" NAMED  10 9 BRZ
   c b SHUT
   t2 ;

: NEST-B7 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder j:IR-ID:ir-value-id one:IR-ID:ir-value-id :}
   c b 7 BLK-OPEN
   c b  c b I64  ARG s" u" NAMED {: u:IR-ID:ir-value-id :}
   c b  c b MEM  ARG {: m:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I64-SUB j one  c b I64  OP2 s" j1" NAMED {: j1:IR-ID:ir-value-id :}
   c b BR  c b j1 USE  c b u USE  c b m USE  c b 3 TO
   c b SHUT ;

: NEST-B8 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder i:IR-ID:ir-value-id one:IR-ID:ir-value-id
      t:IR-ID:ir-value-id m:IR-ID:ir-value-id :}
   c b 8 BLK-OPEN
   c b WSTRUCT-OPCODE:I64-SUB i one  c b I64  OP2 s" i1" NAMED {: i1:IR-ID:ir-value-id :}
   c b BR  c b i1 USE  c b t USE  c b m USE  c b 1 TO
   c b SHUT ;

: NEST-B10 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t2:IR-ID:ir-value-id m:IR-ID:ir-value-id :}
   c b 10 BLK-OPEN
   c b BR  c b t2 USE  c b m USE  c b 7 TO
   c b SHUT ;

: NEST-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" nest" FN-OPEN
   c b NEST-B0 {: one:IR-ID:ir-value-id :}
   c b NEST-B1 {: i:IR-ID:ir-value-id s:IR-ID:ir-value-id m1:IR-ID:ir-value-id :}
   c b i s m1 NEST-B2
   c b NEST-B3 {: j:IR-ID:ir-value-id t:IR-ID:ir-value-id m2:IR-ID:ir-value-id :}
   c b j one NEST-B4
   c b t j m2 NEST-B5
   c b t j NEST-B6 {: t2:IR-ID:ir-value-id :}
   c b j one NEST-B7
   c b i one t m2 NEST-B8
   c b 9 BLK-OPEN  c b s RET  c b SHUT
   c b t2 m2 NEST-B10
   c b 11 BLK-OPEN  c b BR  c b 8 TO  c b SHUT
   c b IR-BUILD:END-FUN drop ;

: NEST-WANT ( -- ptr u8 n )
   WANT-RESET
   s" b0 i=x s=x loop1 block9 b1 if:zi br9:1 else b2 j=i t=s loop3 b3 if:zj " W+
   s" b8 i=i1 s=t br1:4 else block7 b4 if:zl b6 if:zt br9:6 else b10 u=t2 br7:2 end " W+
   s" else b5 u=t1 br7:1 end end b7 j=j1 t=u br3:1 end end end end b9 fin end" W+
   WANT$ ;

: NEST-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c BUILDER {: b:IR-BUILD:builder :}
   NAMES-CLEAR
   c b NEST-FN
   c b WSTRUCT:FREEZE {: m:IR-BUILD:module :}
   m  m 0 FUN  WCTL:STRUCTURE
   s" W01: nested loops and ifs give the nesting, the copies and every depth" T-LABEL
   TRACE$ NEST-WANT T$=
   s" W01: the copies need no temporary" T-LABEL
   WCTL:TEMPS 0 T=
   s" the loop headers are the blocks back edges enter" T-LABEL
   m 1 HEADER# TTRUE
   m 3 HEADER# TTRUE
   m 0 HEADER# TFALSE
   m 7 HEADER# TFALSE
   m 9 HEADER# TFALSE
   s" each block's immediate dominator, the entry its own" T-LABEL
   m 0 IDOM# 0 T=
   m 1 IDOM# 0 T=
   m 3 IDOM# 2 T=
   m 7 IDOM# 4 T=
   m 8 IDOM# 3 T=
   m 9 IDOM# 1 T=
   m 10 IDOM# 6 T=
   s" a block the entry never reaches is written nowhere and dominated by none" T-LABEL
   m 11 IDOM# 11 T=
   m 11 HEADER# TFALSE ;

: NEST-CASE ( -- )
   BND [: NEST-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- W02: two entries into one cycle -----------------------------------------
\ tangle, blocks 0-3: b1 and b2 form a cycle the entry enters at both.
\ b0(ctx x m): z = eqz x; brz z -> b1, b2
\ b1: br b2
\ b2: brz z -> b3, b1
\ b3: return 0 x
: TANGLE-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" tangle" FN-OPEN
   c b 0 ENTRY drop {: x:IR-ID:ir-value-id :}
   c b x EQZ {: z:IR-ID:ir-value-id :}
   c b z 1 2 BRZ
   c b SHUT
   c b 1 BLK-OPEN  c b BR  c b 2 TO  c b SHUT
   c b 2 BLK-OPEN  c b z 3 1 BRZ  c b SHUT
   c b 3 BLK-OPEN  c b x RET  c b SHUT
   c b IR-BUILD:END-FUN drop ;

\ plain, blocks 4-7: an if whose arms meet.
\ b4(ctx x m): z = eqz x; brz z -> b5, b6
\ b5: br b7(1 m)
\ b6: br b7(2 m)
\ b7(v m1): return 0 v
: PLAIN-ARM ( IR-CTX:ctx IR-BUILD:builder n n IR-ID:ir-value-id ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n v:n m:IR-ID:ir-value-id a:ptr u:n :}
   c b k BLK-OPEN
   c b v I64-1 a u NAMED {: w:IR-ID:ir-value-id :}
   c b BR  c b w USE  c b m USE  c b 7 TO
   c b SHUT ;

: PLAIN-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" plain" FN-OPEN
   c b 4 ENTRY {: x:IR-ID:ir-value-id m:IR-ID:ir-value-id :}
   c b  c b x EQZ  s" z" NAMED  5 6 BRZ
   c b SHUT
   c b 5 1 m s" k1" PLAIN-ARM
   c b 6 2 m s" k2" PLAIN-ARM
   c b 7 BLK-OPEN
   c b  c b I64  ARG s" v" NAMED {: v:IR-ID:ir-value-id :}
   c b  c b MEM  ARG drop
   c b v RET
   c b SHUT
   c b IR-BUILD:END-FUN drop ;

\ ladder, blocks 8-11: b10 and b11 are both merges b8 dominates.
\ b8(ctx x m): z = eqz x; brz z -> b9, b10
\ b9: brz z -> b10, b11
\ b10: br b11
\ b11: return 0 x
: LADDER-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" ladder" FN-OPEN
   c b 8 ENTRY drop {: x:IR-ID:ir-value-id :}
   c b x EQZ s" z" NAMED {: z:IR-ID:ir-value-id :}
   c b z 9 10 BRZ
   c b SHUT
   c b 9 BLK-OPEN  c b z 10 11 BRZ  c b SHUT
   c b 10 BLK-OPEN  c b BR  c b 11 TO  c b SHUT
   c b 11 BLK-OPEN  c b x RET  c b SHUT
   c b IR-BUILD:END-FUN drop ;

: LADDER-WANT ( -- ptr u8 n )
   WANT-RESET
   s" block11 block10 b8 if:z br10:1 else b9 if:z br11:3 else br10:2 end end " W+
   s" end b10 br11:0 end b11 fin" W+
   WANT$ ;

: TANGLE-RUN ( -- )
   0 FIX @  0 FIX @ 0 FUN  WCTL:STRUCTURE ;

\ A block of tangle while plain is the function structured.
: STRAY-RUN ( -- )
   0 FIX @ 1 IDOM# drop ;

\ A block of another module whose ordinal lies inside plain's window.
: FOREIGN-RUN ( -- )
   IR-ID:NEW-MODULE drop 5 IR-ID:PACK-BLOCK WCTL:HEADER? drop ;

: PAST-STEPS-RUN ( -- )
   WCTL:STEPS WCTL:STEP@ drop ;

: NO-TEMP-RUN ( -- )
   0 WCTL:TEMP-TYPE drop ;

\ Block 1 is the refused tangle's own: a stale window left by plain would
\ answer it, so only a refusal that clears the last structure refuses it.
: REFUSED-BLOCK-RUN ( -- )
   0 FIX @ 1 HEADER# drop ;

: SPELLED? ( IR-BUILD:module IR-ID:ir-symbol-id ptr u8 n -- bool )
   {: m:IR-BUILD:module s:IR-ID:ir-symbol-id a:ptr u:n :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS s a u IR-SYM:FEQ? ;

: TANGLE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c BUILDER {: b:IR-BUILD:builder :}
   NAMES-CLEAR
   c b TANGLE-FN
   c b PLAIN-FN
   c b LADDER-FN
   c b WSTRUCT:FREEZE {: m:IR-BUILD:module :}
   m 0 FIX !
   m  m 2 FUN  WCTL:STRUCTURE
   s" W01: of two merges one block dominates, the later is the outer block" T-LABEL
   TRACE$ LADDER-WANT T$=
   m  m 1 FUN  WCTL:STRUCTURE
   s" W01: a second function is structured in its own block window" T-LABEL
   TRACE$ s" block7 b4 if:z b6 v=k2 br7:1 else b5 v=k1 br7:1 end end b7 fin" T$=
   m 7 IDOM# 4 T=
   s" a block of another function is refused" T-LABEL
   [: STRAY-RUN ;] E-WCTL-RANGE TTHROWSQ
   s" a block of another module is refused" T-LABEL
   [: FOREIGN-RUN ;] E-WCTL-RANGE TTHROWSQ
   s" a step past the last is refused" T-LABEL
   [: PAST-STEPS-RUN ;] E-WCTL-RANGE TTHROWSQ
   s" a temporary the copies never made is refused" T-LABEL
   [: NO-TEMP-RUN ;] E-WCTL-RANGE TTHROWSQ
   s" W02: two entries into one cycle are refused" T-LABEL
   [: TANGLE-RUN ;] E-WCTL-IRREDUCIBLE TTHROWSQ
   s" W02: the refusal names the function" T-LABEL
   m WCTL:REFUSED-FUN s" tangle" SPELLED? TTRUE
   s" W02: the refusal names b1, where the cycle was entered, by its span" T-LABEL
   WCTL:REFUSED-SPAN IR-SOURCE:SPAN-START 4 T=
   WCTL:REFUSED-SPAN IR-SOURCE:SPAN-LEN 3 T=
   s" W02: a refused function leaves nothing to read" T-LABEL
   WCTL:STEPS 0 T=
   [: REFUSED-BLOCK-RUN ;] E-WCTL-RANGE TTHROWSQ ;

: TANGLE-CASE ( -- )
   BND [: TANGLE-BODY ;] IR-CTX:WITH-CONTEXT ;

public

: RUN ( -- )
   T-RESET
   REGISTER-WASM
   SWAP-CASE
   NEST-CASE
   TANGLE-CASE
   T-REPORT ;

;package

WASM-STRUCTURE-TEST:RUN
