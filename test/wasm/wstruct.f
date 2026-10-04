\ wstruct.f - the WSTRUCT dialect, src/arch/wasm/wstruct.f, on the product
\ engine.
\
\ Proves the contract the Wasm selector and encoder build on: with no Wasm
\ backend registered a WSTRUCT builder is refused by the registry; every opcode
\ of the closed vocabulary defines under a Wasm binding and round-trips through
\ its spelling; a module using every form - typed block arguments, a memory
\ token threaded from the entry block, loads and stores with their memarg, a
\ direct and an indirect call, a two-way branch and a trap - freezes through
\ WSTRUCT:FREEZE and reads back as a Wasm module whose double forms need scalar
\ floating point; the substrate refuses, by its own codes, a use its definition
\ does not dominate, a block with no terminator, a successor argument of the
\ wrong type or on a two-way branch, an opcode the module never registered, a
\ WSTRUCT operation under a binding that is not Wasm and an f64 form under a
\ Wasm contract without scalar floating point; and WSTRUCT itself refuses an
\ ordinal or spelling outside its vocabulary, a table of another dialect or
\ schema version, and a return or an entry block that is not the function's
\ declared signature, all of which the full freeze accepts.
\
\ ONE FIXTURE PER CONTEXT. A module holds about seventeen arenas and the live
\ arena registry holds sixty-four, so every module below is built in its own
\ context.

require lib/test.f
require src/compiler/native/abi.f
require src/compiler/native/hir.f
require src/arch/wasm/wstruct.f

package WASM-WSTRUCT-TEST
private

\ ---- bindings ----------------------------------------------------------------
: POLICY ( -- CNUM:numeric-policy )
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY ;

: WASM-CONTRACT ( CTARGET:features -- CTARGET:contract )
   {: f:CTARGET:features :}
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32 f CTARGET:CONTRACT ;

: BND ( -- CBIND:binding )
   CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH WASM-CONTRACT POLICY CBIND:BIND ;

\ A Wasm contract with no scalar floating point.
: INT-BND ( -- CBIND:binding )
   CTARGET:F-BASE WASM-CONTRACT POLICY CBIND:BIND ;

\ A registered row grants the dialect's capability; this suite does not run
\ the backend stages.
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
: MOD ( IR-CTX:ctx -- IR-BUILD:builder )
   IR-BUILD:PLAN-DEFAULT
   WSTRUCT:NEW-BUILDER ;

: SPAN ( IR-CTX:ctx IR-BUILD:builder -- IR-SOURCE:span )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   b  c b s" wstruct-test" IR-BUILD:ADD-SOURCE  0 4 IR-BUILD:ADD-SPAN ;

: I32 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:I32-TYPE ;
: I64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:I64-TYPE ;
: F64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:F64-TYPE ;
: MEM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:MEM-TYPE ;

\ (ctx:i32, x:i64) -> (status:i32, out:t), the Habu call's row when t is i64.
: ROW ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b I32 {: w:IR-ID:ir-type-id :}
   c b I64 {: x:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   w IR-TYPE:FN-PARAM
   x IR-TYPE:FN-PARAM
   w IR-TYPE:FN-RESULT
   t IR-TYPE:FN-RESULT
   c b IR-BUILD:INTERN-CODE-REF ;

: SIG ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b I64  ROW ;

: FN-OPEN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder s:IR-ID:ir-symbol-id t:IR-ID:ir-type-id :}
   c b s IR-BUILD:BEGIN-FUN
   c b t IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:WASM IR-BUILD:SET-CONVENTION
   c b  c b SPAN  IR-BUILD:SET-FUN-SPAN ;

\ The function f under a signature of the caller's.
: SIG-OPEN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b  c b s" f" IR-BUILD:INTERN-SYMBOL  t FN-OPEN ;

: MAIN-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b SIG  SIG-OPEN ;

: BLK-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b SPAN  IR-BUILD:SET-BLOCK-SPAN ;

: ARG ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- IR-ID:ir-value-id )
   IR-BUILD:ADD-BLOCK-ARG ;

\ The entry block, taking the arguments SIG declares and the memory token.
: ENTRY-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b BLK-OPEN
   c b  c b I32  ARG drop
   c b  c b I64  ARG drop
   c b  c b MEM  ARG drop ;

\ A block identity for an ordinal, which is how a branch names a destination
\ that is still being built.
: BLK ( IR-BUILD:builder n -- IR-ID:ir-block-id )
   {: b:IR-BUILD:builder ord:n :}
   b IR-BUILD:MODULE-KEY ord IR-ID:PACK-BLOCK ;

\ ---- appending operations ----------------------------------------------------
\ An operation of a WSTRUCT opcode, its schema materialised first.
: OPEN ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode :}
   c b  c b o WSTRUCT:ENSURE-OP  IR-BUILD:BEGIN-OP
   c b  c b SPAN  IR-BUILD:SET-OP-SPAN ;

: USE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- )
   IR-BUILD:ADD-OPERAND ;

: INT-ATTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-symbol-id v:n :}
   c b k  c b v IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR ;

: END1 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b t IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP {: o:IR-ID:ir-op-id :}
   c b o 0 IR-BUILD:OP-RESULT@ ;

: END0 ( IR-CTX:ctx IR-BUILD:builder -- )
   IR-BUILD:END-OP drop ;

: K ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-type-id n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode t:IR-ID:ir-type-id v:n :}
   c b o OPEN
   c b  c b WSTRUCT:KEY-VALUE  v INT-ATTR
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

: SEL ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode v:IR-ID:ir-value-id
      u:IR-ID:ir-value-id w:IR-ID:ir-value-id t:IR-ID:ir-type-id :}
   c b o OPEN
   c b v USE
   c b u USE
   c b w USE
   c b t END1 ;

: MEMARG ( IR-CTX:ctx IR-BUILD:builder n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder al:n off:n :}
   c b  c b WSTRUCT:KEY-ALIGN  al INT-ATTR
   c b  c b WSTRUCT:KEY-OFFSET  off INT-ATTR ;

\ An i64 load at an i32 address: the value, then the next memory token.
: LOAD64 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ID:ir-value-id m:IR-ID:ir-value-id
      off:n :}
   c b WSTRUCT-OPCODE:I64-LOAD OPEN
   c b a USE
   c b m USE
   c b 3 off MEMARG
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b  c b MEM  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP {: o:IR-ID:ir-op-id :}
   c b o 0 IR-BUILD:OP-RESULT@
   c b o 1 IR-BUILD:OP-RESULT@ ;

: STORE64 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ID:ir-value-id v:IR-ID:ir-value-id
      m:IR-ID:ir-value-id off:n :}
   c b WSTRUCT-OPCODE:I64-STORE OPEN
   c b a USE
   c b v USE
   c b m USE
   c b 3 off MEMARG
   c b  c b MEM  END1 ;

\ The results of a call with one output lane: status, token, lane.
: CALL-END ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b I32  IR-BUILD:ADD-RESULT
   c b  c b MEM  IR-BUILD:ADD-RESULT
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP {: o:IR-ID:ir-op-id :}
   c b o 0 IR-BUILD:OP-RESULT@
   c b o 1 IR-BUILD:OP-RESULT@
   c b o 2 IR-BUILD:OP-RESULT@ ;

: CALL1 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-symbol-id -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder x:IR-ID:ir-value-id m:IR-ID:ir-value-id
      v:IR-ID:ir-value-id f:IR-ID:ir-symbol-id :}
   c b WSTRUCT-OPCODE:CALL OPEN
   c b x USE
   c b m USE
   c b v USE
   c b  c b WSTRUCT:KEY-CALLEE  c b f IR-BUILD:INTERN-SYMBOL-ATTR  IR-BUILD:ADD-ATTR
   c b CALL-END ;

: CALL-IND1 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder x:IR-ID:ir-value-id s:IR-ID:ir-value-id
      m:IR-ID:ir-value-id v:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:CALL-INDIRECT OPEN
   c b x USE
   c b s USE
   c b m USE
   c b v USE
   c b CALL-END ;

: BR0 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-block-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder d:IR-ID:ir-block-id :}
   c b WSTRUCT-OPCODE:BR OPEN
   c b d IR-BUILD:ADD-SUCCESSOR
   c b END0 ;

: BR1 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-block-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id d:IR-ID:ir-block-id :}
   c b WSTRUCT-OPCODE:BR OPEN
   c b v USE
   c b d IR-BUILD:ADD-SUCCESSOR
   c b END0 ;

: BR3 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-block-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder s:IR-ID:ir-value-id r:IR-ID:ir-value-id
      m:IR-ID:ir-value-id d:IR-ID:ir-block-id :}
   c b WSTRUCT-OPCODE:BR OPEN
   c b s USE
   c b r USE
   c b m USE
   c b d IR-BUILD:ADD-SUCCESSOR
   c b END0 ;

\ The first destination when the i32 condition is zero, the second otherwise.
: BRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-block-id IR-ID:ir-block-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id z:IR-ID:ir-block-id
      nz:IR-ID:ir-block-id :}
   c b WSTRUCT-OPCODE:BRZ OPEN
   c b v USE
   c b z IR-BUILD:ADD-SUCCESSOR
   c b nz IR-BUILD:ADD-SUCCESSOR
   c b END0 ;

: RET ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder s:IR-ID:ir-value-id r:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:RETURN OPEN
   c b s USE
   c b r USE
   c b END0 ;

: TRAP ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b WSTRUCT-OPCODE:UNREACHABLE OPEN
   c b END0 ;

\ ---- no backend registered ---------------------------------------------------
: UNLOADED-RUN ( -- )
   BND [: MOD drop ;] IR-CTX:WITH-CONTEXT ;

: UNLOADED-CASE ( -- )
   s" with no Wasm backend registered the registry refuses a WSTRUCT builder" T-LABEL
   [: UNLOADED-RUN ;] E-CTGT-UNLOADED TTHROWSQ ;

\ ---- the closed vocabulary ---------------------------------------------------
64 BUFFER: SPELL

: SAME? ( IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- bool )
   IR-ID:SYMBOL-LOCAL swap IR-ID:SYMBOL-LOCAL = ;

\ Define one opcode and ask for it again by the spelling the module holds.
: ENSURE-ONE ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder i:n :}
   c b i WSTRUCT:NTH WSTRUCT:ENSURE-OP {: s:IR-ID:ir-symbol-id :}
   c b s IR-BUILD:SCHEMA-DEFINED? TTRUE
   c b s SPELL 64 IR-BUILD:SYMBOL-COPY {: u:n :}
   c b SPELL u WSTRUCT:ENSURE-NAMED s SAME? TTRUE ;

\ A spelling defined twice would leave fewer schemas than opcodes.
: VOCAB-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   WSTRUCT:OPCODES 0 ?do c b i ENSURE-ONE loop
   b IR-BUILD:SCHEMAS WSTRUCT:OPCODES T=
   c b WSTRUCT:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FSCHEMAS WSTRUCT:OPCODES T= ;

: NTH-LOW ( -- )    -1 WSTRUCT:NTH drop ;
: NTH-HIGH ( -- )   WSTRUCT:OPCODES WSTRUCT:NTH drop ;

: FOREIGN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b s" wstruct.i64.rotl" WSTRUCT:ENSURE-NAMED drop ;

: FOREIGN-RUN ( -- )
   BND [: FOREIGN-BODY ;] IR-CTX:WITH-CONTEXT ;

: VOCAB-CASE ( -- )
   s" every opcode defines and round-trips through its spelling" T-LABEL
   BND [: VOCAB-BODY ;] IR-CTX:WITH-CONTEXT
   s" an ordinal outside the vocabulary is refused" T-LABEL
   [: NTH-LOW ;] E-WSTRUCT-OPCODE TTHROWSQ
   [: NTH-HIGH ;] E-WSTRUCT-OPCODE TTHROWSQ
   s" a spelling outside the vocabulary is refused" T-LABEL
   [: FOREIGN-RUN ;] E-WSTRUCT-OPCODE TTHROWSQ ;

\ ---- the well-formed module --------------------------------------------------
\ b0(ctx x m): y = x + 1; brz (y == 0) -> b1, b2
\ b1: v m1 = load [$31000+8]; m2 = store [$31000+16] v; s m3 r = call f;
\     brz s -> b5, b4
\ b2: d = x / 1; q = trunc_sat (select (g, f, g != g)) where f = convert d and
\     g = f + f; s2 m4 r2 = call_indirect slot 0 q; br b3(s2 r2 m4)
\ b3(s3 r3 m5): return s3 r3
\ b4: unreachable
\ b5: br b3(s r m3)
: B0 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b BLK-OPEN
   c b  c b I32  ARG {: x:IR-ID:ir-value-id :}
   c b  c b I64  ARG {: v:IR-ID:ir-value-id :}
   c b  c b MEM  ARG {: m:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I64-CONST  c b I64  1 K {: one:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I64-ADD v one  c b I64  OP2 {: y:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I64-EQZ y  c b I32  OP1 {: z:IR-ID:ir-value-id :}
   c b z  b 1 BLK  b 2 BLK  BRZ
   c b IR-BUILD:END-BLOCK drop
   x v m one ;

: B1 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder x:IR-ID:ir-value-id m:IR-ID:ir-value-id :}
   c b BLK-OPEN
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  $31000 K {: a:IR-ID:ir-value-id :}
   c b a m 8 LOAD64 {: v:IR-ID:ir-value-id m1:IR-ID:ir-value-id :}
   c b a v m1 16 STORE64 {: m2:IR-ID:ir-value-id :}
   c b x m2 v  c b s" f" IR-BUILD:INTERN-SYMBOL  CALL1
   {: s:IR-ID:ir-value-id m3:IR-ID:ir-value-id r:IR-ID:ir-value-id :}
   c b s  b 5 BLK  b 4 BLK  BRZ
   c b IR-BUILD:END-BLOCK drop
   s r m3 ;

: B2 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder x:IR-ID:ir-value-id v:IR-ID:ir-value-id
      m:IR-ID:ir-value-id one:IR-ID:ir-value-id :}
   c b BLK-OPEN
   c b WSTRUCT-OPCODE:I64-DIV-S v one  c b I64  OP2 {: d:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:F64-CONVERT-I64-S d  c b F64  OP1 {: f:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:F64-ADD f f  c b F64  OP2 {: g:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:F64-NE g g  c b I32  OP2 {: n:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:F64-SELECT g f n  c b F64  SEL {: h:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I64-TRUNC-SAT-F64-S h  c b I64  OP1 {: q:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: slot:IR-ID:ir-value-id :}
   c b x slot m q CALL-IND1
   {: s2:IR-ID:ir-value-id m4:IR-ID:ir-value-id r2:IR-ID:ir-value-id :}
   c b s2 r2 m4  b 3 BLK  BR3
   c b IR-BUILD:END-BLOCK drop ;

: B3 ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b BLK-OPEN
   c b  c b I32  ARG {: s:IR-ID:ir-value-id :}
   c b  c b I64  ARG {: r:IR-ID:ir-value-id :}
   c b  c b MEM  ARG drop
   c b s r RET
   c b IR-BUILD:END-BLOCK drop ;

: B4 ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b BLK-OPEN
   c b TRAP
   c b IR-BUILD:END-BLOCK drop ;

: B5 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder s:IR-ID:ir-value-id r:IR-ID:ir-value-id
      m:IR-ID:ir-value-id :}
   c b BLK-OPEN
   c b s r m  b 3 BLK  BR3
   c b IR-BUILD:END-BLOCK drop ;

: WELL-FORMED ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b B0 {: x:IR-ID:ir-value-id v:IR-ID:ir-value-id m:IR-ID:ir-value-id
      one:IR-ID:ir-value-id :}
   c b x m B1 {: s:IR-ID:ir-value-id r:IR-ID:ir-value-id m3:IR-ID:ir-value-id :}
   c b x v m one B2
   c b B3
   c b B4
   c b s r m3 B5
   c b IR-BUILD:END-FUN drop ;

: SPELLED? ( IR-BUILD:module IR-ID:ir-symbol-id ptr u8 n -- bool )
   {: m:IR-BUILD:module s:IR-ID:ir-symbol-id a:ptr u:n :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS s a u IR-SYM:FEQ? ;

: FP? ( IR-BUILD:module IR-ID:ir-symbol-id -- bool )
   {: m:IR-BUILD:module s:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSCHEMA-ROWS s IR-SCHEMA:FFEATURES@ CTARGET:F-SCALAR-FP CTARGET:HAS? ;

: WELL-FORMED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b WELL-FORMED
   c b WSTRUCT-OPCODE:I64-ADD WSTRUCT:ENSURE-OP {: add:IR-ID:ir-symbol-id :}
   c b WSTRUCT-OPCODE:F64-ADD WSTRUCT:ENSURE-OP {: fadd:IR-ID:ir-symbol-id :}
   c b WSTRUCT:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FROZEN? TTRUE
   s" the frozen table is this dialect's, at this version" T-LABEL
   m  m IR-BUILD:FSCHEMA-ROWS m IR-BUILD:FKEY IR-SCHEMA:FDIALECT@
      WSTRUCT:NAME SPELLED? TTRUE
   m IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FMAJOR@ WSTRUCT:MAJOR T=
   m IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FMINOR@ WSTRUCT:MINOR T=
   s" the function is a Wasm function of six blocks" T-LABEL
   m IR-BUILD:FFUN-ROWS IR-FUN:FFUNS 1 T=
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY 0 IR-ID:PACK-FUN
      IR-FUN:FCONVENTION@ IR--FUN-CONVENTION:WASM IR--FUN-CONVENTION:EQ TTRUE
   m IR-BUILD:FBLOCK-ROWS IR-FUN:FBLOCKS 6 T=
   s" a double form needs scalar floating point and an integer form does not" T-LABEL
   m fadd FP? TTRUE
   m add FP? TFALSE
   m IR-BUILD:FSCHEMA-ROWS add IR-SCHEMA:FARCH@
      CTARGET-ARCH:WASM CTARGET-ARCH:EQ TTRUE ;

: WELL-FORMED-CASE ( -- )
   s" a well-formed module freezes through WSTRUCT:FREEZE" T-LABEL
   BND [: WELL-FORMED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a use its definition does not dominate ----------------------------------
\ The entry branches straight to b2, b1 defines a value and b2 returns it. The
\ value exists before b2 is built, so construction accepts the operand; only
\ the dominance check of the full freeze sees that b1 is not on every path.
: DOM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b ENTRY-OPEN
   c b  b 2 BLK  BR0
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b WSTRUCT-OPCODE:I64-CONST  c b I64  7 K {: v:IR-ID:ir-value-id :}
   c b  b 2 BLK  BR0
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: s:IR-ID:ir-value-id :}
   c b s v RET
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b WSTRUCT:FREEZE drop ;

: DOM-RUN ( -- )
   BND [: DOM-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a block with no terminator -----------------------------------------------
: TERM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b BLK-OPEN
   c b WSTRUCT-OPCODE:I64-CONST  c b I64  7 K drop
   c b IR-BUILD:END-BLOCK drop ;

: TERM-RUN ( -- )
   BND [: TERM-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- successor arguments -----------------------------------------------------
\ b0 hands an i32 to b1, whose one argument is an i64.
: SUCCARG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b ENTRY-OPEN
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: s:IR-ID:ir-value-id :}
   c b s  b 1 BLK  BR1
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b  c b I64  ARG {: r:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: z:IR-ID:ir-value-id :}
   c b z r RET
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b WSTRUCT:FREEZE drop ;

: SUCCARG-RUN ( -- )
   BND [: SUCCARG-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A two-way branch has no per-edge argument window, so a destination of brz
\ that takes an argument is refused.
: BRZ-ARG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b ENTRY-OPEN
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: s:IR-ID:ir-value-id :}
   c b s  b 1 BLK  b 2 BLK  BRZ
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b  c b I64  ARG {: r:IR-ID:ir-value-id :}
   c b s r RET
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b TRAP
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b WSTRUCT:FREEZE drop ;

: BRZ-ARG-RUN ( -- )
   BND [: BRZ-ARG-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- an opcode the module never registered ------------------------------------
\ The spelling is interned and nothing defined its schema: the operation names
\ an opcode this module's table does not hold.
: UNREGISTERED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b BLK-OPEN
   c b  c b WSTRUCT-OPCODE:I64-CONST WSTRUCT:OPCODE  IR-BUILD:BEGIN-OP
   c b  c b SPAN  IR-BUILD:SET-OP-SPAN
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP drop ;

: UNREGISTERED-RUN ( -- )
   BND [: UNREGISTERED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- the target ---------------------------------------------------------------
\ The host's own binding: its backend lowers it, so the builder is made, and the
\ first WSTRUCT schema is refused against the bound architecture.
: NATIVE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b WSTRUCT-OPCODE:I64-ADD WSTRUCT:ENSURE-OP drop ;

: NATIVE-RUN ( -- )
   NABI:BINDING [: NATIVE-BODY ;] IR-CTX:WITH-CONTEXT ;

: INT-FORM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b  c b WSTRUCT-OPCODE:I64-ADD WSTRUCT:ENSURE-OP  IR-BUILD:SCHEMA-DEFINED? TTRUE ;

: FLOAT-FORM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b WSTRUCT-OPCODE:F64-ADD WSTRUCT:ENSURE-OP drop ;

: FLOAT-FORM-RUN ( -- )
   INT-BND [: FLOAT-FORM-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- another dialect's module -------------------------------------------------
: HIR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b WSTRUCT-OPCODE:I64-ADD WSTRUCT:ENSURE-OP drop ;

: HIR-RUN ( -- )
   BND [: HIR-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A table that carries this dialect's name at another schema version.
: VERSION-BODY ( IR-CTX:ctx n n -- )
   {: c:IR-CTX:ctx major:n minor:n :}
   IR-BUILD:PLAN-DEFAULT
   c WSTRUCT:NAME major minor IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b WSTRUCT-OPCODE:I64-ADD WSTRUCT:ENSURE-OP drop ;

: MAJOR-RUN ( -- )
   BND [: WSTRUCT:MAJOR 1+ WSTRUCT:MINOR VERSION-BODY ;] IR-CTX:WITH-CONTEXT ;

: MINOR-RUN ( -- )
   BND [: WSTRUCT:MAJOR WSTRUCT:MINOR 1+ VERSION-BODY ;] IR-CTX:WITH-CONTEXT ;

: REFUSE-CASE ( -- )
   s" a use its definition does not dominate is refused at the freeze" T-LABEL
   [: DOM-RUN ;] E-IR-VERIFY-DOM TTHROWSQ
   s" a block with no terminator is refused as it closes" T-LABEL
   [: TERM-RUN ;] E-IR-FUN-TERM TTHROWSQ
   s" a successor argument of the wrong type is refused at the freeze" T-LABEL
   [: SUCCARG-RUN ;] E-IR-VERIFY-SUCCARG TTHROWSQ
   s" a destination of brz that takes an argument is refused at the freeze" T-LABEL
   [: BRZ-ARG-RUN ;] E-IR-VERIFY-SUCCARG TTHROWSQ
   s" an operation naming an opcode the module never registered is refused" T-LABEL
   [: UNREGISTERED-RUN ;] E-IR-SCHEMA-OPCODE TTHROWSQ
   s" a WSTRUCT operation under the host's binding is refused" T-LABEL
   [: NATIVE-RUN ;] E-IR-SCHEMA-TARGET TTHROWSQ
   s" a Wasm binding without scalar floating point defines i64 forms" T-LABEL
   INT-BND [: INT-FORM-BODY ;] IR-CTX:WITH-CONTEXT
   s" and refuses the double the f64 forms need" T-LABEL
   [: FLOAT-FORM-RUN ;] E-IR-TYPE-TARGET TTHROWSQ
   s" another dialect's module is refused before anything is defined in it" T-LABEL
   [: HIR-RUN ;] E-WSTRUCT-DIALECT TTHROWSQ
   s" a table of another schema version is refused the same way" T-LABEL
   [: MAJOR-RUN ;] E-WSTRUCT-DIALECT TTHROWSQ
   [: MINOR-RUN ;] E-WSTRUCT-DIALECT TTHROWSQ ;

\ ---- the declared signature ---------------------------------------------------
\ Each function below passes the full IR-BUILD:FREEZE: the return schema's tail
\ types its lanes but not their count, and nothing there reads the signature.
: SHUT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b WSTRUCT:FREEZE drop ;

\ (ctx:i32, x:i64) -> (status:i32, out:i64) returning the status alone.
: RET-ARITY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b ENTRY-OPEN
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: s:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:RETURN OPEN
   c b s USE
   c b END0
   c b SHUT ;

: RET-ARITY-RUN ( -- )
   BND [: RET-ARITY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ (ctx:i32, x:i64) -> (status:i32, out:f64) returning an i64 lane.
: RET-TYPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b  c b  c b F64  ROW  SIG-OPEN
   c b BLK-OPEN
   c b  c b I32  ARG drop
   c b  c b I64  ARG {: v:IR-ID:ir-value-id :}
   c b  c b MEM  ARG drop
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: s:IR-ID:ir-value-id :}
   c b s v RET
   c b SHUT ;

: RET-TYPE-RUN ( -- )
   BND [: RET-TYPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ (ctx:i32, x:i64) -> (status:i32, out:i64) whose entry block takes no x.
: ENTRY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b BLK-OPEN
   c b  c b I32  ARG drop
   c b  c b MEM  ARG drop
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  0 K {: s:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I64-CONST  c b I64  0 K {: r:IR-ID:ir-value-id :}
   c b s r RET
   c b SHUT ;

: ENTRY-RUN ( -- )
   BND [: ENTRY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A function that only traps holds no return, and the freeze adds no symbol for
\ one to a module that never interned it.
: TRAP-ONLY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD {: b:IR-BUILD:builder :}
   c b MAIN-OPEN
   c b ENTRY-OPEN
   c b TRAP
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b WSTRUCT:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS m IR-BUILD:FKEY
   s" wstruct.return" IR-SYM:FFIND nip TFALSE ;

: SIGNATURE-CASE ( -- )
   s" a function that only traps freezes and gains no return symbol" T-LABEL
   BND [: TRAP-ONLY-BODY ;] IR-CTX:WITH-CONTEXT
   s" a return one output lane short of the signature is refused" T-LABEL
   [: RET-ARITY-RUN ;] E-WSTRUCT-SIGNATURE TTHROWSQ
   s" a return lane of another type than the signature's is refused" T-LABEL
   [: RET-TYPE-RUN ;] E-WSTRUCT-SIGNATURE TTHROWSQ
   s" an entry block missing a declared argument is refused" T-LABEL
   [: ENTRY-RUN ;] E-WSTRUCT-SIGNATURE TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   UNLOADED-CASE
   REGISTER-WASM
   VOCAB-CASE
   WELL-FORMED-CASE
   REFUSE-CASE
   SIGNATURE-CASE
   T-REPORT ;

;package

WASM-WSTRUCT-TEST:RUN
