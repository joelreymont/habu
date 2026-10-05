\ encode.f - WENC, src/arch/wasm/encode.f, on the product engine.
\
\ Proves that a frozen WSTRUCT module is written as its emission: a straight
\ line through every opcode with an operand form, an if/else whose arms meet,
\ two nested loops, calls to a host word and to a function of the module, and
\ a data and a code address literal each come out byte for byte; the header
\ names every body with its offset, size, lanes and frame variant, a function
\ past the lane arity taking the aligned frame; every call site is NEMIT:CALL
\ at its own offset, a padded field WLEB reads back after the `call` opcode, its
\ target the host entry or the callee's body offset; every address literal is a
\ site of its kind at its own offset, a padded field WLEB reads back as its
\ value after the `i64.const` opcode, and a number is none; an emission of
\ exactly the module-byte ceiling, its header counted, is sealed. Refused by
\ name: a table of another dialect or WSTRUCT version, a module its builder was
\ not bound from, call_indirect, a function with no body, a brz or two-byte
\ opcode asked for one byte, a callee that is neither, a stated arity unlike
\ the signature or past its header byte, a call whose lanes in or out are not
\ its callee's signature, an address kind WSTRUCT does not have, a memory
\ access aligned past its width, a body past the local or body-byte ceiling, a
\ module past the function ceiling, an emission past the module-byte ceiling
\ only once its header is counted, and a read after a refusal or past the last
\ row.
\
\ THE BYTES ARE WASM-TOOLS'. Each pinned body is what wasm-tools 1.243.0
\ assembles for the same function written as text, its locals declared as WENC
\ declares them, and that module validates with the profile's features; only a
\ call's index differs, the five-byte padded zero WENC writes where the text
\ assembler writes one byte, and an address literal's immediate, the ten-byte
\ padded SLEB WENC writes where the text assembler writes the shortest. The
\ module with both padded address fields spliced in validates too.
\
\ A PROFILE WITH SMALL CEILINGS. WPROF is installed once per process, and V1's
\ ceilings are the browsers' (50000 locals, 7654321 body bytes), too large to
\ build past here, so this suite installs V1 with 64 locals, 3 functions, 400
\ body bytes and 1000 module bytes.
\
\ ONE FIXTURE PER CONTEXT. A module holds about seventeen arenas and the live
\ arena registry holds sixty-four, so every module below is built in its own
\ context.

require lib/test.f
require lib/errors.f
require lib/string.f
require src/core/sha256.f
require src/compiler/numeric-policy.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/context.f
require src/compiler/ir/source.f
require src/compiler/ir/type.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f
require src/compiler/native/frozen.f
require src/compiler/native/backend.f
require src/compiler/native/emission.f
require src/arch/wasm/leb.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/profile.f
require src/arch/wasm/structure.f
require src/arch/wasm/encode.f

package WASM-ENCODE-TEST
private

\ ---- bindings and the profile -------------------------------------------------
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

64 constant LOCALS-CEIL
3 constant FUNCTIONS-CEIL
400 constant BODY-CEIL
1000 constant MODULE-CEIL

\ V1 with its four ceilings, the last four fields, lowered; fields count from
\ the deepest, the order MAKE takes.
: SMALL ( -- WPROF:profile )
   WPROF:V1 WPROF-PROFILE:UNMAKE 2drop 2drop
   {: f0:n f1:n f2:n f3:n f4:n f5:n f6:n f7:n f8:n f9:n
      f10:n f11:n f12:n f13:n :}
   f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13
   LOCALS-CEIL FUNCTIONS-CEIL BODY-CEIL MODULE-CEIL
   WPROF-PROFILE:MAKE ;

\ ---- module rigging ----------------------------------------------------------
\ Every builder's identities are bound before anything is frozen from it.
: BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   c IR-BUILD:PLAN-DEFAULT WSTRUCT:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b WENC:BIND-DIALECT
   b ;

: SPAN ( IR-CTX:ctx IR-BUILD:builder -- IR-SOURCE:span )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   b  c b s" encode" IR-BUILD:ADD-SOURCE  0 6 IR-BUILD:ADD-SPAN ;

: I32 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:I32-TYPE ;
: I64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:I64-TYPE ;
: F64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:F64-TYPE ;
: MEM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )   WSTRUCT:MEM-TYPE ;

\ (ctx:i32, in lanes) -> (status:i32, out lanes), the Habu call's row.
: SIG ( IR-CTX:ctx IR-BUILD:builder n n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder in:n out:n :}
   c b I32 {: w:IR-ID:ir-type-id :}
   c b I64 {: x:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   w IR-TYPE:FN-PARAM
   in 0 ?do  x IR-TYPE:FN-PARAM  loop
   w IR-TYPE:FN-RESULT
   out 0 ?do  x IR-TYPE:FN-RESULT  loop
   c b IR-BUILD:INTERN-CODE-REF ;

\ Function a u of signature sig, defined, exported and of Wasm's convention.
: FN-OPEN-SIG ( IR-CTX:ctx IR-BUILD:builder ptr u8 n IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n sig:IR-ID:ir-type-id :}
   c b  c b a u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   c b sig IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:WASM IR-BUILD:SET-CONVENTION
   c b  c b SPAN  IR-BUILD:SET-FUN-SPAN ;

\ Function a u of the Habu call's row, in lanes in and out lanes out.
: FN-OPEN ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n in:n out:n :}
   c b a u  c b in out SIG  FN-OPEN-SIG ;

: FN-SHUT ( IR-CTX:ctx IR-BUILD:builder -- )
   IR-BUILD:END-FUN drop ;

: BLK-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b SPAN  IR-BUILD:SET-BLOCK-SPAN ;

: ARG ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- IR-ID:ir-value-id )
   IR-BUILD:ADD-BLOCK-ARG ;

\ The entry block of a one-lane function: the context, the lane, the token.
: ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b BLK-OPEN
   c b  c b I32  ARG
   c b  c b I64  ARG
   c b  c b MEM  ARG ;

: SHUT ( IR-CTX:ctx IR-BUILD:builder -- )
   IR-BUILD:END-BLOCK drop ;

\ ---- appending operations ----------------------------------------------------
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

: SELECT ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id u:IR-ID:ir-value-id
      w:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:F64-SELECT OPEN
   c b v USE  c b u USE  c b w USE
   c b  c b F64  END1 ;

: MEMARG ( IR-CTX:ctx IR-BUILD:builder n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder al:n off:n :}
   c b  c b WSTRUCT:KEY-ALIGN  al INT-ATTR
   c b  c b WSTRUCT:KEY-OFFSET  off INT-ATTR ;

\ A load of type t at address a after token k: the value, then the next token.
: LOAD ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-type-id IR-ID:ir-value-id IR-ID:ir-value-id n n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode t:IR-ID:ir-type-id
      a:IR-ID:ir-value-id k:IR-ID:ir-value-id al:n off:n :}
   c b o OPEN
   c b a USE  c b k USE
   c b al off MEMARG
   c b t IR-BUILD:ADD-RESULT
   c b  c b MEM  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP {: op:IR-ID:ir-op-id :}
   c b op 0 IR-BUILD:OP-RESULT@  c b op 1 IR-BUILD:OP-RESULT@ ;

: STORE ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id n n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode a:IR-ID:ir-value-id
      v:IR-ID:ir-value-id k:IR-ID:ir-value-id al:n off:n :}
   c b o OPEN
   c b a USE  c b v USE  c b k USE
   c b al off MEMARG
   c b  c b MEM  END1 ;

\ The call's results after its operands are handed in: status, token, lane.
: CALL-SHUT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b I32  IR-BUILD:ADD-RESULT
   c b  c b MEM  IR-BUILD:ADD-RESULT
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP {: op:IR-ID:ir-op-id :}
   c b op 0 IR-BUILD:OP-RESULT@  c b op 1 IR-BUILD:OP-RESULT@
   c b op 2 IR-BUILD:OP-RESULT@ ;

\ The open call's callee, the function spelled a u.
: CALLEE-ATTR ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n :}
   c b  c b WSTRUCT:KEY-CALLEE  c b  c b a u IR-BUILD:INTERN-SYMBOL
   IR-BUILD:INTERN-SYMBOL-ATTR  IR-BUILD:ADD-ATTR ;

\ A direct call of the callee spelled a u with context cx, token k and lane x.
: CALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id ptr u8 n -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder cx:IR-ID:ir-value-id k:IR-ID:ir-value-id
      x:IR-ID:ir-value-id a u:n :}
   c b WSTRUCT-OPCODE:CALL OPEN
   c b cx USE  c b k USE  c b x USE
   c b a u CALLEE-ATTR
   c b CALL-SHUT ;

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

: I32-K ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:n :}
   c b WSTRUCT-OPCODE:I32-CONST  c b I32  v K ;

\ A cell-wide constant v of address kind kind, which WSTRUCT:ADDR-ATTR checks.
: I64-AK ( IR-CTX:ctx IR-BUILD:builder n n -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:n kind:n :}
   c b WSTRUCT-OPCODE:I64-CONST OPEN
   c b  c b WSTRUCT:KEY-VALUE  v INT-ATTR
   c b  c b WSTRUCT:KEY-ADDR  c b kind WSTRUCT:ADDR-ATTR  IR-BUILD:ADD-ATTR
   c b  c b I64  END1 ;

\ A number.
: I64-K ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-value-id )
   WSTRUCT:ADDR-NONE I64-AK ;

\ Return status s and the lane r.
: RETV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder s:IR-ID:ir-value-id r:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:RETURN OPEN
   c b s USE
   c b r USE
   c b IR-BUILD:END-OP drop ;

\ Return status 0, made just before, and the lane r.
: RET ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ID:ir-value-id :}
   c b  c b 0 I32-K  r RETV ;

\ ---- reading the emission back ------------------------------------------------
: B@ ( n -- n )
   WENC:BYTES + c@ ;

\ len bytes at offset at, the low one first.
: LE@ ( n n -- n )
   {: at:n len:n :}
   0  len 0 ?do  at i + B@  i 8 * lshift or  loop ;

\ Byte f of function k's header row.
: ROW ( n n -- n )
   {: k:n f:n :}
   8  k 12 * +  f + ;

: BODY-OFF ( n -- n )    0 ROW 4 LE@ ;
: BODY-SIZE ( n -- n )   4 ROW 4 LE@ ;

1024 constant CAP
CAP BUFFER: GOT-BUF
CAP BUFFER: WANT-BUF
TYPED-VARIABLE WANT-U len

\ The emission's len bytes from at, in hex.
: HEX$ ( n n -- ptr u8 n )
   {: at:n len:n :}
   len 0 ?do  at i + B@  GOT-BUF i 2 * +  BYTE>HEX  loop
   GOT-BUF len 2 * ;

: BODY$ ( n -- ptr u8 n )
   {: k:n :}
   k BODY-OFF  k BODY-SIZE  HEX$ ;

: WANT-RESET ( -- )
   WANT-U BUF-RESET ;

: W+ ( ptr u8 n -- )
   WANT-BUF CAP WANT-U BUF-APPEND ;

: WANT$ ( -- ptr u8 n )
   WANT-BUF WANT-U @ LEN>N ;

\ ---- the bodies wasm-tools assembles -----------------------------------------
: LINE-WANT ( -- ptr u8 n )
   WANT-RESET
   s" 03117f0f7e0b7c41072102200220026a2103200320026b2104427d2113200120137c2114201420137d2115201520017e2116" W+
   s" 201620137f211720172001832118201820018421192019200185211a201a201386211b201b201388211c201c502105201c20" W+
   s" 01512106201c2001522107201c2001532108201c2001552109201c200157210a201c200159210b44000000000000f83f2122" W+
   s" 20222022a0212320232022a1212420242023a2212520252022a3212620269f212720279a212820289921292029202261210c" W+
   s" 2029202262210d2029202263210e2029202264210f201ca721102010ad211d201db9212a202afc06211e202abd211f201fbf" W+
   s" 212b202b202a200f1b212c200028020821112000290310212020003100012121200020113602182000202037032020002021" W+
   s" 3c0028410021122012201e0f0b" W+
   WANT$ ;

: BRANCH-WANT ( -- ptr u8 n )
   WANT-RESET
   s" 02027f037e024020015021022002044042022104200421060c010542012105200521060c010b0b41002103200320060f0b" W+
   WANT$ ;

: NEST-WANT ( -- ptr u8 n )
   WANT-RESET
   s" 02037f067e420021052005210603404200210720072108034020085021022002044042012109200921080c01052006502103" W+
   s" 200304404201210a200a21060c030541002104200420060f0b0b0b0b000b" W+
   WANT$ ;

: CALLER-WANT ( -- ptr u8 n )
   s" 02027f027e20002001108080808000210421022000200410808080800021052103200320050f0b" ;

: CALLEE-WANT ( -- ptr u8 n )
   s" 01017f41002102200220010f0b" ;

: WIDE-WANT ( -- ptr u8 n )
   s" 01017f4100210120010f0b" ;

\ wasm-tools writes 4280a00c and 428080808004 for the two literals.
: ADDR-WANT ( -- ptr u8 n )
   s" 02017f037e4280a08c80808080808000210342808080808480808080002104200320047c210541002102200220050f0b" ;

\ ---- a straight line through every opcode with an operand form -----------------
\ The integer chain: i32 add and sub, then every i64 arithmetic and bitwise form
\ on the lane x and the constant -3. Answers the chain's last value.
: LINE-INT ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder x:IR-ID:ir-value-id :}
   c b I32 {: w:IR-ID:ir-type-id :}
   c b I64 {: t:IR-ID:ir-type-id :}
   c b 7 I32-K {: a1 :}
   c b WSTRUCT-OPCODE:I32-ADD a1 a1 w OP2 {: a2 :}
   c b WSTRUCT-OPCODE:I32-SUB a2 a1 w OP2 drop
   c b -3 I64-K {: d :}
   c b WSTRUCT-OPCODE:I64-ADD x d t OP2 {: v1 :}
   c b WSTRUCT-OPCODE:I64-SUB v1 d t OP2 {: v2 :}
   c b WSTRUCT-OPCODE:I64-MUL v2 x t OP2 {: v3 :}
   c b WSTRUCT-OPCODE:I64-DIV-S v3 d t OP2 {: v4 :}
   c b WSTRUCT-OPCODE:I64-AND v4 x t OP2 {: v5 :}
   c b WSTRUCT-OPCODE:I64-OR v5 x t OP2 {: v6 :}
   c b WSTRUCT-OPCODE:I64-XOR v6 x t OP2 {: v7 :}
   c b WSTRUCT-OPCODE:I64-SHL v7 d t OP2 {: v8 :}
   c b WSTRUCT-OPCODE:I64-SHR-U v8 d t OP2 ;

\ Every i64 comparison of n against the lane x.
: LINE-CMP ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder n:IR-ID:ir-value-id x:IR-ID:ir-value-id :}
   c b I32 {: w:IR-ID:ir-type-id :}
   c b WSTRUCT-OPCODE:I64-EQZ n w OP1 drop
   c b WSTRUCT-OPCODE:I64-EQ n x w OP2 drop
   c b WSTRUCT-OPCODE:I64-NE n x w OP2 drop
   c b WSTRUCT-OPCODE:I64-LT-S n x w OP2 drop
   c b WSTRUCT-OPCODE:I64-GT-S n x w OP2 drop
   c b WSTRUCT-OPCODE:I64-LE-S n x w OP2 drop
   c b WSTRUCT-OPCODE:I64-GE-S n x w OP2 drop ;

\ Every f64 arithmetic form from the constant 1.5, then every comparison of
\ the result against it. Answers the last comparison.
: LINE-F64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b F64 {: t:IR-ID:ir-type-id :}
   c b I32 {: w:IR-ID:ir-type-id :}
   c b WSTRUCT-OPCODE:F64-CONST t $3FF8000000000000 K {: fa :}
   c b WSTRUCT-OPCODE:F64-ADD fa fa t OP2 {: f1 :}
   c b WSTRUCT-OPCODE:F64-SUB f1 fa t OP2 {: f2 :}
   c b WSTRUCT-OPCODE:F64-MUL f2 f1 t OP2 {: f3 :}
   c b WSTRUCT-OPCODE:F64-DIV f3 fa t OP2 {: f4 :}
   c b WSTRUCT-OPCODE:F64-SQRT f4 t OP1 {: f5 :}
   c b WSTRUCT-OPCODE:F64-NEG f5 t OP1 {: f6 :}
   c b WSTRUCT-OPCODE:F64-ABS f6 t OP1 {: fh :}
   c b WSTRUCT-OPCODE:F64-EQ fh fa w OP2 drop
   c b WSTRUCT-OPCODE:F64-NE fh fa w OP2 drop
   c b WSTRUCT-OPCODE:F64-LT fh fa w OP2 drop
   c b WSTRUCT-OPCODE:F64-GT fh fa w OP2 ;

\ Every conversion from n, and a select on the condition q. Answers the
\ saturating truncation.
: LINE-CONV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder n:IR-ID:ir-value-id q:IR-ID:ir-value-id :}
   c b I32 {: w:IR-ID:ir-type-id :}
   c b I64 {: t:IR-ID:ir-type-id :}
   c b F64 {: f:IR-ID:ir-type-id :}
   c b WSTRUCT-OPCODE:I32-WRAP-I64 n w OP1 {: c1 :}
   c b WSTRUCT-OPCODE:I64-EXTEND-I32-U c1 t OP1 {: c2 :}
   c b WSTRUCT-OPCODE:F64-CONVERT-I64-S c2 f OP1 {: fi :}
   c b WSTRUCT-OPCODE:I64-TRUNC-SAT-F64-S fi t OP1 {: dd :}
   c b WSTRUCT-OPCODE:I64-REINTERPRET-F64 fi t OP1 {: c3 :}
   c b WSTRUCT-OPCODE:F64-REINTERPRET-I64 c3 f OP1 {: fj :}
   c b fj fi q SELECT drop
   dd ;

\ Every load and store at the context address, each ordered by the last token.
: LINE-MEM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder cx:IR-ID:ir-value-id k:IR-ID:ir-value-id :}
   c b WSTRUCT-OPCODE:I32-LOAD  c b I32  cx k 2 8 LOAD {: l1 k1 :}
   c b WSTRUCT-OPCODE:I64-LOAD  c b I64  cx k1 3 16 LOAD {: l2 k2 :}
   c b WSTRUCT-OPCODE:I64-LOAD8-U  c b I64  cx k2 0 1 LOAD {: l3 k3 :}
   c b WSTRUCT-OPCODE:I32-STORE cx l1 k3 2 24 STORE {: k4 :}
   c b WSTRUCT-OPCODE:I64-STORE cx l2 k4 3 32 STORE {: k5 :}
   c b WSTRUCT-OPCODE:I64-STORE8 cx l3 k5 0 40 STORE drop ;

: LINE-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" line" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b x LINE-INT {: n :}
   c b n x LINE-CMP
   c b LINE-F64 {: q :}
   c b n q LINE-CONV {: dd :}
   c b cx k LINE-MEM
   c b dd RET
   c b SHUT
   c b FN-SHUT ;

\ ---- an if/else whose arms meet ------------------------------------------------
\ b0 tests the lane: to b1 when it is not zero, else b2; each hands the merge b3
\ its constant.
: ARM ( IR-CTX:ctx IR-BUILD:builder n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:n to:n :}
   c b BLK-OPEN
   c b v I64-K {: k :}
   c b BR  c b k USE  c b to TO
   c b SHUT ;

: BRANCH-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" branch" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b WSTRUCT-OPCODE:I64-EQZ x  c b I32  OP1 {: z :}
   c b z 1 2 BRZ
   c b SHUT
   c b 1 3 ARM
   c b 2 3 ARM
   c b BLK-OPEN
   c b  c b I64  ARG {: v :}
   c b v RET
   c b SHUT
   c b FN-SHUT ;

\ ---- two nested loops ------------------------------------------------------------
\ b1 heads the outer loop on i, b2 the inner on j: the inner runs again from b3
\ while j is zero, the outer from b5 while i is zero, and b6 returns i.
: NEST-HEADS ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b ENTRY {: cx x k :}
   c b 0 I64-K {: n0 :}
   c b BR  c b n0 USE  c b 1 TO
   c b SHUT
   c b BLK-OPEN
   c b  c b I64  ARG {: iv :}
   c b 0 I64-K {: j0 :}
   c b BR  c b j0 USE  c b 2 TO
   c b SHUT
   c b BLK-OPEN
   c b  c b I64  ARG {: jv :}
   c b WSTRUCT-OPCODE:I64-EQZ jv  c b I32  OP1 {: t :}
   c b t 4 3 BRZ
   c b SHUT
   iv ;

: NEST-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" nest" 1 1 FN-OPEN
   c b NEST-HEADS {: iv :}
   c b 1 2 ARM
   c b BLK-OPEN
   c b WSTRUCT-OPCODE:I64-EQZ iv  c b I32  OP1 {: u :}
   c b u 6 5 BRZ
   c b SHUT
   c b 1 1 ARM
   c b BLK-OPEN
   c b iv RET
   c b SHUT
   c b FN-SHUT ;

\ ---- calls ---------------------------------------------------------------------
\ caller calls a host word and then callee; wide takes the aligned frame, so its
\ signature passes no lane.
: CALLER-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" caller" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b cx k x s" host 4096" CALL {: s1 k1 r1 :}
   c b cx k1 r1 s" callee" CALL {: s2 k2 r2 :}
   c b s2 r2 RETV
   c b SHUT
   c b FN-SHUT ;

\ Function a u, which returns its lane.
: RET-FN ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n :}
   c b a u 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b x RET
   c b SHUT
   c b FN-SHUT ;

: CALLEE-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   s" callee" RET-FN ;

: WIDE-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" wide" 0 0 FN-OPEN
   c b BLK-OPEN
   c b  c b I32  ARG drop
   c b  c b MEM  ARG drop
   c b 0 I32-K {: s :}
   c b WSTRUCT-OPCODE:RETURN OPEN
   c b s USE
   c b IR-BUILD:END-OP drop
   c b SHUT
   c b FN-SHUT ;

\ caller and callee take and leave one lane; wide takes seventeen and leaves one.
: CALLS-ARITY ( n -- n n )
   2 = if 17 1 exit then
   1 1 ;

: ONE-LANE ( n -- n n )
   drop 1 1 ;

\ ---- the cases -----------------------------------------------------------------
1 TYPED-BUFFER FIX IR-BUILD:module

\ A one-function module's emission: the body its row names, then the header.
: ONE-FN-CK ( ptr u8 n n -- )
   {: a u:n size:n :}
   0 BODY$ a u T$=
   WENC:FUNS 1 T=
   0 4 LE@ WENC:MAGIC T=
   4 4 LE@ 1 T=
   0 BODY-OFF 20 T=
   0 WENC:FUNCTION-OFFSET@ 20 T=
   0 BODY-SIZE size T=
   0 8 ROW 1 LE@ 1 T=
   0 9 ROW 1 LE@ 1 T=
   0 10 ROW 1 LE@ 0 T=
   0 11 ROW 1 LE@ 0 T=
   WENC:SIZE 20 size + T= ;

\ Freeze the module built by fn in a fresh builder and encode it one lane each
\ way.
: ENCODED ( IR-CTX:ctx [ IR-CTX:ctx IR-BUILD:builder -- ] -- )
   {: c:IR-CTX:ctx fn :}
   c BUILDER {: b:IR-BUILD:builder :}
   c b fn execute
   c b WSTRUCT:FREEZE 0 FIX !
   0 FIX @ [: ONE-LANE ;] WENC:ENCODE ;

: LINE-BODY ( IR-CTX:ctx -- )
   [: LINE-FN ;] ENCODED
   s" a straight line through every operand form is wasm-tools' function" T-LABEL
   LINE-WANT 313 ONE-FN-CK
   s" its numbers, one of them negative, are no address sites" T-LABEL
   WENC:ADDR-SITES 0 T= ;

: BRANCH-BODY ( IR-CTX:ctx -- )
   [: BRANCH-FN ;] ENCODED
   s" an if/else whose arms meet in a block is wasm-tools' function" T-LABEL
   BRANCH-WANT 49 ONE-FN-CK ;

: NEST-BODY ( IR-CTX:ctx -- )
   [: NEST-FN ;] ENCODED
   s" two nested loops, ending in unreachable, are wasm-tools' function" T-LABEL
   NEST-WANT 80 ONE-FN-CK ;

: SHAPES-CASE ( -- )
   BND [: LINE-BODY ;] IR-CTX:WITH-CONTEXT
   BND [: BRANCH-BODY ;] IR-CTX:WITH-CONTEXT
   BND [: NEST-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- the calls module: header, bodies and sites --------------------------------
\ Function k's row: body offset, size, inputs, outputs, frame variant and pad.
: ROW-IS ( n n n n n n -- )
   {: k:n off:n size:n in:n out:n fv:n :}
   k BODY-OFF off T=
   k WENC:FUNCTION-OFFSET@ off T=
   k BODY-SIZE size T=
   k 8 ROW 1 LE@ in T=
   k 9 ROW 1 LE@ out T=
   k 10 ROW 1 LE@ fv T=
   k 11 ROW 1 LE@ 0 T= ;

\ Site k: NEMIT:CALL at offset off, right after a `call` opcode, inside caller's
\ body, a padded field WLEB reads back as the zero written there.
: SITE-IS ( n n n -- )
   {: k:n off:n target:n :}
   k WENC:CALL-KIND@ NEMIT:CALL T=
   k WENC:CALL-SITE@ off T=
   off 1- B@ WSTRUCT-OPCODE:CALL WENC:OPCODE-BYTE T=
   off 0 BODY-OFF > TTRUE
   off 5 + 0 BODY-OFF 0 BODY-SIZE + <= TTRUE
   WENC:BYTES WENC:SIZE off WLEB:U32-PAD@ 0 T=
   k WENC:CALL-TARGET@ target T= ;

: CALLS-CK ( -- )
   s" calls: three bodies after a header of three rows" T-LABEL
   WENC:FUNS 3 T=
   WENC:BYTES 4 s" HBW1" T$=
   4 4 LE@ 3 T=
   WENC:SIZE 107 T=
   s" calls: caller's body with two padded call fields, and its row" T-LABEL
   0 BODY$ CALLER-WANT T$=
   0 44 39 1 1 0 ROW-IS
   s" calls: callee's body and row" T-LABEL
   1 BODY$ CALLEE-WANT T$=
   1 83 13 1 1 0 ROW-IS
   s" calls: wide, past the lane arity, takes the aligned frame" T-LABEL
   2 BODY$ WIDE-WANT T$=
   2 96 11 17 1 1 ROW-IS
   s" calls: the host word's site, at 54, targets its entry" T-LABEL
   WENC:CALL-SITES 2 T=
   0 54 4096 SITE-IS
   s" calls: callee's site, at 68, targets its body offset" T-LABEL
   1 68  1 WENC:FUNCTION-OFFSET@  SITE-IS ;

: CALLS-ENCODE ( -- )
   0 FIX @ [: CALLS-ARITY ;] WENC:ENCODE ;

\ caller is stated to take two lanes where its signature passes one.
: TWO-LANES ( n -- n n )
   drop 2 1 ;

: UNLIKE-RUN ( -- )
   0 FIX @ [: TWO-LANES ;] WENC:ENCODE ;

\ wide is stated to take 256 lanes, which the aligned frame admits and the
\ header's byte cannot hold.
: WIDE-256 ( n -- n n )
   2 = if 256 1 exit then
   1 1 ;

: PAST-BYTE-RUN ( -- )
   0 FIX @ [: WIDE-256 ;] WENC:ENCODE ;

: SIZE-RUN ( -- )
   WENC:SIZE drop ;

: PAST-SITES-RUN ( -- )
   WENC:CALL-SITES WENC:CALL-SITE@ drop ;

: PAST-FUNS-RUN ( -- )
   WENC:FUNS WENC:FUNCTION-OFFSET@ drop ;

: CALLS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c BUILDER {: b:IR-BUILD:builder :}
   c b CALLER-FN
   c b CALLEE-FN
   c b WIDE-FN
   c b WSTRUCT:FREEZE 0 FIX !
   CALLS-ENCODE
   CALLS-CK
   s" a row past the last call site is refused" T-LABEL
   [: PAST-SITES-RUN ;] E-WENC-STATE TTHROWSQ
   s" a row past the last function is refused" T-LABEL
   [: PAST-FUNS-RUN ;] E-WENC-STATE TTHROWSQ
   s" a stated arity unlike the signature is refused" T-LABEL
   [: UNLIKE-RUN ;] E-WENC-ARITY TTHROWSQ
   s" a refused encode leaves nothing to read" T-LABEL
   [: SIZE-RUN ;] E-WENC-STATE TTHROWSQ
   s" an arity past the header's byte is refused" T-LABEL
   [: PAST-BYTE-RUN ;] E-WENC-ARITY TTHROWSQ
   s" a retired emission leaves nothing to read" T-LABEL
   CALLS-ENCODE
   WENC:SIZE 107 T=
   WENC:RETIRE
   [: SIZE-RUN ;] E-WENC-STATE TTHROWSQ ;

: CALLS-CASE ( -- )
   BND [: CALLS-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- address literals ---------------------------------------------------------
\ The static-data region's start and a code address of the host.
$31000 constant DATA-LIT
$40000000 constant CODE-LIT

\ addr adds a data address to a code address and returns the sum.
: ADDR-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" addr" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b DATA-LIT WSTRUCT:ADDR-DATA I64-AK {: d :}
   c b CODE-LIT WSTRUCT:ADDR-CODE I64-AK {: p :}
   c b WSTRUCT-OPCODE:I64-ADD d p  c b I64  OP2 {: s :}
   c b s RET
   c b SHUT
   c b FN-SHUT ;

\ Address site k: kind at offset off, right after an `i64.const` opcode, a
\ padded field WLEB reads back as the value v written there.
: ADDR-SITE-IS ( n n n n -- )
   {: k:n off:n v:n kind:n :}
   k WENC:ADDR-SITE@ off T=
   k WENC:ADDR-SITE-KIND@ kind T=
   off 1- B@ WSTRUCT-OPCODE:I64-CONST WENC:OPCODE-BYTE T=
   WENC:BYTES WENC:SIZE off WLEB:S64-PAD@ v T= ;

: PAST-ADDRS-RUN ( -- )
   WENC:ADDR-SITES WENC:ADDR-SITE@ drop ;

: PAST-KINDS-RUN ( -- )
   WENC:ADDR-SITES WENC:ADDR-SITE-KIND@ drop ;

: ADDR-SITES-RUN ( -- )
   WENC:ADDR-SITES drop ;

: ADDR-BODY ( IR-CTX:ctx -- )
   [: ADDR-FN ;] ENCODED
   s" a data and a code address literal are wasm-tools' function, padded" T-LABEL
   ADDR-WANT 48 ONE-FN-CK
   s" the data address is a DATA site at 26" T-LABEL
   WENC:ADDR-SITES 2 T=
   0 26 DATA-LIT WSTRUCT:ADDR-DATA ADDR-SITE-IS
   s" the code address is a CODE site at 39" T-LABEL
   1 39 CODE-LIT WSTRUCT:ADDR-CODE ADDR-SITE-IS
   s" a row past the last address site is refused" T-LABEL
   [: PAST-ADDRS-RUN ;] E-WENC-STATE TTHROWSQ
   s" a kind past the last address site is refused" T-LABEL
   [: PAST-KINDS-RUN ;] E-WENC-STATE TTHROWSQ ;

\ A kind WSTRUCT does not have, built past WSTRUCT:ADDR-ATTR, which refuses it.
: BAD-KIND-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" bad-kind" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b WSTRUCT-OPCODE:I64-CONST OPEN
   c b  c b WSTRUCT:KEY-VALUE  0 INT-ATTR
   c b  c b WSTRUCT:KEY-ADDR  WSTRUCT:ADDR-CODE 1+ INT-ATTR
   c b  c b I64  END1 {: v :}
   c b v RET
   c b SHUT
   c b FN-SHUT ;

: BAD-KIND-RUN ( -- )
   BND [: [: BAD-KIND-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;

: ADDR-CASE ( -- )
   BND [: ADDR-BODY ;] IR-CTX:WITH-CONTEXT
   s" an address kind WSTRUCT does not have is refused" T-LABEL
   [: BAD-KIND-RUN ;] E-WSTRUCT-ADDR TTHROWSQ
   s" a refused encode leaves no address site to read" T-LABEL
   [: ADDR-SITES-RUN ;] E-WENC-STATE TTHROWSQ ;

\ ---- binding --------------------------------------------------------------------
\ Run first, before this process binds any builder.
: UNBOUND-RUN ( -- )
   0 FIX @ [: ONE-LANE ;] WENC:ENCODE ;

: UNBOUND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c WSTRUCT:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b CALLEE-FN
   c b WSTRUCT:FREEZE 0 FIX !
   s" a module whose builder was never bound is refused" T-LABEL
   [: UNBOUND-RUN ;] E-WENC-STATE TTHROWSQ
   s" a module of a builder other than the one bound is refused" T-LABEL
   c BUILDER drop
   [: UNBOUND-RUN ;] E-WENC-STATE TTHROWSQ ;

: UNBOUND-CASE ( -- )
   BND [: UNBOUND-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A table named a u at the given version.
: TABLE-BODY ( IR-CTX:ctx ptr u8 n n n -- )
   {: c:IR-CTX:ctx a u:n major:n minor:n :}
   IR-BUILD:PLAN-DEFAULT
   c  c a u major minor IR-BUILD:NEW-BUILDER  WENC:BIND-DIALECT ;

: VERSION-BODY ( IR-CTX:ctx n n -- )
   {: c:IR-CTX:ctx major:n minor:n :}
   c WSTRUCT:NAME major minor TABLE-BODY ;

\ Another dialect's table at WSTRUCT's own version.
: OTHER-RUN ( -- )
   BND [: s" other" WSTRUCT:MAJOR WSTRUCT:MINOR TABLE-BODY ;] IR-CTX:WITH-CONTEXT ;

: MAJOR-RUN ( -- )
   BND [: WSTRUCT:MAJOR 1+ WSTRUCT:MINOR VERSION-BODY ;] IR-CTX:WITH-CONTEXT ;

: MINOR-RUN ( -- )
   BND [: WSTRUCT:MAJOR WSTRUCT:MINOR 1+ VERSION-BODY ;] IR-CTX:WITH-CONTEXT ;

: DIALECT-CASE ( -- )
   s" another dialect's table is not bound" T-LABEL
   [: OTHER-RUN ;] E-WSTRUCT-DIALECT TTHROWSQ
   s" a WSTRUCT table of another major version is not bound" T-LABEL
   [: MAJOR-RUN ;] E-WSTRUCT-DIALECT TTHROWSQ
   s" a WSTRUCT table of another minor version is not bound" T-LABEL
   [: MINOR-RUN ;] E-WSTRUCT-DIALECT TTHROWSQ ;

\ ---- forms with no Wasm bytes here ----------------------------------------------
: INDIRECT-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" indirect" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b 0 I32-K {: slot :}
   c b WSTRUCT-OPCODE:CALL-INDIRECT OPEN
   c b cx USE  c b slot USE  c b k USE  c b x USE
   c b CALL-SHUT {: s1 k1 r1 :}
   c b s1 r1 RETV
   c b SHUT
   c b FN-SHUT ;

\ An imported function: a signature, no blocks, and exported, as the substrate
\ requires of a function with no body (src/compiler/ir/fun.f).
: IMPORT-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b s" outside" IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   c b  c b 1 1 SIG  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:IMPORTED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:WASM IR-BUILD:SET-CONVENTION
   c b  c b SPAN  IR-BUILD:SET-FUN-SPAN
   c b FN-SHUT ;

: INDIRECT-RUN ( -- )
   BND [: [: INDIRECT-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;

: IMPORT-RUN ( -- )
   BND [: [: IMPORT-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;

: BRZ-BYTE-RUN ( -- )
   WSTRUCT-OPCODE:BRZ WENC:OPCODE-BYTE drop ;

: SAT-BYTE-RUN ( -- )
   WSTRUCT-OPCODE:I64-TRUNC-SAT-F64-S WENC:OPCODE-BYTE drop ;

: FORM-CASE ( -- )
   s" call_indirect is refused by name" T-LABEL
   [: INDIRECT-RUN ;] E-WENC-FORM TTHROWSQ
   s" a function with no body is refused by name" T-LABEL
   [: IMPORT-RUN ;] E-WENC-FORM TTHROWSQ
   s" brz has no opcode byte" T-LABEL
   [: BRZ-BYTE-RUN ;] E-WENC-FORM TTHROWSQ
   s" the saturating truncation has two opcode bytes, not one" T-LABEL
   [: SAT-BYTE-RUN ;] E-WENC-FORM TTHROWSQ ;

\ ---- callees that are neither -----------------------------------------------------
: CALLS-ONE ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n :}
   c b s" stray" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b cx k x a u CALL {: s1 k1 r1 :}
   c b s1 r1 RETV
   c b SHUT
   c b FN-SHUT ;

: NO-PREFIX-FN ( IR-CTX:ctx IR-BUILD:builder -- )   s" hosts42" CALLS-ONE ;
: SIGNED-FN ( IR-CTX:ctx IR-BUILD:builder -- )      s" host -5" CALLS-ONE ;
: LONG-FN ( IR-CTX:ctx IR-BUILD:builder -- )        s" host 1234567890123456789012" CALLS-ONE ;
: PAST-CELL-FN ( IR-CTX:ctx IR-BUILD:builder -- )   s" host 9999999999999999999" CALLS-ONE ;

: NO-PREFIX-RUN ( -- )  BND [: [: NO-PREFIX-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: SIGNED-RUN ( -- )     BND [: [: SIGNED-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: LONG-RUN ( -- )       BND [: [: LONG-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: PAST-CELL-RUN ( -- )  BND [: [: PAST-CELL-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;

: CALLEE-CASE ( -- )
   s" a callee spelled host with no space is refused" T-LABEL
   [: NO-PREFIX-RUN ;] E-WENC-CALLEE TTHROWSQ
   s" a callee spelled host and a signed number is refused" T-LABEL
   [: SIGNED-RUN ;] E-WENC-CALLEE TTHROWSQ
   s" a callee spelling longer than any host entry is refused" T-LABEL
   [: LONG-RUN ;] E-WENC-CALLEE TTHROWSQ
   s" a callee spelled host and a number past a cell is refused" T-LABEL
   [: PAST-CELL-RUN ;] E-WENC-CALLEE TTHROWSQ ;

\ ---- calls unlike their callee's signature -------------------------------------
\ Each call below is one WSTRUCT's call row admits, to a function of its module.
\ bad-call hands callee its context and no lane.
: SHORT-IN-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" bad-call" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b WSTRUCT-OPCODE:CALL OPEN
   c b cx USE  c b k USE
   c b s" callee" CALLEE-ATTR
   c b CALL-SHUT {: s1 k1 r1 :}
   c b s1 r1 RETV
   c b SHUT
   c b FN-SHUT ;

\ short-out takes callee's status back and no lane.
: SHORT-OUT-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" short-out" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b WSTRUCT-OPCODE:CALL OPEN
   c b cx USE  c b k USE  c b x USE
   c b s" callee" CALLEE-ATTR
   c b  c b I32  IR-BUILD:ADD-RESULT
   c b  c b MEM  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP {: op:IR-ID:ir-op-id :}
   c b  c b op 0 IR-BUILD:OP-RESULT@  x RETV
   c b SHUT
   c b FN-SHUT ;

\ callee's row, (ctx:i32, lane:i64) -> (status:i32, lane:i64), with an f64 for
\ its lane coming in, or with the flag set going out.
: F-SIG ( IR-CTX:ctx IR-BUILD:builder bool -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder out:bool :}
   c b I32 {: w:IR-ID:ir-type-id :}
   c b I64 {: x:IR-ID:ir-type-id :}
   c b F64 {: f:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   w IR-TYPE:FN-PARAM
   out if x else f then IR-TYPE:FN-PARAM
   w IR-TYPE:FN-RESULT
   out if f else x then IR-TYPE:FN-RESULT
   c b IR-BUILD:INTERN-CODE-REF ;

\ f-callee returns the lane 0 whatever f64 it takes.
: F-CALLEE-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" f-callee"  c b false F-SIG  FN-OPEN-SIG
   c b BLK-OPEN
   c b  c b I32  ARG drop
   c b  c b F64  ARG drop
   c b  c b MEM  ARG drop
   c b  c b 0 I64-K  RET
   c b SHUT
   c b FN-SHUT ;

\ f-result would leave an f64 where callee leaves its lane; it never returns,
\ since a return's lanes are i64s.
: F-RESULT-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" f-result"  c b true F-SIG  FN-OPEN-SIG
   c b BLK-OPEN
   c b  c b I32  ARG drop
   c b  c b I64  ARG drop
   c b  c b MEM  ARG drop
   c b WSTRUCT-OPCODE:UNREACHABLE OPEN
   c b IR-BUILD:END-OP drop
   c b SHUT
   c b FN-SHUT ;

: SHORT-IN-MOD ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b SHORT-IN-FN
   c b CALLEE-FN ;

: SHORT-OUT-MOD ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b SHORT-OUT-FN
   c b CALLEE-FN ;

\ stray hands f-callee its i64 lane where f-callee declares an f64.
: F-LANE-MOD ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" f-callee" CALLS-ONE
   c b F-CALLEE-FN ;

\ stray takes back an i64 lane where f-result declares an f64.
: F-RESULT-MOD ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" f-result" CALLS-ONE
   c b F-RESULT-FN ;

: SHORT-IN-RUN ( -- )   BND [: [: SHORT-IN-MOD ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: SHORT-OUT-RUN ( -- )  BND [: [: SHORT-OUT-MOD ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: F-LANE-RUN ( -- )     BND [: [: F-LANE-MOD ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: F-RESULT-RUN ( -- )   BND [: [: F-RESULT-MOD ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;

: SIGNATURE-CASE ( -- )
   s" a call handing its callee fewer lanes than it declares is refused" T-LABEL
   [: SHORT-IN-RUN ;] E-WENC-ARITY TTHROWSQ
   s" a call taking back fewer lanes than its callee declares is refused" T-LABEL
   [: SHORT-OUT-RUN ;] E-WENC-ARITY TTHROWSQ
   s" a call handing an i64 lane where its callee declares an f64 is refused" T-LABEL
   [: F-LANE-RUN ;] E-WENC-ARITY TTHROWSQ
   s" a call taking back an i64 lane where its callee declares an f64 is refused" T-LABEL
   [: F-RESULT-RUN ;] E-WENC-ARITY TTHROWSQ ;

\ ---- memory accesses aligned past their width --------------------------------
\ LINE-MEM aligns each access at its width, which is sealed; each below asks one
\ power of two more.
\ A function that loads type t by opcode o from its context, aligned at 2^al.
: LOAD-AT ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-type-id n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode t:IR-ID:ir-type-id al:n :}
   c b s" align" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b o t cx k al 0 LOAD drop drop
   c b x RET
   c b SHUT
   c b FN-SHUT ;

\ The same storing its lane, or its context when narrow is set.
: STORE-AT ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode bool n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode narrow:bool al:n :}
   c b s" align" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b o cx  narrow if cx else x then  k al 0 STORE drop
   c b x RET
   c b SHUT
   c b FN-SHUT ;

: I32-LOAD-3 ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b WSTRUCT-OPCODE:I32-LOAD  c b I32  3 LOAD-AT ;

: I64-LOAD-4 ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b WSTRUCT-OPCODE:I64-LOAD  c b I64  4 LOAD-AT ;

: BYTE-LOAD-1 ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b WSTRUCT-OPCODE:I64-LOAD8-U  c b I64  1 LOAD-AT ;

: I32-STORE-3 ( IR-CTX:ctx IR-BUILD:builder -- )
   WSTRUCT-OPCODE:I32-STORE true 3 STORE-AT ;

: I64-STORE-4 ( IR-CTX:ctx IR-BUILD:builder -- )
   WSTRUCT-OPCODE:I64-STORE false 4 STORE-AT ;

: BYTE-STORE-1 ( IR-CTX:ctx IR-BUILD:builder -- )
   WSTRUCT-OPCODE:I64-STORE8 false 1 STORE-AT ;

: I32-LOAD-RUN ( -- )    BND [: [: I32-LOAD-3 ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: I64-LOAD-RUN ( -- )    BND [: [: I64-LOAD-4 ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: BYTE-LOAD-RUN ( -- )   BND [: [: BYTE-LOAD-1 ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: I32-STORE-RUN ( -- )   BND [: [: I32-STORE-3 ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: I64-STORE-RUN ( -- )   BND [: [: I64-STORE-4 ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: BYTE-STORE-RUN ( -- )  BND [: [: BYTE-STORE-1 ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;

: ALIGN-CASE ( -- )
   s" i32.load aligned at 8 bytes is refused" T-LABEL
   [: I32-LOAD-RUN ;] E-WENC-FORM TTHROWSQ
   s" i64.load aligned at 16 bytes is refused" T-LABEL
   [: I64-LOAD-RUN ;] E-WENC-FORM TTHROWSQ
   s" i64.load8_u aligned at 2 bytes is refused" T-LABEL
   [: BYTE-LOAD-RUN ;] E-WENC-FORM TTHROWSQ
   s" i32.store aligned at 8 bytes is refused" T-LABEL
   [: I32-STORE-RUN ;] E-WENC-FORM TTHROWSQ
   s" i64.store aligned at 16 bytes is refused" T-LABEL
   [: I64-STORE-RUN ;] E-WENC-FORM TTHROWSQ
   s" i64.store8 aligned at 2 bytes is refused" T-LABEL
   [: BYTE-STORE-RUN ;] E-WENC-FORM TTHROWSQ ;

\ ---- past the ceilings -------------------------------------------------------------
\ cnt adds, each a value of its own: a body of 7cnt + 19 bytes and cnt + 4 locals.
: CHAIN-FN ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n cnt:n :}
   c b a u 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   c b 1 I64-K
   cnt 0 ?do
      {: v:IR-ID:ir-value-id :}
      c b WSTRUCT-OPCODE:I64-ADD v x  c b I64  OP2
   loop
   {: last:IR-ID:ir-value-id :}
   c b last RET
   c b SHUT
   c b FN-SHUT ;

\ cnt constants: a body of 4cnt + 15 bytes and cnt + 3 locals.
: CONSTS-FN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder cnt:n :}
   c b s" consts" 1 1 FN-OPEN
   c b ENTRY {: cx x k :}
   cnt 0 ?do  c b 1 I64-K drop  loop
   c b x RET
   c b SHUT
   c b FN-SHUT ;

\ 425 bytes in 62 locals.
: LONG-BODY-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   s" long" 58 CHAIN-FN ;

\ 65 locals in 263 bytes.
: MANY-LOCALS-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   62 CONSTS-FN ;

\ Three bodies of 348 bytes in 51 locals each.
: BIG-MODULE-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" big1" 47 CHAIN-FN
   c b s" big2" 47 CHAIN-FN
   c b s" big3" 47 CHAIN-FN ;

\ Three bodies of 327 bytes in 48 locals each: 981 bytes, and 1025 with the
\ header's 44.
: EDGE-MODULE-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" edge1" 44 CHAIN-FN
   c b s" edge2" 44 CHAIN-FN
   c b s" edge3" 44 CHAIN-FN ;

\ Bodies of 383, 390 and 183 bytes: 1000 with the header's 44.
: FULL-MODULE-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" full1" 52 CHAIN-FN
   c b s" full2" 53 CHAIN-FN
   c b 42 CONSTS-FN ;

\ One function past the function ceiling.
: FOUR-FN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b s" ret1" RET-FN
   c b s" ret2" RET-FN
   c b s" ret3" RET-FN
   c b s" ret4" RET-FN ;

: LONG-BODY-RUN ( -- )    BND [: [: LONG-BODY-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: MANY-LOCALS-RUN ( -- )  BND [: [: MANY-LOCALS-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: BIG-MODULE-RUN ( -- )   BND [: [: BIG-MODULE-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: EDGE-MODULE-RUN ( -- )  BND [: [: EDGE-MODULE-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;
: FOUR-RUN ( -- )         BND [: [: FOUR-FN ;] ENCODED ;] IR-CTX:WITH-CONTEXT ;

: FULL-MODULE-BODY ( IR-CTX:ctx -- )
   [: FULL-MODULE-FN ;] ENCODED
   s" an emission of exactly the module-byte ceiling, its header counted, is sealed" T-LABEL
   WENC:FUNS 3 T=
   WENC:SIZE MODULE-CEIL T= ;

: CEILING-CASE ( -- )
   s" a body past the body-byte ceiling is refused" T-LABEL
   [: LONG-BODY-RUN ;] E-WENC-CEILING TTHROWSQ
   s" a body past the local ceiling is refused" T-LABEL
   [: MANY-LOCALS-RUN ;] E-WENC-CEILING TTHROWSQ
   s" bodies past the module-byte ceiling are refused" T-LABEL
   [: BIG-MODULE-RUN ;] E-WENC-CEILING TTHROWSQ
   s" bodies under the module-byte ceiling and past it with their header are refused" T-LABEL
   [: EDGE-MODULE-RUN ;] E-WENC-CEILING TTHROWSQ
   BND [: FULL-MODULE-BODY ;] IR-CTX:WITH-CONTEXT
   s" a module past the function ceiling is refused" T-LABEL
   [: FOUR-RUN ;] E-WENC-CEILING TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   REGISTER-WASM
   SMALL WPROF:CURRENT!
   UNBOUND-CASE
   ADDR-CASE
   SHAPES-CASE
   CALLS-CASE
   DIALECT-CASE
   FORM-CASE
   CALLEE-CASE
   SIGNATURE-CASE
   ALIGN-CASE
   CEILING-CASE
   T-REPORT ;

;package

WASM-ENCODE-TEST:RUN
