\ kernel.f - WKERNEL, the engine primitives a Wasm module calls: the four whose
\ bodies read or write the context, hand-built as WSTRUCT functions, and the map
\ from each engine-prefix name a module may call to its provider, one of those
\ rows or a word of src/arch/wasm/kernel-words.f (docs/wasm-backend.md 10.1).
\
\ THE ROWS. ENCODE builds them as one module under WBACK's binding, so the Wasm
\ backend is installed first, and leaves it as WENC's emission
\ (src/arch/wasm/encode.f): function k is the row the map answers as k. Each
\ takes the Habu call's row (section 7.1) with the lanes its primitive has.
\ - emit ( c -- ) stores c's low byte at WPROF's OUT base plus ctx.out-len and
\   counts it; with OUT full it traps instead.
\ - depth ( -- n ) answers the cells from ctx.stack-base to ctx.stack-top: the
\   rows every caller stored (src/arch/wasm/select.f CALL-SAVE).
\ - .s ( -- ) calls `.` on each of those cells, deepest first, and leaves them,
\   as the engine's B.S does (src/habu/habu1.f). The call names the engine's `.`
\   by its host entry, as a compiled `.` does, so the link resolves it through
\   this map; a status 1 from it propagates.
\ - throw ( n -- ) is a terminal's callee (select.f SEL-TERMINAL): it takes its
\   code off the context stack, stores it in ctx.throw-code and answers status
\   1. A zero code throws too, as the engine's does: `0 throw` natively ends
\   uncaught with code 0, and the checker reads `throw` as never returning.
\
\ THE MAP. PROVIDER answers a row by its function or a word by its qualified
\ spelling. Any other name is refused with E-WLINK-UNRESOLVED, since a call
\ site naming it names no function the link can hold.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/type.f
require src/compiler/ir/source.f
require src/compiler/ir/build.f
require src/compiler/native/dict.f
require src/arch/wasm/profile.f
require src/arch/wasm/wstruct.f
require src/arch/wasm/encode.f
require src/arch/wasm/link.f
require src/arch/wasm/backend.f

package WKERNEL
public

ENUM provider 0
   VARIANT row FIELD fun n ;VARIANT
   VARIANT word FIELD name ptr u8 FIELD name-len n ;VARIANT
;ENUM

private

\ The rows' functions, in the order ENCODE builds them.
0 constant EMIT-FUN
1 constant DEPTH-FUN
2 constant DOT-S-FUN
3 constant THROW-FUN

WPROF:STACK-BASE WPROF:OUT-BASE - constant OUT-CAP

\ Each function's span is its name in this text.
: ROWS$ ( -- ptr u8 n )
   s" emit depth .s throw" ;

\ ---- staging one WSTRUCT operation --------------------------------------------
1 TYPED-BUFFER K-CTX IR-CTX:ctx
1 TYPED-BUFFER K-BLD IR-BUILD:builder
1 TYPED-BUFFER K-SRC IR-ID:ir-source-id
1 TYPED-BUFFER K-SPAN IR-SOURCE:span
1 TYPED-BUFFER K-TOK IR-ID:ir-value-id     \ the token the next effect takes
variable MADE                              \ blocks built, the next one's ordinal

: CTX ( -- IR-CTX:ctx )              0 K-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 K-BLD @ ;
: TOK ( -- IR-ID:ir-value-id )       0 K-TOK @ ;
: TOK! ( IR-ID:ir-value-id -- )      0 K-TOK ! ;

: I32 ( -- IR-ID:ir-type-id )        CTX BLD WSTRUCT:I32-TYPE ;
: I64 ( -- IR-ID:ir-type-id )        CTX BLD WSTRUCT:I64-TYPE ;
: MEM ( -- IR-ID:ir-type-id )        CTX BLD WSTRUCT:MEM-TYPE ;

: OPEN ( WSTRUCT:opcode -- )
   {: o:WSTRUCT:opcode :}
   CTX BLD  CTX BLD o WSTRUCT:ENSURE-OP  IR-BUILD:BEGIN-OP
   CTX BLD  0 K-SPAN @  IR-BUILD:SET-OP-SPAN ;

: USE ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   CTX BLD v IR-BUILD:ADD-OPERAND ;

: RES ( IR-ID:ir-type-id -- )
   {: t:IR-ID:ir-type-id :}
   CTX BLD t IR-BUILD:ADD-RESULT ;

: CLOSE ( -- IR-ID:ir-op-id )
   CTX BLD IR-BUILD:END-OP ;

: RESULT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: o:IR-ID:ir-op-id i:n :}
   CTX BLD o i IR-BUILD:OP-RESULT@ ;

: CLOSE1 ( IR-ID:ir-type-id -- IR-ID:ir-value-id )
   RES CLOSE 0 RESULT ;

: INT-ATTR ( IR-ID:ir-symbol-id n -- )
   {: k:IR-ID:ir-symbol-id v:n :}
   CTX BLD k  CTX BLD v IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR ;

: K32 ( n -- IR-ID:ir-value-id )
   {: v:n :}
   WSTRUCT-OPCODE:I32-CONST OPEN
   CTX BLD WSTRUCT:KEY-VALUE v INT-ATTR
   I32 CLOSE1 ;

: K64 ( n -- IR-ID:ir-value-id )
   {: v:n :}
   WSTRUCT-OPCODE:I64-CONST OPEN
   CTX BLD WSTRUCT:KEY-VALUE v INT-ATTR
   CTX BLD  CTX BLD WSTRUCT:KEY-ADDR  CTX BLD WSTRUCT:ADDR-NONE WSTRUCT:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   I64 CLOSE1 ;

: OP1 ( IR-ID:ir-value-id WSTRUCT:opcode IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id o:WSTRUCT:opcode t:IR-ID:ir-type-id :}
   o OPEN  a USE  t CLOSE1 ;

: OP2 ( IR-ID:ir-value-id IR-ID:ir-value-id WSTRUCT:opcode IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id b:IR-ID:ir-value-id o:WSTRUCT:opcode
      t:IR-ID:ir-type-id :}
   o OPEN  a USE  b USE  t CLOSE1 ;

\ The i32 a widened to an i64 cell.
: WIDE ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   WSTRUCT-OPCODE:I64-EXTEND-I32-U I64 OP1 ;

\ ---- linear memory -----------------------------------------------------------
\ An access at an i32 address plus a byte offset, ordered by the current token.
: MEMARG ( n n -- )
   {: al:n off:n :}
   CTX BLD WSTRUCT:KEY-ALIGN al INT-ATTR
   CTX BLD WSTRUCT:KEY-OFFSET off INT-ATTR ;

: LOAD ( IR-ID:ir-value-id n WSTRUCT:opcode IR-ID:ir-type-id n -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id off:n o:WSTRUCT:opcode t:IR-ID:ir-type-id al:n :}
   o OPEN  a USE  TOK USE
   al off MEMARG
   t RES  MEM RES
   CLOSE {: id:IR-ID:ir-op-id :}
   id 1 RESULT TOK!
   id 0 RESULT ;

: STORE ( IR-ID:ir-value-id IR-ID:ir-value-id n WSTRUCT:opcode n -- )
   {: a:IR-ID:ir-value-id v:IR-ID:ir-value-id off:n o:WSTRUCT:opcode al:n :}
   o OPEN  a USE  v USE  TOK USE
   al off MEMARG
   MEM CLOSE1 TOK! ;

: LD32 ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   WSTRUCT-OPCODE:I32-LOAD I32 2 LOAD ;

: LD64 ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   WSTRUCT-OPCODE:I64-LOAD I64 3 LOAD ;

: ST8 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- )
   WSTRUCT-OPCODE:I64-STORE8 0 STORE ;

: ST32 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- )
   WSTRUCT-OPCODE:I32-STORE 2 STORE ;

: ST64 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- )
   WSTRUCT-OPCODE:I64-STORE 3 STORE ;

\ ---- blocks and control --------------------------------------------------------
: BLOCK ( -- )
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD 0 K-SPAN @ IR-BUILD:SET-BLOCK-SPAN ;

: BLOCK-END ( -- )
   CTX BLD IR-BUILD:END-BLOCK drop
   1 MADE +! ;

: ARG ( IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: t:IR-ID:ir-type-id :}
   CTX BLD t IR-BUILD:ADD-BLOCK-ARG ;

\ The entry block, opened on its context address and its token.
: ENTRY ( -- IR-ID:ir-value-id )
   BLOCK
   I32 ARG
   MEM ARG TOK! ;

: SUCC ( n -- )
   {: ord:n :}
   CTX BLD  BLD IR-BUILD:MODULE-KEY ord IR-ID:PACK-BLOCK  IR-BUILD:ADD-SUCCESSOR ;

\ A br is opened, handed the destination's arguments with USE and closed by TO.
: TO ( n -- )
   SUCC CLOSE drop  BLOCK-END ;

\ To block z when the i32 condition is zero, to block nz otherwise.
: BRZ ( IR-ID:ir-value-id n n -- )
   {: c:IR-ID:ir-value-id z:n nz:n :}
   WSTRUCT-OPCODE:BRZ OPEN  c USE  z SUCC  nz TO ;

\ A return opened on its status; the output lanes follow it.
: RET-OPEN ( IR-ID:ir-value-id -- )
   WSTRUCT-OPCODE:RETURN OPEN USE ;

: RET ( IR-ID:ir-value-id -- )
   RET-OPEN CLOSE drop  BLOCK-END ;

: TRAP ( -- )
   WSTRUCT-OPCODE:UNREACHABLE OPEN CLOSE drop  BLOCK-END ;

\ ---- one function -------------------------------------------------------------
\ (ctx:i32, in lanes) -> (status:i32, out lanes), the Habu call's row.
: SIGNATURE ( n n -- IR-ID:ir-type-id )
   {: in:n out:n :}
   IR-TYPE:FN-BEGIN
   I32 IR-TYPE:FN-PARAM
   in 0 ?do  I64 IR-TYPE:FN-PARAM  loop
   I32 IR-TYPE:FN-RESULT
   out 0 ?do  I64 IR-TYPE:FN-RESULT  loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

\ Function name of in lanes in and out lanes out, its span its name at off in
\ ROWS$.
: FUN-OPEN ( ptr u8 n n n n -- )
   {: name:ptr u:n off:n in:n out:n :}
   BLD 0 K-SRC @ off u IR-BUILD:ADD-SPAN 0 K-SPAN !
   CTX BLD  CTX BLD name u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CTX BLD in out SIGNATURE IR-BUILD:SET-SIGNATURE
   CTX BLD IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CTX BLD IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CTX BLD IR--FUN-CONVENTION:WASM IR-BUILD:SET-CONVENTION
   CTX BLD 0 K-SPAN @ IR-BUILD:SET-FUN-SPAN ;

: FUN-SHUT ( -- )
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- the rows ------------------------------------------------------------------
\ Blocks: 0 the test, 1 the trap, 2 the store.
: EMIT-ROW ( -- )
   s" emit" 0 1 0 FUN-OPEN
   MADE @ {: at:n :}
   BLOCK
   I32 ARG {: cx:IR-ID:ir-value-id :}
   I64 ARG {: c:IR-ID:ir-value-id :}
   MEM ARG TOK!
   cx WPROF:CTX-OUT-LEN LD32 {: len:IR-ID:ir-value-id :}
   len WIDE  OUT-CAP K64  WSTRUCT-OPCODE:I64-LT-S I32 OP2  at 1+  at 2 +  BRZ
   BLOCK  TRAP
   BLOCK
   len c WPROF:OUT-BASE ST8
   cx  len 1 K32 WSTRUCT-OPCODE:I32-ADD I32 OP2  WPROF:CTX-OUT-LEN ST32
   0 K32 RET
   FUN-SHUT ;

: DEPTH-ROW ( -- )
   s" depth" 5 0 1 FUN-OPEN
   ENTRY {: cx:IR-ID:ir-value-id :}
   cx WPROF:CTX-STACK-TOP LD32  cx WPROF:CTX-STACK-BASE LD32
   WSTRUCT-OPCODE:I32-SUB I32 OP2  WIDE
   3 K64 WSTRUCT-OPCODE:I64-SHR-U I64 OP2 {: n:IR-ID:ir-value-id :}
   0 K32 RET-OPEN  n USE  CLOSE drop  BLOCK-END
   FUN-SHUT ;

\ The engine's `.`, by the entry a compiled call to it carries.
: DOT-CALLEE ( -- IR-ID:ir-symbol-id )
   SB-RESET
   s" host " SB-APPEND
   s" ." NDICT:CALL-TARGET FMT:SB-U
   CTX BLD SB$ IR-BUILD:INTERN-SYMBOL ;

\ `.` of the cell v; answers its status.
: DOT-CALL ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: cx:IR-ID:ir-value-id v:IR-ID:ir-value-id :}
   DOT-CALLEE {: callee:IR-ID:ir-symbol-id :}
   WSTRUCT-OPCODE:CALL OPEN
   cx USE  TOK USE  v USE
   CTX BLD  CTX BLD WSTRUCT:KEY-CALLEE  CTX BLD callee IR-BUILD:INTERN-SYMBOL-ATTR
   IR-BUILD:ADD-ATTR
   I32 RES  MEM RES
   CLOSE {: id:IR-ID:ir-op-id :}
   id 1 RESULT TOK!
   id 0 RESULT ;

\ Blocks: 0 the entry, 1 the loop's head on the cell's address, 2 its body, 3
\ the end, 4 the next cell, 5 a status 1 propagated.
: DOT-S-ROW ( -- )
   s" .s" 11 0 0 FUN-OPEN
   MADE @ {: at:n :}
   ENTRY {: cx:IR-ID:ir-value-id :}
   cx WPROF:CTX-STACK-BASE LD32 {: base:IR-ID:ir-value-id :}
   cx WPROF:CTX-STACK-TOP LD32 WIDE {: top:IR-ID:ir-value-id :}
   WSTRUCT-OPCODE:BR OPEN  base USE  TOK USE  at 1+ TO
   BLOCK
   I32 ARG {: p:IR-ID:ir-value-id :}
   MEM ARG TOK!
   p WIDE top WSTRUCT-OPCODE:I64-LT-S I32 OP2  at 3 +  at 2 +  BRZ
   BLOCK
   cx  p 0 LD64  DOT-CALL {: s:IR-ID:ir-value-id :}
   s  at 4 +  at 5 +  BRZ
   BLOCK  0 K32 RET
   BLOCK
   p 8 K32 WSTRUCT-OPCODE:I32-ADD I32 OP2 {: next:IR-ID:ir-value-id :}
   WSTRUCT-OPCODE:BR OPEN  next USE  TOK USE  at 1+ TO
   BLOCK  s RET
   FUN-SHUT ;

\ The code off the stack into ctx.throw-code.
: THROW-ROW ( -- )
   s" throw" 14 0 0 FUN-OPEN
   ENTRY {: cx:IR-ID:ir-value-id :}
   cx WPROF:CTX-STACK-TOP LD32  8 K32  WSTRUCT-OPCODE:I32-SUB I32 OP2 {: t:IR-ID:ir-value-id :}
   cx  t 0 LD64  WPROF:CTX-THROW-CODE ST64
   cx t WPROF:CTX-STACK-TOP ST32
   1 K32 RET
   FUN-SHUT ;

\ Function k's lanes in and out.
: LANES ( n -- n n )
   {: k:n :}
   k EMIT-FUN = if 1 0 exit then
   k DEPTH-FUN = if 0 1 exit then
   0 0 ;

: BUILD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 K-CTX !
   IR-BUILD:PLAN-DEFAULT
   c WSTRUCT:NEW-BUILDER 0 K-BLD !
   CTX BLD ROWS$ IR-BUILD:ADD-SOURCE 0 K-SRC !
   0 MADE !
   EMIT-ROW  DEPTH-ROW  DOT-S-ROW  THROW-ROW
   CTX BLD WSTRUCT:FREEZE [: LANES ;] WENC:ENCODE ;

\ The kernel-words word answering an engine name, or an empty spelling.
: WORD-OF ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   a u s" ." STR=CI if s" WKWORDS:DOT" exit then
   a u s" u." STR=CI if s" WKWORDS:U-DOT" exit then
   a u s" f." STR=CI if s" WKWORDS:F-DOT" exit then
   a u s" type" STR=CI if s" WKWORDS:TYPE-BYTES" exit then
   a u s" cr" STR=CI if s" WKWORDS:NEWLINE" exit then
   a u s" space" STR=CI if s" WKWORDS:BLANK" exit then
   a u s" negate" STR=CI if s" WKWORDS:NEG" exit then
   a u s" abs" STR=CI if s" WKWORDS:ABSOLUTE" exit then
   a u s" min" STR=CI if s" WKWORDS:MINIMUM" exit then
   a u s" /mod" STR=CI if s" WKWORDS:DIVREM" exit then
   a u s" 0<" STR=CI if s" WKWORDS:ZERO-NEG?" exit then
   a u s" 0<>" STR=CI if s" WKWORDS:NONZERO?" exit then
   s" " ;

\ The row answering an engine name, or -1.
: ROW-OF ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u s" emit" STR=CI if EMIT-FUN exit then
   a u s" depth" STR=CI if DEPTH-FUN exit then
   a u s" .s" STR=CI if DOT-S-FUN exit then
   a u s" throw" STR=CI if THROW-FUN exit then
   -1 ;

public

\ The rows as WENC's emission, which the next ENCODE or RETIRE gives back.
: ENCODE ( -- )
   WBACK:BINDING [: BUILD ;] IR-CTX:WITH-CONTEXT ;

: PROVIDER ( ptr u8 n -- WKERNEL:provider )
   {: a:ptr u:n :}
   a u ROW-OF {: k:n :}
   k 0 < 0= if k WKERNEL-PROVIDER:row exit then
   a u WORD-OF {: w:ptr wu:n :}
   wu 0= if E-WLINK-UNRESOLVED throw then
   w wu WKERNEL-PROVIDER:word ;

;package
