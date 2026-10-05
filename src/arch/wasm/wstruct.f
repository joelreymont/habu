\ wstruct.f - WSTRUCT, the Wasm backend's dialect: WebAssembly instructions as
\ operations of a control-flow graph on the IR substrate.
\
\ WHY A DIALECT OF ITS OWN. NBACK:FREEZE hands every selector a module frozen
\ with IR-BUILD:FREEZE-INTERIM (src/compiler/native/backend.f), which skips the
\ schema, dominance, terminator, single-definition, successor-argument and span
\ checks (src/compiler/ir/build.f FREEZE-INTERIM). A native backend gets them
\ back by verifying its machine module whole; a Wasm backend has no machine
\ module, so the selector builds a WSTRUCT module and freezes it with the full
\ IR-BUILD:FREEZE before anything encodes it (docs/wasm-backend.md 17.2).
\
\ A GRAPH, NOT A TREE. Blocks take typed i32, i64 and f64 arguments, a branch
\ hands its destination those arguments, and Wasm's block, loop and if are not
\ operations here: the substrate records a region count per schema
\ (src/compiler/ir/schema.f REGION-MAX) but IR-BUILD has no way to build a
\ region, and the full freeze only means something - dominance, one definition,
\ successor arguments - over a graph. src/arch/wasm/structure.f derives the
\ dominators, loops and control tree from the frozen graph and the encoder
\ assigns label depths as it writes.
\
\ TWO-WAY BRANCHES CARRY NO ARGUMENTS. `brz` names two successors and the
\ substrate has no per-edge argument window, so both of its destinations take
\ none and its one operand is the condition (src/compiler/ir/verify.f
\ SUCCARGS-CK; HIR's brz is the same shape). Block arguments travel on `br`.
\
\ THE MEMORY ORDER IS A VALUE. Every load, store and call takes a memory token
\ and answers the next one, so "this load happens after that store" is a
\ dependency the module holds (the verifier refuses an effect with no token to
\ carry it). No operation mints a token: a function's first one is an argument
\ of its entry block, so every effect it performs is ordered after its entry,
\ and a join takes the token as a block argument like any other value. A token
\ is not a Wasm value; the encoder writes nothing for one.
\
\ THE CALL IS THE HABU CALL. A schema states one type for a variadic tail, so a
\ call is the internal convention of section 7.1 and not every Wasm signature:
\ the context address and the input lanes in, the status and the output lanes
\ out, every lane an i64. `return` answers the same row.
\
\ AN ADDRESS IS A KIND OF CONSTANT. An i64.const states whether its value is a
\ number, a data address or a code address (HIR's three kinds, src/compiler/
\ native/hir.f ADDR-NONE..ADDR-CODE), because the encoder writes an address as a
\ padded field the linker rewrites and a number as itself.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/native/backend.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/type.f
require src/compiler/ir/schema.f
require src/compiler/ir/build.f

\ WSTRUCT's codes, -9805..-9809, in the Wasm backend's block -9800..-9829.
-9805 constant E-WSTRUCT-FIRST
-9809 constant E-WSTRUCT-LAST
-9805 constant E-WSTRUCT-DIALECT   \ a module whose schema table was created for another dialect or another schema version
-9806 constant E-WSTRUCT-OPCODE    \ an ordinal outside the dialect's closed opcode vocabulary
-9808 constant E-WSTRUCT-ADDR      \ an address kind outside NONE, DATA and CODE

package WSTRUCT
public

\ An ENUM, so a selection rule cannot name an instruction this dialect does not
\ have and every MATCH over it answers for every member.
ENUM opcode DERIVE eq
   i32-const
   i64-const
   f64-const
   i32-add
   i32-sub
   i64-add
   i64-sub
   i64-mul
   i64-div-s
   i64-and
   i64-or
   i64-xor
   i64-shl
   i64-shr-u
   i64-eqz
   i64-eq
   i64-ne
   i64-lt-s
   i64-gt-s
   i64-le-s
   i64-ge-s
   f64-add
   f64-sub
   f64-mul
   f64-div
   f64-sqrt
   f64-neg
   f64-abs
   f64-eq
   f64-ne
   f64-lt
   f64-gt
   i32-wrap-i64
   i64-extend-i32-u
   f64-convert-i64-s
   i64-trunc-sat-f64-s
   i64-reinterpret-f64
   f64-reinterpret-i64
   f64-select
   i32-load
   i64-load
   i64-load8-u
   i32-store
   i64-store
   i64-store8
   br
   brz
   return
   unreachable
   call
   call-indirect
;ENUM

\ ---- the dialect identity ----------------------------------------------------
: NAME ( -- ptr u8 n )
   s" wstruct" ;

\ Every consumer compares the version exactly, so a table with a form and one
\ without are two different tables.
0 constant MAJOR
2 constant MINOR

\ ---- the opcode spellings ----------------------------------------------------
\ The Wasm mnemonic under the dialect's prefix. A Wasm instruction's semantic
\ rule is the specification's rule for that mnemonic and its rendering is that
\ mnemonic, so the spelling is also each schema's rule and renderer identifier,
\ and what a reader of a frozen module looks the opcode up by.
: OP-NAME ( WSTRUCT:opcode -- ptr u8 n )
   MATCH opcode
      i32-const           OF s" wstruct.i32.const" ENDOF
      i64-const           OF s" wstruct.i64.const" ENDOF
      f64-const           OF s" wstruct.f64.const" ENDOF
      i32-add             OF s" wstruct.i32.add" ENDOF
      i32-sub             OF s" wstruct.i32.sub" ENDOF
      i64-add             OF s" wstruct.i64.add" ENDOF
      i64-sub             OF s" wstruct.i64.sub" ENDOF
      i64-mul             OF s" wstruct.i64.mul" ENDOF
      i64-div-s           OF s" wstruct.i64.div_s" ENDOF
      i64-and             OF s" wstruct.i64.and" ENDOF
      i64-or              OF s" wstruct.i64.or" ENDOF
      i64-xor             OF s" wstruct.i64.xor" ENDOF
      i64-shl             OF s" wstruct.i64.shl" ENDOF
      i64-shr-u           OF s" wstruct.i64.shr_u" ENDOF
      i64-eqz             OF s" wstruct.i64.eqz" ENDOF
      i64-eq              OF s" wstruct.i64.eq" ENDOF
      i64-ne              OF s" wstruct.i64.ne" ENDOF
      i64-lt-s            OF s" wstruct.i64.lt_s" ENDOF
      i64-gt-s            OF s" wstruct.i64.gt_s" ENDOF
      i64-le-s            OF s" wstruct.i64.le_s" ENDOF
      i64-ge-s            OF s" wstruct.i64.ge_s" ENDOF
      f64-add             OF s" wstruct.f64.add" ENDOF
      f64-sub             OF s" wstruct.f64.sub" ENDOF
      f64-mul             OF s" wstruct.f64.mul" ENDOF
      f64-div             OF s" wstruct.f64.div" ENDOF
      f64-sqrt            OF s" wstruct.f64.sqrt" ENDOF
      f64-neg             OF s" wstruct.f64.neg" ENDOF
      f64-abs             OF s" wstruct.f64.abs" ENDOF
      f64-eq              OF s" wstruct.f64.eq" ENDOF
      f64-ne              OF s" wstruct.f64.ne" ENDOF
      f64-lt              OF s" wstruct.f64.lt" ENDOF
      f64-gt              OF s" wstruct.f64.gt" ENDOF
      i32-wrap-i64        OF s" wstruct.i32.wrap_i64" ENDOF
      i64-extend-i32-u    OF s" wstruct.i64.extend_i32_u" ENDOF
      f64-convert-i64-s   OF s" wstruct.f64.convert_i64_s" ENDOF
      i64-trunc-sat-f64-s OF s" wstruct.i64.trunc_sat_f64_s" ENDOF
      i64-reinterpret-f64 OF s" wstruct.i64.reinterpret_f64" ENDOF
      f64-reinterpret-i64 OF s" wstruct.f64.reinterpret_i64" ENDOF
      f64-select          OF s" wstruct.f64.select" ENDOF
      i32-load            OF s" wstruct.i32.load" ENDOF
      i64-load            OF s" wstruct.i64.load" ENDOF
      i64-load8-u         OF s" wstruct.i64.load8_u" ENDOF
      i32-store           OF s" wstruct.i32.store" ENDOF
      i64-store           OF s" wstruct.i64.store" ENDOF
      i64-store8          OF s" wstruct.i64.store8" ENDOF
      br                  OF s" wstruct.br" ENDOF
      brz                 OF s" wstruct.brz" ENDOF
      return              OF s" wstruct.return" ENDOF
      unreachable         OF s" wstruct.unreachable" ENDOF
      call                OF s" wstruct.call" ENDOF
      call-indirect       OF s" wstruct.call_indirect" ENDOF
   ;MATCH ;

\ ---- the closed opcode vocabulary --------------------------------------------
\ The ordinal is a position in this one table, stated here and never derived
\ from the enum's declaration order.
51 constant OPCODES

: NTH ( n -- WSTRUCT:opcode )
   case
      0  of WSTRUCT-OPCODE:I32-CONST endof
      1  of WSTRUCT-OPCODE:I64-CONST endof
      2  of WSTRUCT-OPCODE:F64-CONST endof
      3  of WSTRUCT-OPCODE:I32-ADD endof
      4  of WSTRUCT-OPCODE:I32-SUB endof
      5  of WSTRUCT-OPCODE:I64-ADD endof
      6  of WSTRUCT-OPCODE:I64-SUB endof
      7  of WSTRUCT-OPCODE:I64-MUL endof
      8  of WSTRUCT-OPCODE:I64-DIV-S endof
      9  of WSTRUCT-OPCODE:I64-AND endof
      10 of WSTRUCT-OPCODE:I64-OR endof
      11 of WSTRUCT-OPCODE:I64-XOR endof
      12 of WSTRUCT-OPCODE:I64-SHL endof
      13 of WSTRUCT-OPCODE:I64-SHR-U endof
      14 of WSTRUCT-OPCODE:I64-EQZ endof
      15 of WSTRUCT-OPCODE:I64-EQ endof
      16 of WSTRUCT-OPCODE:I64-NE endof
      17 of WSTRUCT-OPCODE:I64-LT-S endof
      18 of WSTRUCT-OPCODE:I64-GT-S endof
      19 of WSTRUCT-OPCODE:I64-LE-S endof
      20 of WSTRUCT-OPCODE:I64-GE-S endof
      21 of WSTRUCT-OPCODE:F64-ADD endof
      22 of WSTRUCT-OPCODE:F64-SUB endof
      23 of WSTRUCT-OPCODE:F64-MUL endof
      24 of WSTRUCT-OPCODE:F64-DIV endof
      25 of WSTRUCT-OPCODE:F64-SQRT endof
      26 of WSTRUCT-OPCODE:F64-NEG endof
      27 of WSTRUCT-OPCODE:F64-ABS endof
      28 of WSTRUCT-OPCODE:F64-EQ endof
      29 of WSTRUCT-OPCODE:F64-NE endof
      30 of WSTRUCT-OPCODE:F64-LT endof
      31 of WSTRUCT-OPCODE:F64-GT endof
      32 of WSTRUCT-OPCODE:I32-WRAP-I64 endof
      33 of WSTRUCT-OPCODE:I64-EXTEND-I32-U endof
      34 of WSTRUCT-OPCODE:F64-CONVERT-I64-S endof
      35 of WSTRUCT-OPCODE:I64-TRUNC-SAT-F64-S endof
      36 of WSTRUCT-OPCODE:I64-REINTERPRET-F64 endof
      37 of WSTRUCT-OPCODE:F64-REINTERPRET-I64 endof
      38 of WSTRUCT-OPCODE:F64-SELECT endof
      39 of WSTRUCT-OPCODE:I32-LOAD endof
      40 of WSTRUCT-OPCODE:I64-LOAD endof
      41 of WSTRUCT-OPCODE:I64-LOAD8-U endof
      42 of WSTRUCT-OPCODE:I32-STORE endof
      43 of WSTRUCT-OPCODE:I64-STORE endof
      44 of WSTRUCT-OPCODE:I64-STORE8 endof
      45 of WSTRUCT-OPCODE:BR endof
      46 of WSTRUCT-OPCODE:BRZ endof
      47 of WSTRUCT-OPCODE:RETURN endof
      48 of WSTRUCT-OPCODE:UNREACHABLE endof
      49 of WSTRUCT-OPCODE:CALL endof
      50 of WSTRUCT-OPCODE:CALL-INDIRECT endof
      E-WSTRUCT-OPCODE throw
   endcase ;

\ ---- the types ---------------------------------------------------------------
\ Wasm's integers carry no sign - the instruction decides, div_s against shr_u -
\ so both widths intern as the substrate's signed integer of their width.
: I32-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W32 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: I64-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ The type table refuses a double under a contract without scalar floating
\ point, so asking for this type is itself the first f64 refusal.
: F64-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-FMT:DOUBLE IR-BUILD:INTERN-FLT ;

: MEM-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-DOMAIN:DATA-MEM IR-BUILD:INTERN-TOKEN ;

\ ---- the attribute keys ------------------------------------------------------
\ Each is spelled once: a builder interns the spelling, and a reader of a
\ frozen module looks the key up by it.
\ A constant's value: an i32 constant's value is the signed 32-bit number it
\ pushes, an f64 constant's is its IEEE 754 bits.
: KEY-VALUE$ ( -- ptr u8 n )    s" wstruct.value" ;

: KEY-VALUE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   KEY-VALUE$ IR-BUILD:INTERN-SYMBOL ;

\ A memory access's memarg: the alignment as its base-2 exponent, as Wasm
\ encodes it, and the unsigned byte offset added to the address operand.
: KEY-ALIGN$ ( -- ptr u8 n )    s" wstruct.align" ;

: KEY-ALIGN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   KEY-ALIGN$ IR-BUILD:INTERN-SYMBOL ;

: KEY-OFFSET$ ( -- ptr u8 n )   s" wstruct.offset" ;

: KEY-OFFSET ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   KEY-OFFSET$ IR-BUILD:INTERN-SYMBOL ;

\ A direct call's target, a symbol attribute naming the callee.
: KEY-CALLEE$ ( -- ptr u8 n )   s" wstruct.callee" ;

: KEY-CALLEE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   KEY-CALLEE$ IR-BUILD:INTERN-SYMBOL ;

\ An i64.const's address kind, one of the three below.
: KEY-ADDR$ ( -- ptr u8 n )     s" wstruct.addr" ;

: KEY-ADDR ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   KEY-ADDR$ IR-BUILD:INTERN-SYMBOL ;

0 constant ADDR-NONE
1 constant ADDR-DATA
2 constant ADDR-CODE

\ Refused where the attribute is built, as HIR refuses its own.
: ADDR-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   dup ADDR-NONE < over ADDR-CODE > or if E-WSTRUCT-ADDR throw then
   IR-BUILD:INTERN-INT-ATTR ;

\ Interning deduplicates, so asking twice answers the same identity.
: OPCODE ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode -- IR-ID:ir-symbol-id )
   OP-NAME IR-BUILD:INTERN-SYMBOL ;

private

\ ---- the target every schema names -------------------------------------------
\ Every form needs a Wasm contract; a form that reads or writes a double also
\ needs its scalar floating point.
: F-INT ( -- CTARGET:features )
   CTARGET:F-BASE ;

: F-FLT ( -- CTARGET:features )
   CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH ;

\ The four fields every form closes with. The spelling is the rule and the
\ renderer (OP-NAME).
: FINISH ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id CTARGET:features -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id f:CTARGET:features :}
   CTARGET-ARCH:WASM f IR-SCHEMA:SET-TARGET
   op IR-SCHEMA:SET-RULE
   op IR-SCHEMA:SET-RENDERER
   c b IR-BUILD:DEFINE-OP ;

\ A value-producing operation ends no block, names no successor, holds no
\ region and carries no effect token.
: PURE-VALUE ( -- )
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE ;

: LINEAR-MEMORY ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

\ ---- the forms ---------------------------------------------------------------
: CONST-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id CTARGET:features -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id t:IR-ID:ir-type-id
      f:CTARGET:features :}
   op IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-VALUE IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   c b op f FINISH ;

\ The cell-wide constant, the one form a Habu literal selects to, also states
\ its address kind.
: CELL-CONST-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id t:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-VALUE IR-SCHEMA:ADD-ATTR
   c b KEY-ADDR IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ Two operands of one type: the arithmetic, the bitwise forms, the shifts -
\ whose count is the second operand, taken modulo the width - and the
\ comparisons, whose result is an i32 truth value.
: BINARY-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id IR-ID:ir-type-id CTARGET:features -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id ti:IR-ID:ir-type-id
      to:IR-ID:ir-type-id f:CTARGET:features :}
   op IR-SCHEMA:BEGIN-OP
   ti IR-SCHEMA:ADD-OPERAND
   ti IR-SCHEMA:ADD-OPERAND
   to IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   c b op f FINISH ;

\ i64.div_s traps on a zero divisor and on MIN-N / -1, where Habu throws and
\ wraps; the selector guards both before it reaches this form.
: DIVIDE-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id t:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   true IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ One operand: eqz, the f64 square root, negation and magnitude, and every
\ conversion. None traps: the truncation is the saturating one, which answers
\ where i64.trunc_f64_s would trap, and is why WPROF:FEATURES lists
\ saturating-float-to-int.
: UNARY-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id IR-ID:ir-type-id CTARGET:features -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id ti:IR-ID:ir-type-id
      to:IR-ID:ir-type-id f:CTARGET:features :}
   op IR-SCHEMA:BEGIN-OP
   ti IR-SCHEMA:ADD-OPERAND
   to IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   c b op f FINISH ;

\ Wasm's select: the first operand when the i32 third is not zero, else the
\ second.
: SELECT-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id IR-ID:ir-type-id CTARGET:features -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id t:IR-ID:ir-type-id
      w:IR-ID:ir-type-id f:CTARGET:features :}
   op IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   w IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   c b op f FINISH ;

\ The address is an i32 offset into the one memory; the memory token is the
\ last operand and the last result. An access past the memory traps.
: LOAD-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id t:IR-ID:ir-type-id
      a:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   a IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   c b KEY-ALIGN IR-SCHEMA:ADD-ATTR
   c b KEY-OFFSET IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ LINEAR-MEMORY
   true IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ Wasm's operand order, the address and then the value stored.
: STORE-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id t:IR-ID:ir-type-id
      a:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   a IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-ALIGN IR-SCHEMA:ADD-ATTR
   c b KEY-OFFSET IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE LINEAR-MEMORY
   true IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ The operands are the destination's block arguments. With one successor the
\ verifier types each against the argument it becomes, never against the
\ schema, so the tail's i64 is the arity rule and not a type rule.
: BR-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id t:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 1 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ The first successor when the i32 condition is zero, the second otherwise -
\ HIR's order, so a selector maps one onto the other.
: BRZ-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id w:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   w IR-SCHEMA:ADD-OPERAND
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ The status and then the output lanes.
: RETURN-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id s:IR-ID:ir-type-id
      t:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   s IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ A terminator, so it is the last operation of its block and every effect
\ before it in the block - the fault a failed check records - is already done.
: UNREACHABLE-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   true IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ The context address and the token, then the input lanes; the status and the
\ token, then the output lanes. The callee may trap and may touch any memory.
: CALL-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id s:IR-ID:ir-type-id
      t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   s IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND-TAIL
   s IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   t IR-SCHEMA:ADD-RESULT-TAIL
   c b KEY-CALLEE IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE LINEAR-MEMORY
   true IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ The same row with the callee's table slot, an i32, after the context address.
\ A slot outside the table or a function of another signature traps.
: CALL-INDIRECT-FORM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id IR-ID:ir-type-id IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id s:IR-ID:ir-type-id
      t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   op IR-SCHEMA:BEGIN-OP
   s IR-SCHEMA:ADD-OPERAND
   s IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND-TAIL
   s IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   t IR-SCHEMA:ADD-RESULT-TAIL
   IR--SCHEMA-EFFECT:READ-WRITE LINEAR-MEMORY
   true IR-SCHEMA:SET-TRAP
   c b op F-INT FINISH ;

\ ---- defining one opcode -----------------------------------------------------
\ Every type an arm needs is interned before its form opens the schema stage,
\ so a refused type leaves no stage open; the double is interned only by the
\ arms that use one.
: DEFINE ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode op:IR-ID:ir-symbol-id :}
   c b I32-TYPE {: w:IR-ID:ir-type-id :}
   c b I64-TYPE {: x:IR-ID:ir-type-id :}
   c b MEM-TYPE {: k:IR-ID:ir-type-id :}
   o MATCH opcode
      i32-const           OF c b op w F-INT CONST-FORM ENDOF
      i64-const           OF c b op x CELL-CONST-FORM ENDOF
      f64-const           OF c b op c b F64-TYPE F-FLT CONST-FORM ENDOF
      i32-add             OF c b op w w F-INT BINARY-FORM ENDOF
      i32-sub             OF c b op w w F-INT BINARY-FORM ENDOF
      i64-add             OF c b op x x F-INT BINARY-FORM ENDOF
      i64-sub             OF c b op x x F-INT BINARY-FORM ENDOF
      i64-mul             OF c b op x x F-INT BINARY-FORM ENDOF
      i64-div-s           OF c b op x DIVIDE-FORM ENDOF
      i64-and             OF c b op x x F-INT BINARY-FORM ENDOF
      i64-or              OF c b op x x F-INT BINARY-FORM ENDOF
      i64-xor             OF c b op x x F-INT BINARY-FORM ENDOF
      i64-shl             OF c b op x x F-INT BINARY-FORM ENDOF
      i64-shr-u           OF c b op x x F-INT BINARY-FORM ENDOF
      i64-eqz             OF c b op x w F-INT UNARY-FORM ENDOF
      i64-eq              OF c b op x w F-INT BINARY-FORM ENDOF
      i64-ne              OF c b op x w F-INT BINARY-FORM ENDOF
      i64-lt-s            OF c b op x w F-INT BINARY-FORM ENDOF
      i64-gt-s            OF c b op x w F-INT BINARY-FORM ENDOF
      i64-le-s            OF c b op x w F-INT BINARY-FORM ENDOF
      i64-ge-s            OF c b op x w F-INT BINARY-FORM ENDOF
      f64-add             OF c b op c b F64-TYPE dup F-FLT BINARY-FORM ENDOF
      f64-sub             OF c b op c b F64-TYPE dup F-FLT BINARY-FORM ENDOF
      f64-mul             OF c b op c b F64-TYPE dup F-FLT BINARY-FORM ENDOF
      f64-div             OF c b op c b F64-TYPE dup F-FLT BINARY-FORM ENDOF
      f64-sqrt            OF c b op c b F64-TYPE dup F-FLT UNARY-FORM ENDOF
      f64-neg             OF c b op c b F64-TYPE dup F-FLT UNARY-FORM ENDOF
      f64-abs             OF c b op c b F64-TYPE dup F-FLT UNARY-FORM ENDOF
      f64-eq              OF c b op c b F64-TYPE w F-FLT BINARY-FORM ENDOF
      f64-ne              OF c b op c b F64-TYPE w F-FLT BINARY-FORM ENDOF
      f64-lt              OF c b op c b F64-TYPE w F-FLT BINARY-FORM ENDOF
      f64-gt              OF c b op c b F64-TYPE w F-FLT BINARY-FORM ENDOF
      i32-wrap-i64        OF c b op x w F-INT UNARY-FORM ENDOF
      i64-extend-i32-u    OF c b op w x F-INT UNARY-FORM ENDOF
      f64-convert-i64-s   OF c b op x c b F64-TYPE F-FLT UNARY-FORM ENDOF
      i64-trunc-sat-f64-s OF c b op c b F64-TYPE x F-FLT UNARY-FORM ENDOF
      i64-reinterpret-f64 OF c b op c b F64-TYPE x F-FLT UNARY-FORM ENDOF
      f64-reinterpret-i64 OF c b op x c b F64-TYPE F-FLT UNARY-FORM ENDOF
      f64-select          OF c b op c b F64-TYPE w F-FLT SELECT-FORM ENDOF
      i32-load            OF c b op w w k LOAD-FORM ENDOF
      i64-load            OF c b op x w k LOAD-FORM ENDOF
      i64-load8-u         OF c b op x w k LOAD-FORM ENDOF
      i32-store           OF c b op w w k STORE-FORM ENDOF
      i64-store           OF c b op x w k STORE-FORM ENDOF
      i64-store8          OF c b op x w k STORE-FORM ENDOF
      br                  OF c b op x BR-FORM ENDOF
      brz                 OF c b op w BRZ-FORM ENDOF
      return              OF c b op w x RETURN-FORM ENDOF
      unreachable         OF c b op UNREACHABLE-FORM ENDOF
      call                OF c b op w x k CALL-FORM ENDOF
      call-indirect       OF c b op w x k CALL-INDIRECT-FORM ENDOF
   ;MATCH ;

\ ---- the machine this compilation is for --------------------------------------
\ A coherent Wasm target can own HIR; producing a Wasm module for it is the
\ registry's question. With no Wasm backend loaded the registry refuses with
\ E-CTGT-UNLOADED; a loaded one that does not serve this contract refuses here,
\ before anything is allocated.
: CHECK-TARGET ( IR-CTX:ctx -- )
   IR-CTX:BINDING@ CBIND:VALIDATE CBIND:TARGET@
   NBACK:LOWERS? 0= if E-IR-SCHEMA-TARGET throw then ;

\ The table's dialect name and version are fixed when the module is created, so
\ reading them back off the live module decides whose table it is.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  NAME IR-BUILD:SYMBOL-IS?
   0= if E-WSTRUCT-DIALECT throw then
   c b IR-BUILD:SCHEMA-MAJOR@ MAJOR <> if E-WSTRUCT-DIALECT throw then
   c b IR-BUILD:SCHEMA-MINOR@ MINOR <> if E-WSTRUCT-DIALECT throw then ;

public

\ ---- creation and materialisation --------------------------------------------
: NEW-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   dup CHECK-TARGET
   NAME MAJOR MINOR IR-BUILD:NEW-BUILDER ;

\ Define a requested opcode's schema the first time a module asks for it; the
\ module's schema table stays the one authority on what it holds.
: ENSURE-OP ( IR-CTX:ctx IR-BUILD:builder WSTRUCT:opcode -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:WSTRUCT:opcode :}
   c b DIALECT-CK
   c b o OPCODE {: op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-DEFINED? 0= if c b o op DEFINE then
   op ;

\ ---- the freeze ---------------------------------------------------------------
\ What a selector calls in place of IR-BUILD:FREEZE: the full freeze of a
\ WSTRUCT table.
: FREEZE ( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b DIALECT-CK
   c b IR-BUILD:FREEZE ;

;package
