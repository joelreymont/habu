\ a64ir.f - the ARM64 machine dialect: the closed set of operations that stand
\ for real ARM64 instruction forms, with virtual registers as SSA values.
\
\ docs/compiler-ir-design.md section 5.3 ("closed-world operation schemas") and
\ section 7.2's stage chain. Section 5.3 line 229 says each dialect has an
\ exhaustive operation family and one schema table; this file is that family for
\ the straight-line integer subset of ARM64, and it fills a module's schema table
\ through the IR-SCHEMA builder that src/compiler/ir/build.f owns. It defines no
\ storage of its own and repeats no check IR-SCHEMA already makes.
\
\ WHAT AN A64IR OPERATION IS. One instruction of the modelled ARM64 vocabulary,
\ with its register operands replaced by SSA values. A value of this dialect is a
\ virtual general register: the operation that defines it is the instruction that
\ writes it, and the operations that take it as an operand are the instructions
\ that read it. Which physical register each one ends up in is the register
\ allocator's answer and is deliberately absent here, and the four bytes each
\ operation encodes to are the emission leaf's answer and are absent here too.
\ Between those two neighbours this file owns exactly one thing: which machine
\ operations exist and what shape each one has.
\
\ THE VOCABULARY IS THE MODELLED ONE, NOT A SECOND ONE. Every opcode below names
\ one form of the 48-form instruction vocabulary that formal/Common/Insn.v models
\ and src/arch/arm64/asm.f encodes, and it carries only operands that form has:
\   a64.movz  Movz rd imm hw    - write a 16-bit half into a cleared register
\   a64.movk  Movk rd imm sh    - overwrite one 16-bit half, keeping the rest
\   a64.add   Add rd rn rm      - 64-bit register addition
\   a64.sub   Sub rd rn rm      - 64-bit register subtraction
\   a64.mul   Mul rd rn rm      - 64-bit register multiplication
\   a64.ret   Ret               - return to the address in the link register
\ There is no opcode here for a form no pass in the chain produces yet. An opcode
\ with no selection rule and no emission would be a promise, not a schema.
\
\ WHY MOVK TAKES AN OPERAND WHEN THE INSTRUCTION HAS ONE REGISTER. Movk keeps the
\ bits of rd it does not write, so the register it names is both a source and a
\ destination. In SSA a value is written once, so the value the instruction keeps
\ has to be named: a64.movk reads the value the previous half left and defines
\ the value with this half merged in. That is what makes a materialised 64-bit
\ constant a chain of operations the allocator can read rather than a hidden
\ update of a register nobody declared.
\
\ WHY A RETURN CARRIES OPERANDS WHEN THE INSTRUCTION CARRIES NONE. The Ret form
\ reads no register the assembler names, but the values a word returns are still
\ live where control leaves, and something has to say so or the allocator is free
\ to reuse their registers one instruction early. The returned values are
\ therefore the terminator's operands, exactly as they are in the HIR dialect
\ this stage selects from. Which physical registers they must sit in at that
\ point is the target contract's answer, not this dialect's.
\
\ THE MACHINE BOUNDS. Two, and both come off the move-wide form. Its immediate
\ field is sixteen bits, so a half is 0..65535; its half selector is two bits, so
\ a 64-bit register is written in four halves and a legal shift is 0, 16, 32 or
\ 48. They are written here as the field widths they are - the same way
\ src/compiler/a64-effect.f writes its bounds - and test/compiler/native-a64ir.f
\ reads src/arch/arm64/asm.f's own IMM16-LIM and HW-LIM back and asserts them
\ against these, so a bound that moved in the assembler reddens this dialect
\ instead of silently disagreeing with it.
\
\ NOTHING HERE MAY TRAP. Add, Sub and Mul on ARM64 wrap; none of the six forms
\ raises on overflow. That is why every schema below declares no trap, and it is
\ why the selector refuses a source operation whose own schema says it may trap:
\ a trapping addition needs a flag-setting form and a conditional branch to a
\ trap target, and none of that is in this dialect yet.
\
\ ONE REGISTER CLASS, DELIBERATELY NAMED. Every value of this dialect is a 64-bit
\ general-register value, and GPR-TYPE is the single place that says so. The
\ floating and SIMD register files, the stack pointer, frame slots, labels and
\ fixups are further operand records of the same dialect and are not here yet; the
\ seam where they arrive is this one reader, which is why it exists instead of
\ each schema interning its own type inline.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/type.f
require src/compiler/ir/schema.f
require src/compiler/ir/build.f

package A64IR
public

\ The whole operation family of the straight-line integer subset. It is an ENUM
\ so design line 229's closed world is a property of the type: a selection rule
\ cannot name a machine operation this dialect does not have, and every MATCH
\ over it has to answer for all six.
ENUM opcode DERIVE eq
   movz
   movk
   add
   sub
   mul
   ret
;ENUM

private

\ ---- the machine bounds ------------------------------------------------------
\ Read off the move-wide form: a 16-bit immediate field and a 2-bit half
\ selector, over a 64-bit register.
64 constant XBITS                    \ bits in a general register
16 constant IMM-BITS                 \ the move-wide immediate field
2 constant HW-BITS                   \ the move-wide half selector

1 IMM-BITS lshift constant IMM-LIM   \ a half holds 0 .. IMM-LIM-1
1 HW-BITS lshift constant HALVES-N   \ four selectable halves
XBITS HALVES-N / constant HALF-N     \ bits per half

$FFFF constant HALF-MASK

\ ---- the dialect's own symbols -----------------------------------------------
\ Every symbol this dialect mints is spelled `a64.`-something, so a dialect
\ symbol and any other name in the module's one interner can never collide.

: TARGET ( -- )
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET ;

\ Design lines 236-238: a value-producing machine operation ends no block, names
\ no successor, holds no region, and carries no effect token. None of the six
\ forms touches memory, so none of them takes a memory effect either.
: PURE-VALUE ( -- )
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE ;

\ Design line 240: no form of this dialect raises.
: TOTAL ( -- )
   false IR-SCHEMA:SET-TRAP ;

public

\ ---- the dialect identity ----------------------------------------------------
: NAME ( -- ptr u8 n )
   s" a64" ;

\ Version 0.1: the integer subset is not the whole machine, and the major version
\ stays at zero until it is.
0 constant MAJOR
1 constant MINOR

\ ---- the machine bounds, for a consumer that has to agree with them -----------
\ A pass that materialises a constant walks the halves of a register, and it asks
\ here rather than repeating the arithmetic.
: REG-BITS ( -- n )      XBITS ;
: HALVES ( -- n )        HALVES-N ;
: HALF-BITS ( -- n )     HALF-N ;
: IMM-LIMIT ( -- n )     IMM-LIM ;

\ The i-th 16-bit half of a 64-bit value, counting from the least significant.
\ The shift is logical, so a negative value reads as the bit pattern the machine
\ holds, which is what a move-wide chain has to reproduce.
: HALF-OF ( n n -- n )
   {: v:n i:n :}
   i 0 < i HALVES-N >= or if E-A64IR-SHIFT throw then
   v i HALF-N * rshift HALF-MASK and ;

\ The shift, in bits, that selects the i-th half.
: HALF-SHIFT ( n -- n )
   {: i:n :}
   i 0 < i HALVES-N >= or if E-A64IR-SHIFT throw then
   i HALF-N * ;

private

\ ---- checked move-wide operands ----------------------------------------------
\ A move-wide immediate that does not fit the field, and a shift that does not
\ select a half. They are private because a caller reaches both fields through
\ the attribute builders below, so there is no route to an operand that skipped
\ its bound.
: IMM16 ( n -- n )
   dup 0 < over IMM-LIM >= or if E-A64IR-IMM throw then ;

: HALF ( n -- n )
   dup 0 < over XBITS >= or if E-A64IR-SHIFT throw then
   dup HALF-N mod 0<> if E-A64IR-SHIFT throw then ;

public

\ ---- the type of a virtual register ------------------------------------------
\ One 64-bit general-register value. Every operand and every result of this
\ dialect has this type today; a second register class arrives as a second
\ reader beside this one, never as a raw type interned at a use site.
: GPR-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ ---- the opcode names --------------------------------------------------------
\ This module's interned symbol for one opcode. Interning deduplicates, so asking
\ twice answers the same identity, and this is the symbol both IR-SCHEMA's
\ readers and IR-BUILD:BEGIN-OP take.
: OPCODE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz OF s" a64.movz" ENDOF
      movk OF s" a64.movk" ENDOF
      add  OF s" a64.add"  ENDOF
      sub  OF s" a64.sub"  ENDOF
      mul  OF s" a64.mul"  ENDOF
      ret  OF s" a64.ret"  ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

\ Design line 479: the two attribute keys a move-wide operation requires. The
\ immediate and the half it goes into are the whole content of a move, so a move
\ without either means nothing, and IR-OP refuses one that omits it.
: KEY-IMM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.imm" IR-BUILD:INTERN-SYMBOL ;

: KEY-SHIFT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.shift" IR-BUILD:INTERN-SYMBOL ;

\ The two attribute values, each refused before it is interned if it does not fit
\ the field it names. A pass building a move goes through these, so there is no
\ route by which an out-of-range move-wide operand reaches a module at all.
: IMM-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   IMM16 IR-BUILD:INTERN-INT-ATTR ;

: SHIFT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   HALF IR-BUILD:INTERN-INT-ATTR ;

private

\ ---- the schema definitions --------------------------------------------------
\ Design lines 242 and 243 require a semantic-rule identifier and a renderer
\ identifier per schema, so a later pass dispatches on an identity rather than on
\ a string comparison. Each opcode names its own, derived from its own spelling.
\ Neither is public: the schema table is the authority on what an opcode's rule
\ and renderer are, and IR-SCHEMA:RULE@ and RENDERER@ answer it.
: RULE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz OF s" a64.rule.movz" ENDOF
      movk OF s" a64.rule.movk" ENDOF
      add  OF s" a64.rule.add"  ENDOF
      sub  OF s" a64.rule.sub"  ENDOF
      mul  OF s" a64.rule.mul"  ENDOF
      ret  OF s" a64.rule.ret"  ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

: RENDERER ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz OF s" a64.render.movz" ENDOF
      movk OF s" a64.render.movk" ENDOF
      add  OF s" a64.render.add"  ENDOF
      sub  OF s" a64.render.sub"  ENDOF
      mul  OF s" a64.render.mul"  ENDOF
      ret  OF s" a64.render.ret"  ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

\ The two fields every schema of this dialect names the same way.
: NAMED ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b o RULE IR-SCHEMA:SET-RULE
   c b o RENDERER IR-SCHEMA:SET-RENDERER ;

\ The two attribute keys a move-wide operation declares, in the order a builder
\ has to present them.
: MOVE-ATTRS ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b KEY-IMM IR-SCHEMA:ADD-ATTR
   c b KEY-SHIFT IR-SCHEMA:ADD-ATTR ;

\ Movz: no register is read, one is written, and the immediate and its half say
\ what goes into it.
: DEF-MOVZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOVZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b MOVE-ATTRS
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOVZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Movk: the value whose other halves survive is the operand, and the value with
\ this half merged in is the result.
: DEF-MOVK ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOVK OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b MOVE-ATTRS
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOVK NAMED
   c b IR-BUILD:DEFINE-OP ;

\ One shifted-register three-operand form: two registers read, one written. The
\ three arithmetic opcodes differ only in their names, so they share this shape.
: DEF-BINARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Ret: the block's one terminator. Design line 237 makes it a terminator and
\ design lines 706-708 give a terminator no results of its own; the values still
\ live where control leaves are its operands, and how many there are is a
\ property of the routine rather than of the form, so the list is one variadic
\ cell.
: DEF-RET ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RET OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:RET NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the table this dialect may fill -----------------------------------------
\ Design line 229's closed world is per dialect, so an operation family may only
\ be defined into the schema table of the dialect it belongs to. The table's
\ dialect name and schema version are fixed when the module is created and
\ nothing can change them afterwards, so reading them back off the live module
\ decides it: the name is compared byte for byte through the module's own
\ interner, which appends nothing, and the version has to be the exact version
\ these definitions were written for.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64IR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MAJOR@ MAJOR <> if E-A64IR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MINOR@ MINOR <> if E-A64IR-DIALECT throw then ;

public

\ ---- creation and registration -----------------------------------------------
\ Create a builder for a module of this dialect. The staged IR-BUILD plan is
\ consumed here exactly as IR-BUILD:NEW-BUILDER consumes it; what this word adds
\ is the dialect's own name and schema version, which no caller should be
\ spelling out.
: NEW-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   NAME MAJOR MINOR IR-BUILD:NEW-BUILDER ;

\ Define the whole machine operation family into this builder's schema table.
\ Nearly every check belongs to IR-SCHEMA:DEFINE - the module owns each symbol
\ and type, the target contract admits the requirement, no opcode is defined
\ twice, the ceilings hold - so registering twice, or against a module or a
\ target that cannot hold these schemas, is refused there and this word repeats
\ none of it. The one check that is this dialect's own is the first line, because
\ IR-SCHEMA has no opinion about which dialect its caller is.
: REGISTER ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b DIALECT-CK
   c b GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b t DEF-MOVZ
   c b t DEF-MOVK
   c b t A64IR-OPCODE:ADD DEF-BINARY
   c b t A64IR-OPCODE:SUB DEF-BINARY
   c b t A64IR-OPCODE:MUL DEF-BINARY
   c b t DEF-RET ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
