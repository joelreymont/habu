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
\   a64.movz     Movz rd imm hw  - write a 16-bit half into a cleared register
\   a64.movk     Movk rd imm sh  - overwrite one 16-bit half, keeping the rest
\   a64.mov      Orr rd xzr rm   - copy one register into another
\   a64.add      Add rd rn rm    - 64-bit register addition
\   a64.sub      Sub rd rn rm    - 64-bit register subtraction
\   a64.mul      Mul rd rn rm    - 64-bit register multiplication
\   a64.str      Str rt sp off   - store a register into a frame slot
\   a64.ldr      Ldr rt sp off   - load a register back out of a frame slot
\   a64.reserve  Subi sp sp n    - claim the routine's own frame
\   a64.release  Addi sp sp n    - give the frame back
\   a64.ret      Ret             - return to the address in the link register
\ There is no opcode here for a form no pass in the chain produces yet. An opcode
\ with no selection rule and no emission would be a promise, not a schema.
\
\ WHY A COPY IS A FORM WHEN THE MACHINE HAS NO COPY INSTRUCTION. A routine's
\ contract says which register each returned value leaves in, and the value the
\ program computed is not always already there - it can be an argument the caller
\ put somewhere else, or a value whose register was decided by a tie. Putting it
\ where it has to be is one instruction, and it has to be an operation of this
\ dialect for the same reason a spill store is: a register allocator may decide
\ it, but only a module can contain it, and only what a module contains can be
\ checked. ARM64 has no separate move instruction - `mov xd, xm` IS `orr xd, xzr,
\ xm`, which is what a disassembler prints back - so this form is that one form
\ with the zero register in its first source, and src/arch/arm64/asm.f says so
\ once in ENC-MOV rather than at each caller.
\
\ THE FOUR MEMORY FORMS EXIST BECAUSE A SPILL IS AN INSTRUCTION. A straight-line
\ block can hold more values at once than any register file has, so the register
\ allocator has to be able to put a value somewhere that is not a register and
\ read it back. That somewhere is a slot of the routine's own frame, and the
\ allocator can only decide it if the decision names instructions something can
\ emit - which is what a64.str and a64.ldr are. A routine has no frame until it
\ takes one: nothing may be written below the stack pointer on this platform, so
\ a routine that uses a slot must move the stack pointer down over its own frame
\ and put it back before it returns. a64.reserve and a64.release are that pair,
\ and they are in the dialect rather than added silently at emission because a
\ frame the module does not name is a frame nothing can check.
\
\ WHY A FRAME SLOT IS AN ATTRIBUTE AND NOT AN OPERAND. An operand of this IR is
\ an SSA value - something an operation defined - and a slot is not: no operation
\ computes it, it has no type in the value grammar, and two operations naming the
\ same slot are not naming one definition. What a slot is, is a constant field of
\ the instruction, which is exactly what an attribute is, and IR-SCHEMA can
\ validate an attribute: it declares the key, the freeze verifier proves every
\ operation of the form carries exactly one attribute under it, and the value
\ goes through the checked builder below, so no operation with an unreachable
\ slot can be built at all. The base register is not an operand either, for the
\ same reason in reverse: the frame is reached from the stack pointer, the stack
\ pointer is not a value of this dialect and never can be (a value is a register
\ the allocator may hand out, and this one it may not), so the base of a frame
\ access is a property of the form and the two forms say so by name.
\
\ THE MEMORY FORMS ARE NOT PURE, AND SAY SO IN THE ONE PLACE THAT DECIDES. An
\ operation that reads or writes memory has to declare it, and IR-VERIFY makes
\ that declaration structural: an operation whose effect is not pure must carry a
\ memory token, so the order of the memory operations is an SSA chain and not a
\ convention. The four memory forms therefore thread one token - a64.reserve
\ mints it, a64.str and a64.ldr take it and pass it on, a64.release ends it - and
\ MEM-TYPE below is the single place that says what that token is. A value of
\ this dialect is now one of two things, a general register or a memory token,
\ and the register allocator reads the type to tell them apart rather than
\ knowing which opcode produced which.
\
\ THE FRAME BOUNDS ARE A64EFF'S, NOT A SECOND COPY. How far a slot can sit from
\ the stack pointer and how deep a frame can be are facts about the unsigned
\ offset field of the Ldr and Str forms, and src/compiler/a64-effect.f already
\ owns them - it is the schema that describes a routine's frame region, its
\ SLOT-REACH is that field's reach for one access width, and its own suite pins
\ both against src/arch/arm64/asm.f. So the two builders below ask A64EFF rather
\ than restating the arithmetic, and a slot the assembler could not encode is
\ refused before it is interned, exactly as an out-of-field move-wide immediate
\ is.
\
\ WHY MOVK TAKES AN OPERAND WHEN THE INSTRUCTION HAS ONE REGISTER. Movk keeps the
\ bits of rd it does not write, so the register it names is both a source and a
\ destination. In SSA a value is written once, so the value the instruction keeps
\ has to be named: a64.movk reads the value the previous half left and defines
\ the value with this half merged in. That is what makes a materialised 64-bit
\ constant a chain of operations the allocator can read rather than a hidden
\ update of a register nobody declared. The two SSA values are still one register
\ field, and a64.movk's schema says so with a tie, so the register allocator gets
\ the constraint from the form itself rather than from this opcode's name.
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
\ NOTHING HERE MAY TRAP. Add, Sub and Mul on ARM64 wrap; none of the ten forms
\ raises on overflow. A frame access does not either: its slot is proved
\ addressable before the operation can be built. That is why every schema below declares no trap, and it is
\ why the selector refuses a source operation whose own schema says it may trap:
\ a trapping addition needs a flag-setting form and a conditional branch to a
\ trap target, and none of that is in this dialect yet.
\
\ TWO VALUE CLASSES, DELIBERATELY NAMED. A value of this dialect is either a
\ 64-bit general register or the memory token the frame forms thread, and
\ GPR-TYPE and MEM-TYPE are the single places that say which is which. The
\ floating and SIMD register files, labels and fixups are further records of the
\ same dialect and are not here yet; the seam where they arrive is these two
\ readers, which is why they exist instead of each schema interning its own type
\ inline.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f
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
\ over it has to answer for all ten.
ENUM opcode DERIVE eq
   movz
   movk
   mov
   add
   sub
   mul
   store
   load
   reserve
   release
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

\ ---- the frame bounds --------------------------------------------------------
\ A frame access moves one whole general register, which is the widest access the
\ modelled memory forms carry. Everything else about a slot - how far it can sit
\ from the stack pointer, and how deep a frame may be - is A64EFF's, because
\ A64EFF is the schema that describes the frame region and its bounds are already
\ pinned against the shipped assembler.
XBITS 8 / constant SLOT-BYTES        \ bytes one frame access moves

\ The frame itself is claimed and given back by an add/sub-immediate, whose
\ immediate is an unsigned twelve-bit field with no scale. That is a tighter
\ bound than the reach of a slot - a scaled offset field of the same width
\ addresses eight times as far - so the deepest frame this dialect can RESERVE is
\ this field's largest value, rounded down to the stack alignment. It is written
\ here as the field width it is, the same way the move-wide bounds are, and
\ test/compiler/native-a64ir.f pins it against the shipped assembler's IMM12-LIM
\ and against ENC-SUBI's own output.
12 constant OFF-BITS                 \ the add/sub immediate and the offset field
1 OFF-BITS lshift 1- constant OFF-MAX
OFF-MAX dup A64EFF:SP-ALIGN mod - constant FRAME-LIM

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

\ ---- checked frame operands --------------------------------------------------
\ A slot offset the memory forms can reach: inside the frame it is measured from,
\ naturally aligned to the access width - which is what makes the scale division
\ exact - and inside the reach of the scaled offset field. How far that reaches
\ is A64EFF's answer for this width, not a constant repeated here.
: SLOT ( n -- n )
   dup 0 < if E-A64IR-SLOT throw then
   dup SLOT-BYTES mod 0<> if E-A64IR-SLOT throw then
   dup SLOT-BYTES A64EFF:SLOT-REACH > if E-A64IR-SLOT throw then ;

\ A frame a routine could both declare and take: the stack pointer stays aligned,
\ the frame stays inside the region A64EFF can describe at all, and it stays
\ inside the one immediate that claims it.
: FRAME ( n -- n )
   dup 0 < if E-A64IR-FRAME throw then
   dup A64EFF:SP-ALIGN mod 0<> if E-A64IR-FRAME throw then
   dup A64EFF:FRAME-MAX > if E-A64IR-FRAME throw then
   dup FRAME-LIM > if E-A64IR-FRAME throw then ;

public

\ ---- the type of a virtual register ------------------------------------------
\ One 64-bit general-register value. Every operand and every result of this
\ dialect has this type today; a second register class arrives as a second
\ reader beside this one, never as a raw type interned at a use site.
: GPR-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ ---- the type of the memory token --------------------------------------------
\ The order of the frame accesses, as a value the operations pass along. It lives
\ in no register: it is what makes "this load happens after that store" a
\ dependency the module holds rather than a property of the printed order.
: MEM-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-DOMAIN:DATA-MEM IR-BUILD:INTERN-TOKEN ;

\ ---- the bytes one frame access moves ----------------------------------------
\ A consumer that places slots asks for the width rather than assuming it, and
\ takes the reach that goes with it from A64EFF.
: SLOT-WIDTH ( -- n )    SLOT-BYTES ;

\ The deepest frame a routine of this dialect can reserve. A consumer deciding
\ how much frame a program needs asks here rather than assuming the whole region
\ A64EFF can describe is reachable in one instruction.
: FRAME-LIMIT ( -- n )   FRAME-LIM ;

\ ---- the opcode names --------------------------------------------------------
\ This module's interned symbol for one opcode. Interning deduplicates, so asking
\ twice answers the same identity, and this is the symbol both IR-SCHEMA's
\ readers and IR-BUILD:BEGIN-OP take.
: OPCODE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz    OF s" a64.movz"    ENDOF
      movk    OF s" a64.movk"    ENDOF
      mov     OF s" a64.mov"     ENDOF
      add     OF s" a64.add"     ENDOF
      sub     OF s" a64.sub"     ENDOF
      mul     OF s" a64.mul"     ENDOF
      store   OF s" a64.str"     ENDOF
      load    OF s" a64.ldr"     ENDOF
      reserve OF s" a64.reserve" ENDOF
      release OF s" a64.release" ENDOF
      ret     OF s" a64.ret"     ENDOF
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

\ The two attribute keys the frame forms require: which slot a frame access
\ names, and how deep a frame the routine reserves.
: KEY-SLOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.slot" IR-BUILD:INTERN-SYMBOL ;

: KEY-FRAME ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.frame" IR-BUILD:INTERN-SYMBOL ;

: SLOT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   SLOT IR-BUILD:INTERN-INT-ATTR ;

: FRAME-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   FRAME IR-BUILD:INTERN-INT-ATTR ;

private

\ ---- the schema definitions --------------------------------------------------
\ Design lines 242 and 243 require a semantic-rule identifier and a renderer
\ identifier per schema, so a later pass dispatches on an identity rather than on
\ a string comparison. Each opcode names its own, derived from its own spelling.
\ Neither is public: the schema table is the authority on what an opcode's rule
\ and renderer are, and IR-SCHEMA:RULE@ and RENDERER@ answer it.
: RULE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz    OF s" a64.rule.movz"    ENDOF
      movk    OF s" a64.rule.movk"    ENDOF
      mov     OF s" a64.rule.mov"     ENDOF
      add     OF s" a64.rule.add"     ENDOF
      sub     OF s" a64.rule.sub"     ENDOF
      mul     OF s" a64.rule.mul"     ENDOF
      store   OF s" a64.rule.str"     ENDOF
      load    OF s" a64.rule.ldr"     ENDOF
      reserve OF s" a64.rule.reserve" ENDOF
      release OF s" a64.rule.release" ENDOF
      ret     OF s" a64.rule.ret"     ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

: RENDERER ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz    OF s" a64.render.movz"    ENDOF
      movk    OF s" a64.render.movk"    ENDOF
      mov     OF s" a64.render.mov"     ENDOF
      add     OF s" a64.render.add"     ENDOF
      sub     OF s" a64.render.sub"     ENDOF
      mul     OF s" a64.render.mul"     ENDOF
      store   OF s" a64.render.str"     ENDOF
      load    OF s" a64.render.ldr"     ENDOF
      reserve OF s" a64.render.reserve" ENDOF
      release OF s" a64.render.release" ENDOF
      ret     OF s" a64.render.ret"     ENDOF
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
\ this half merged in is the result. The instruction names one register field for
\ both, so the schema declares result 0 tied to operand 0 and every consumer that
\ has to put them in one physical register reads that instead of knowing which
\ opcode the overwrite is.
: DEF-MOVK ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOVK OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 0 IR-SCHEMA:ADD-TIE
   c b MOVE-ATTRS
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOVK NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Mov: one register read, one written, and no tie - the whole point of this form
\ is that the two are DIFFERENT registers, which is what makes it able to put a
\ value where a routine's contract says it has to leave. A copy whose source and
\ destination came out the same register would be an instruction that does
\ nothing, and nothing in the chain builds one: the pass that decides a move
\ decides it only for a value that is not already where it has to be.
: DEF-MOV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOV OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOV NAMED
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

\ ---- the frame forms ---------------------------------------------------------
\ Design lines 238 and 239: an operation that touches memory declares the domain,
\ the address space and the alias behaviour, and carries the token that orders it
\ against the others. The space is the routine's own stack frame, which no other
\ operation of this dialect can reach, so nothing in a module aliases it.
: FRAME-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:LOCAL IR--SCHEMA-ALIAS:UNALIASED e IR-SCHEMA:SET-MEMORY ;

\ Str: the register whose value is being put away, and the token that orders this
\ store against every other frame access. The slot it goes into is the
\ instruction's own field, so it rides as the attribute.
: DEF-STR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:STORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:STORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Ldr: the value read out of the slot, and the token passed on. Result zero is
\ the register, so a consumer that wants the loaded value asks for the first
\ result the way it does of every other value-producing form.
: DEF-LDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:LOAD OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:LOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Reserve: the routine takes its frame, and the token every later frame access
\ carries starts here. It reads no token because there is nothing before it.
: DEF-RESERVE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RESERVE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   c b KEY-FRAME IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:RESERVE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Release: the frame goes back and the token ends. It defines no value, so every
\ frame access of the block is ordered before it and none can follow it.
: DEF-RELEASE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RELEASE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-FRAME IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:RELEASE NAMED
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
   c b MEM-TYPE {: k:IR-ID:ir-type-id :}
   c b t DEF-MOVZ
   c b t DEF-MOVK
   c b t DEF-MOV
   c b t A64IR-OPCODE:ADD DEF-BINARY
   c b t A64IR-OPCODE:SUB DEF-BINARY
   c b t A64IR-OPCODE:MUL DEF-BINARY
   c b t k DEF-STR
   c b t k DEF-LDR
   c b k DEF-RESERVE
   c b k DEF-RELEASE
   c b t DEF-RET ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
