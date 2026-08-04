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
\ one form of the instruction vocabulary src/arch/arm64/asm.f encodes, and it
\ carries only operands that form has. All but one of them is also a form of the
\ 48-form vocabulary formal/Common/Insn.v models; the exception is `a64.mvn`,
\ whose Orn base was added to the assembler with this dialect's complement and is
\ not in that model yet - dot habu-model-orn-in-39435de5 is the row, and
\ until it lands the encoder is held by test/compiler/native-a64ir.f's own
\ assertion against the shipped assembler instead:
\   a64.movz     Movz rd imm hw  - write a 16-bit half into a cleared register
\   a64.movk     Movk rd imm sh  - overwrite one 16-bit half, keeping the rest
\   a64.mov      Orr rd xzr rm   - copy one register into another
\   a64.add      Add rd rn rm    - 64-bit register addition
\   a64.sub      Sub rd rn rm    - 64-bit register subtraction
\   a64.mul      Mul rd rn rm    - 64-bit register multiplication
\   a64.sdiv     Cbnz rm +2; Brk; Sdiv rd rn rm
\                                - 64-bit signed division, trapping on a zero
\                                  divisor exactly as the engine's own `/` does
\   a64.and      And rd rn rm    - 64-bit register bitwise and
\   a64.orr      Orr rd rn rm    - 64-bit register bitwise or
\   a64.eor      Eor rd rn rm    - 64-bit register bitwise exclusive or
\   a64.lslv     Lslv rd rn rm   - shift left by a register's low six bits
\   a64.lsrv     Lsrv rd rn rm   - logical shift right by the same
\   a64.mvn      Orn rd xzr rm   - the bitwise complement of one register
\   a64.str      Str rt sp off   - store a register into a frame slot
\   a64.ldr      Ldr rt sp off   - load a register back out of a frame slot
\   a64.astr     Str rt rn 0     - store a register through an address register
\   a64.aldr     Ldr rt rn 0     - load a register through an address register
\   a64.astrb    Strb rt rn 0    - store a register's lowest byte through an
\                                  address register
\   a64.aldrb    Ldrb rt rn 0    - load one byte through an address register,
\                                  zero-extended into the register
\   a64.reserve  Subi sp sp n    - claim the routine's own frame
\   a64.release  Addi sp sp n    - give the frame back
\   a64.dtake    Subi ds ds n    - take the caller's operands off the data stack
\   a64.dload    Ldr rt ds off   - read one of them out of its slot
\   a64.dstore   Str rt ds off   - write a result into its slot
\   a64.dpublish Addi ds ds n    - make the results the caller's
\   a64.flag     Cmp rn rm; Cset rd cc; Sub rd xzr rd
\                                - leave the Habu flag of one comparison
\   a64.selz     Cmp rn #0; Csel rd rn rm ne
\                                - choose between two registers on whether a
\                                  third is not zero
\   a64.cmpsel   Cmp rn rm; Csel rd rn rm cc
\                                - compare two registers and choose between two
\                                  others under the condition
\   a64.b        B target        - go to one block, handing it its arguments
\   a64.cbz      Cbz rt target; B other
\                                - go to the first block when rt is zero and to
\                                  the second when it is not
\   a64.cmpbr    Cmp rn rm; B.cc target; B other
\                                - compare two registers and go to the first
\                                  block when the condition holds and to the
\                                  second when it does not, without ever
\                                  materialising the flag as a number
\   a64.fflag    Fcmp dn dm; Cset rd cc; Sub rd xzr rd
\                                - leave the Habu flag of one float comparison
\   a64.fflagz   Fcmp dn #0.0; Cset rd cc; Sub rd xzr rd
\                                - the same against the immediate zero, which is
\                                  a form of the instruction and not an operand
\   a64.fcmpbr   Fcmp dn dm; B.cc target; B other
\                                - the fused float compare-and-branch
\   a64.fcmpbrz  Fcmp dn #0.0; B.cc target; B other
\                                - the same against the immediate zero
\   a64.selzd    Cmp rn #0; Fcsel dd dn dm ne
\                                - a64.selz choosing between two DOUBLES
\   a64.cmpseld  Cmp rn rm; Fcsel dd dn dm cc
\                                - a64.cmpsel choosing between two DOUBLES
\   a64.fcmpsel  Fcmp dn dm; Csel rd rn rm cc
\                                - compare two DOUBLES and choose between two
\                                  general registers under the condition
\   a64.fcmpselz Fcmp dn #0.0; Csel rd rn rm cc
\                                - the same against the immediate zero
\   a64.fcmpseld Fcmp dn dm; Fcsel dd dn dm cc
\                                - compare two DOUBLES and choose between two
\                                  more under the condition
\   a64.fcmpselzd Fcmp dn #0.0; Fcsel dd dn dm cc
\                                - the same against the immediate zero
\   a64.call     Addi ds ds n; Bl entry; Subi ds ds m
\                                - hand the caller's data stack to the word being
\                                  compiled, call it, and take the stack back
\   a64.wordcall Addi ds ds n; Bl entry; Subi ds ds m
\                                - the same three instructions to ANOTHER word,
\                                  whose entry address the operation carries
\   a64.lnkstr   Str x30 sp off  - put the caller's return address in a frame slot
\   a64.lnkldr   Ldr x30 sp off  - take it back out again
\   a64.ret      Ret             - return to the address in the link register
\ There is no opcode here for a form no pass in the chain produces yet. An opcode
\ with no selection rule and no emission would be a promise, not a schema.
\
\ THE FOUR FLOAT COMPARISON FORMS ARE FOUR AND NOT TWO, and both splits are
\ instruction forms rather than tastes. The flag-materialising pair and the fused
\ pair differ in exactly what the integer pair differ in - one writes a number
\ into a register and one reads the flags with a branch - and the two-operand and
\ zero-operand halves of each pair differ because FCMP really does have two
\ forms: one comparing two D registers and one comparing a D register against the
\ immediate zero. The engine reaches both (src/habu/habu1.f (FCMP) and (FCMP0)),
\ so modelling the zero comparison as a comparison against a materialised 0.0
\ would emit a longer sequence than the interpreted word for the same source
\ word, and an operation whose operand list does not match its instruction's.
\
\ THE FORMS THAT ARE MORE THAN ONE INSTRUCTION, AND WHY EACH ONE IS. Every other
\ form above is one instruction, and that is the rule this dialect keeps wherever
\ it can: one operation, one form, four bytes. The division breaks it because its
\ zero-divisor guard and the divide it guards are inseparable - the whole point
\ of the guard is that nothing runs between the test and the divide. The two call
\ forms break it because between the first of their three instructions and the
\ last, the data-stack pointer stands above values that are the CALLEE's, so an
\ access placed in the middle would read the callee's stack through the caller's
\ offsets. Every other one breaks it for a single related reason, which is the
\ condition flags: the three comparisons that materialise a flag, the two-way
\ branch, the three compare-and-branches and the eight conditional selects. The
\ flags are a single architectural register that no value of this dialect stands
\ for and the allocator may never hand out, so the instructions that write them
\ and the instruction that reads them have to be inseparable - and the only way
\ an IR can say "inseparable" is to make them one operation. What a form's count
\ is, is written down once and read by the layout: how many instructions the
\ operations of a block are is never guessed at from anything but the opcodes in
\ it.
\
\ THE COUNT IS A CEILING FOR SEVEN FORMS AND AN EXACT NUMBER FOR EVERYTHING ELSE.
\ The one-way branch, the two-way branch and the three compare-and-branches each
\ end in an unconditional branch, and src/compiler/native/emit.f does not emit
\ that branch when the block it names is the block laid out next - the machine
\ gets there by falling into it. So those five are one instruction shorter
\ wherever the layout allows, which makes their count a property of the
\ operation's POSITION as well as of its form. The other two are the copies:
\ a64.mov and a64.fmovdd whose source and destination registers are the same move
\ nothing and are not emitted either, which makes their count a property of the
\ REGISTER ASSIGNMENT. All of that is the
\ emitter's own arithmetic and it is stated there: each rule is one word both its
\ layout pass and its writing pass ask, and the two are held against each other
\ at every block boundary. Nothing outside the emitter counts instructions.
\
\ THE COMPARE-AND-BRANCH IS WHY THE FLAGS ARGUMENT MATTERS RATHER THAN BEING A
\ CURIOSITY. A source comparison that only ever answers a branch test does not
\ need the number a Habu flag is: the machine already has the answer in its
\ condition flags one instruction after the compare, and a branch can read it
\ there. Written as a64.flag followed by a64.cbz that is five instructions and a
\ register - compare, set, negate, test-and-branch, branch - where a64.cmpbr is
\ three and none. It has to be ONE operation for exactly the reason a64.flag is
\ one: the compare writes the flags and the conditional branch reads them, and an
\ IR in which they were two operations would let a later pass put something
\ between them. src/compiler/native/select.f is the pass that chooses it, and it
\ only does so when the source comparison's single use is that branch.
\
\ THE FOUR DATA-STACK FORMS ARE THE OTHER CALLING CONVENTION, MADE OF
\ INSTRUCTIONS. Design section 7.6 gives an externally callable Habu word its
\ inputs and outputs in canonical slots of the CALLER's data stack rather than in
\ registers, and the running engine keeps the pointer to that stack in one
\ register it never gives away (A64EFF:DSTACK-GPR). These four forms are how a
\ routine reaches it, and they are the exact mirror of the frame four: the
\ pointer is named by the form rather than taken as an operand, for the same
\ reason and with the same consequence - no value of this dialect stands for it
\ and none can, because a value is a register the allocator may hand out and this
\ one it may never. The stack is full-ascending and the pointer sits just past
\ the caller's top, so a routine taking `a` arguments moves it down over them
\ once (a64.dtake), reads argument i at 8i, writes result j at 8j, and moves it
\ up over `r` results once (a64.dpublish). That is the same net effect a word the
\ engine compiled itself has, arrived at with two instructions instead of one
\ push or pop per value.
\
\ WHY THEY THREAD A CHAIN OF THEIR OWN. The token type is the one MEM-TYPE below,
\ because there is one kind of ordering value in this dialect and a second would
\ be a second class the allocator has to learn. The CHAIN is separate: a64.dtake
\ mints one and a64.dpublish ends it, exactly as a64.reserve and a64.release do
\ for the frame. Forcing the two into one chain would declare that a frame access
\ and a data-stack access have to keep their order against each other, and they
\ do not - the frame is below the machine stack pointer and the data stack is a
\ region of the engine's own that no frame access can reach - so the schemas say
\ which space each form touches and the orderings stay two.
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
\ ONE FORM MAY TRAP, AND IT IS THE ONE THAT HAS TO. Add, Sub and Mul on ARM64
\ wrap; none of them raises on overflow, and a frame access does not either -
\ its slot is proved addressable before the operation can be built. Division is
\ the exception, and deliberately: the engine's own `/` branches over a `brk`
\ when the divisor is not zero (src/habu/habu1.f BDIV0?), so a divide by zero
\ ends the process rather than answering the zero a bare Sdiv would. a64.sdiv is
\ those three instructions as ONE operation, for the same reason the comparison
\ is three - the guard and the divide are inseparable, and an IR in which they
\ were two operations would let a later pass put something between them - and
\ its schema declares that it may trap. That declaration is what lets the
\ selector tell a source operation it can lower faithfully from one it cannot: a
\ trapping ADDITION still needs a flag-setting form and a conditional branch to
\ a trap target, and none of that is in this dialect yet, so the selector still
\ refuses it.
\
\ THE TWO ADDRESSED FORMS ARE THE ONLY ONES WHOSE BASE IS A VALUE. Every other
\ memory form above reaches a region the FORM names - the frame from the stack
\ pointer, the caller's stack from the engine's own pointer - and neither base is
\ a value of this dialect, because neither register is one the allocator may hand
\ out. a64.aldr and a64.astr are the opposite: their base is an ordinary virtual
\ register, defined by whatever computed the address, so they are how a program
\ reaches a cell it named itself. Both use the same Ldr and Str forms as the
\ frame accesses with an offset of zero, which is what `[Xn]` is on this machine,
\ so no new encoding enters the assembler for them.
\
\ AND THEY SHARE THE DATA STACK'S ORDER, WHICH IS NOT A CONVENIENCE. An address
\ a program computed may name any cell the program can reach, and the caller's
\ data stack is such a region: a routine that stores through a computed address
\ can, in principle, be storing into the very slot a later a64.dstore publishes
\ into. So the two families are in ONE address space and ONE token chain -
\ a64.dtake mints it, every access threads it, a64.dpublish ends it - and the
\ orderings a module states are then true rather than convenient. Splitting them
\ would declare that a computed store and a data-stack store need not keep their
\ order, and nothing proves that. The frame is the case where something does
\ prove it: a frame slot is below the machine stack pointer, no operation of this
\ dialect produces the stack pointer as a value, and no source word can therefore
\ compute an address inside it - which is why the frame keeps a chain and a space
\ of its own, and why the data-stack forms' aliasing is now unrestricted while
\ the frame's stays unaliased.
\
\ THREE VALUE CLASSES, DELIBERATELY NAMED. A value of this dialect is a 64-bit
\ general register, a 64-bit floating register, or the memory token the frame
\ forms thread, and GPR-TYPE, FPR-TYPE and MEM-TYPE are the single places that
\ say which is which. The SIMD register file, labels and fixups are further
\ records of the same dialect and are not here yet; the seam where they arrive is
\ these three readers, which is why they exist instead of each schema interning
\ its own type inline.

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
\ over it has to answer for every member.
ENUM opcode DERIVE eq
   movz
   movk
   mov
   add
   sub
   mul
   sdiv
   and
   orr
   eor
   lslv
   lsrv
   mvn
   store
   load
   reserve
   release
   dtake
   dload
   dstore
   dpublish
   aload
   astore
   abload
   abstore
   flag
   selz
   cmpsel
   br
   brz
   cmpbr
   call
   wordcall
   linksave
   linkload
   ret
   fadd
   fsub
   fmul
   fdiv
   fneg
   fabs
   fsqrt
   scvtf
   fcvtzs
   fmovxd
   fmovdx
   fmovdd
   fflag
   fflagz
   fcmpbr
   fcmpbrz
   selzd
   cmpseld
   fcmpsel
   fcmpselz
   fcmpseld
   fcmpselzd
;ENUM

\ The conditions a comparison may be made under: one per relation the SOURCE
\ dialect has a comparison for, and no others. Six, because six are what Habu's
\ comparison words compare with - `<`, `<=`, `>`, `>=`, `=` and `<>`. The
\ equality member is spelled `equal` and not `eq`, because `eq` is the name the
\ ENUM derives for its own comparison word and a member cannot take it. It is an
\ ENUM so a caller names the condition instead of writing the number the field
\ holds, and so a condition this dialect has no form for is unwritable rather
\ than checked.
\
\ THREE OF THEM ARE THE COMPLEMENTS OF THE OTHER THREE, AND THAT IS NOT A
\ REDUNDANCY. A form that has to BRANCH on the falsity of a relation names the
\ relation and puts its two successors the other way round, so branch polarity
\ still needs no complement and src/compiler/native/select.f never asks for one.
\ What does need them is the form that MATERIALISES a flag: a comparison that
\ answers a number has no successors to swap, so `<>` can only be reached by a
\ condition of its own. `>` and `>=` could have been `<` and `<=` with the
\ operands turned round instead - it is the same relation - but then a lowering
\ would be an operand order in one place and a condition in another, and two
\ tables that have to agree are two tables that can disagree. One condition per
\ source relation keeps it one table, and it also keeps every emitted comparison
\ byte-identical to the sequence the engine's own primitive emits for that word.
\
\ AND THE SEVENTH IS `mi`, WHICH THE FLOAT COMPARISONS NEED AND NO INTEGER
\ COMPARISON DOES. This is the one place in the chain where a condition cannot be
\ read off the relation's NAME, and the reason is the unordered flag.
\
\ A condition is four bits of an instruction, and what those bits MEAN depends on
\ which instruction wrote the flags. After a Subs, `lt` is "signed less than".
\ After an Fcmp it is not: Fcmp raises the unordered condition when either
\ operand is a NaN, and it does so by setting N=0 Z=0 C=1 V=1. Read the six
\ integer conditions against those four bits:
\
\   condition  code  the test        on unordered (N=0 Z=0 C=1 V=1)
\   lt         11    N != V          0 != 1 - TRUE
\   le         13    Z=1 or N != V   TRUE
\   ne          1    Z = 0           TRUE
\   ge         10    N  = V          false
\   gt         12    Z=0 and N = V   false
\   equal       0    Z = 1           false
\   mi          4    N = 1           false
\
\ So `lt` after an Fcmp answers TRUE for a NaN, and the engine's `f<` answers
\ false for one (survey (4) at the head of tools/codegen-compare-corpus3.f,
\ measured). A float less-than lowered under `lt` would therefore take the other
\ arm of a branch than the interpreted word takes, on exactly the input the
\ difference matters for. `mi` is the condition that means less-than after an
\ Fcmp and is false on unordered, which is why the engine's own `f<` uses it, and
\ it is why this member exists rather than the float words borrowing `lt`.
\
\ `gt` and `equal` are NOT borrowed either - they are the same four bits for both
\ kinds of comparison because the architecture says so, and the members are one
\ each because a condition is a machine condition and two names for one field
\ value would be two things that can drift apart. What differs between an integer
\ `>` and a float `f>` is not the condition; it is the instruction that set the
\ flags, and that is the OPCODE's business. The three conditions the float
\ comparisons reach - `mi`, `gt` and `equal` - are exactly the three that are
\ false on unordered, and that is not a coincidence: it is the whole of why every
\ float comparison this engine has answers false for a NaN.
ENUM cond DERIVE eq
   lt
   le
   gt
   ge
   equal
   ne
   mi
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

\ ---- the condition field -----------------------------------------------------
\ A conditional form selects one of sixteen conditions with a four-bit field, and
\ the architecture's own numbering says which number each condition is. Both are
\ written here as the field width and the codes they are, the same way the
\ move-wide bounds are, and test/compiler/native-a64ir.f reads
\ src/arch/arm64/asm.f's own COND-LIM, C-LT and C-LE back and asserts them
\ against these - so a field or a code that moved in the assembler reddens this
\ dialect instead of silently disagreeing with it.
4 constant COND-BITS
1 COND-BITS lshift constant COND-LIM
11 constant COND-LT                  \ signed less than
13 constant COND-LE                  \ signed less than or equal
12 constant COND-GT                  \ signed greater than
10 constant COND-GE                  \ signed greater than or equal
0 constant COND-EQ                   \ equal
1 constant COND-NE                   \ not equal
4 constant COND-MI                   \ negative - less than, after an Fcmp

\ ---- the branch fields -------------------------------------------------------
\ How far each branch form reaches, as the signed word displacement its own
\ field holds. An unconditional branch carries a 26-bit field, a
\ compare-and-branch a 19-bit one and a conditional branch a 19-bit one, all
\ counting instructions rather than bytes. The emitter asks here before it hands
\ a displacement to the encoder, because the encoder masks the field rather than
\ bounding it - a branch out of reach would otherwise become a branch somewhere
\ else. The conditional branch's width is written as its own constant even
\ though it is the same number as the compare-and-branch's: they are two
\ instruction forms with two displacement fields, and one constant standing for
\ both would let a field that moved in one be judged by the other's width.
26 constant B-BITS
19 constant BZ-BITS
19 constant BCOND-BITS

\ Every instruction of this architecture is four bytes, which is why every
\ displacement field above counts instructions rather than bytes. It is written
\ here as the machine fact it is, beside the fields that are measured in it, and
\ it is what says whether an ADDRESS of code can be the address of an
\ instruction at all.
4 constant INSN-BYTES

\ ---- the dialect's own symbols -----------------------------------------------
\ Every symbol this dialect mints is spelled `a64.`-something, so a dialect
\ symbol and any other name in the module's one interner can never collide.

: TARGET ( -- )
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET ;

\ The forms that need a floating unit declare one. A machine without it cannot
\ hold these schemas at all, which is the refusal a target contract exists for.
: FP-TARGET ( -- )
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH
   IR-SCHEMA:SET-TARGET ;

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

\ Version 0.5: the integer subset, the scalar floating forms, the four float
\ comparison forms, and the four conditional selects - two that answer a cell
\ and two that answer a double. The major version stays at zero until the
\ dialect is the whole machine; the minor version moves whenever the schema
\ table gains a form, because a table with these forms in it and one without are
\ two different tables and every consumer compares the version exactly.
0 constant MAJOR
5 constant MINOR

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

\ ---- checked data-stack operands ---------------------------------------------
\ A slot of the caller's data stack, measured the same way a frame slot is - the
\ two forms that reach it are the same Ldr and Str - so the reach is A64EFF's
\ answer for this width and not a constant repeated here. The stack is a stack of
\ whole cells, so an offset that is not a multiple of one names no slot.
: DSLOT ( n -- n )
   dup 0 < if E-A64IR-DSLOT throw then
   dup SLOT-BYTES mod 0<> if E-A64IR-DSLOT throw then
   dup SLOT-BYTES A64EFF:SLOT-REACH > if E-A64IR-DSLOT throw then ;

\ How far the data-stack pointer moves at entry or at exit. It is a whole number
\ of cells - a routine takes and publishes values, not bytes - and it goes into
\ the same unsigned twelve-bit add/sub immediate the frame is claimed with. There
\ is no stack-alignment rule: the data stack is cell-aligned, not sixteen.
: DBYTES ( n -- n )
   dup 0 < if E-A64IR-DBYTES throw then
   dup SLOT-BYTES mod 0<> if E-A64IR-DBYTES throw then
   dup OFF-MAX > if E-A64IR-DBYTES throw then ;

\ A callee entry address a Bl could name: the address of a whole instruction, and
\ not the null address, where no code lives. How far away it is, is not asked
\ here - the distance depends on where the CALLING routine is written, which
\ nothing before emission knows - so the reach stays the emitter's.
: ENTRY ( n -- n )
   dup 0 <= if E-A64IR-ENTRY throw then
   dup INSN-BYTES mod 0<> if E-A64IR-ENTRY throw then ;

\ ---- the checked condition operand -------------------------------------------
\ A condition the four-bit field can hold. It is private because a caller reaches
\ the field through the attribute builder below, which takes a condition of this
\ dialect's own vocabulary, so there is no route to a condition that skipped its
\ bound - and the bound is still made, because the vocabulary and the field are
\ two facts and this is where they are held against each other.
: COND ( n -- n )
   dup 0 < over COND-LIM >= or if E-A64IR-COND throw then ;

\ ---- the checked branch displacement -----------------------------------------
\ A signed value that fits a field of `bits` bits, counted in instructions. Both
\ branch forms ask this and neither carries its own arithmetic.
: FITS? ( n n -- bool )
   {: d:n bits:n :}
   1 bits 1- lshift {: half:n :}
   d half negate >= d half < and ;

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

\ The third value class: a 64-bit floating register. It is the seam the header
\ above names, arriving exactly as it said it would - a reader beside the other
\ two, never a type interned at a use site - and everything downstream reads the
\ class off the value's type against these three identities. The machine really
\ does have two register files, so this is not a label on a cell: an instruction
\ that names a D register cannot name an X register in the same field, and a
\ value in the wrong file is a wrong program rather than a wrong number.
: FPR-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-FMT:DOUBLE IR-BUILD:INTERN-FLT ;

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
      sdiv    OF s" a64.sdiv"    ENDOF
      and     OF s" a64.and"     ENDOF
      orr     OF s" a64.orr"     ENDOF
      eor     OF s" a64.eor"     ENDOF
      lslv    OF s" a64.lslv"    ENDOF
      lsrv    OF s" a64.lsrv"    ENDOF
      mvn     OF s" a64.mvn"     ENDOF
      store    OF s" a64.str"      ENDOF
      load     OF s" a64.ldr"      ENDOF
      reserve  OF s" a64.reserve"  ENDOF
      release  OF s" a64.release"  ENDOF
      dtake    OF s" a64.dtake"    ENDOF
      dload    OF s" a64.dload"    ENDOF
      dstore   OF s" a64.dstore"   ENDOF
      dpublish OF s" a64.dpublish" ENDOF
      aload    OF s" a64.aldr"     ENDOF
      astore   OF s" a64.astr"     ENDOF
      abload   OF s" a64.aldrb"    ENDOF
      abstore  OF s" a64.astrb"    ENDOF
      flag     OF s" a64.flag"     ENDOF
      selz     OF s" a64.selz"     ENDOF
      cmpsel   OF s" a64.cmpsel"   ENDOF
      br       OF s" a64.b"        ENDOF
      brz      OF s" a64.cbz"      ENDOF
      cmpbr    OF s" a64.cmpbr"    ENDOF
      call     OF s" a64.call"     ENDOF
      wordcall OF s" a64.wordcall" ENDOF
      linksave OF s" a64.lnkstr"   ENDOF
      linkload OF s" a64.lnkldr"   ENDOF
      ret      OF s" a64.ret"      ENDOF
      fadd     OF s" a64.fadd" ENDOF
      fsub     OF s" a64.fsub" ENDOF
      fmul     OF s" a64.fmul" ENDOF
      fdiv     OF s" a64.fdiv" ENDOF
      fneg     OF s" a64.fneg" ENDOF
      fabs     OF s" a64.fabs" ENDOF
      fsqrt    OF s" a64.fsqrt" ENDOF
      scvtf    OF s" a64.scvtf" ENDOF
      fcvtzs   OF s" a64.fcvtzs" ENDOF
      fmovxd   OF s" a64.fmovxd" ENDOF
      fmovdx   OF s" a64.fmovdx" ENDOF
      fmovdd   OF s" a64.fmovdd" ENDOF
      fflag    OF s" a64.fflag" ENDOF
      fflagz   OF s" a64.fflagz" ENDOF
      fcmpbr   OF s" a64.fcmpbr" ENDOF
      fcmpbrz  OF s" a64.fcmpbrz" ENDOF
      selzd    OF s" a64.selzd" ENDOF
      cmpseld  OF s" a64.cmpseld" ENDOF
      fcmpsel   OF s" a64.fcmpsel" ENDOF
      fcmpselz  OF s" a64.fcmpselz" ENDOF
      fcmpseld  OF s" a64.fcmpseld" ENDOF
      fcmpselzd OF s" a64.fcmpselzd" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

\ ---- the condition a comparison is made under --------------------------------
\ The number the four-bit field holds for one condition of this dialect's
\ vocabulary. It is public because the emitter hands it to the encoder as the
\ condition operand, and because the suite pins each one against the assembler's
\ own name for it.
: COND-CODE ( A64IR:cond -- n )
   MATCH cond
      lt    OF COND-LT ENDOF
      le    OF COND-LE ENDOF
      gt    OF COND-GT ENDOF
      ge    OF COND-GE ENDOF
      equal OF COND-EQ ENDOF
      ne    OF COND-NE ENDOF
      mi    OF COND-MI ENDOF
   ;MATCH ;

\ The condition one stored code names. It is an exact case, so a code outside
\ this dialect's vocabulary is refused at first touch instead of decoding as some
\ other condition. A pass that reads a comparison back off a module and builds it
\ into another one goes through here.
: N>COND ( n -- A64IR:cond )
   case
      COND-LT of A64IR-COND:LT endof
      COND-LE of A64IR-COND:LE endof
      COND-GT of A64IR-COND:GT endof
      COND-GE of A64IR-COND:GE endof
      COND-EQ of A64IR-COND:EQUAL endof
      COND-NE of A64IR-COND:NE endof
      COND-MI of A64IR-COND:MI endof
      E-A64IR-COND throw
   endcase ;

\ ---- the reach of each branch form -------------------------------------------
\ Whether a word displacement fits the field the form encodes it in. The emitter
\ asks before it encodes, because the encoders mask their displacement fields
\ rather than bounding them.
: B-FITS? ( n -- bool )      B-BITS FITS? ;
: BZ-FITS? ( n -- bool )     BZ-BITS FITS? ;
: BCOND-FITS? ( n -- bool )  BCOND-BITS FITS? ;

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

\ The two attribute keys the data-stack forms require. They are their own keys
\ rather than the frame's, because a consumer that walks a module has to be able
\ to say which region an access is in without asking which opcode it is - and a
\ frame slot and a data-stack slot are counted from different pointers, so one
\ key answering both would let a frame check judge a data-stack access.
: KEY-DSLOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.dslot" IR-BUILD:INTERN-SYMBOL ;

: KEY-DBYTES ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.dbytes" IR-BUILD:INTERN-SYMBOL ;

\ The second data-stack adjustment, which only the call form has: a call moves
\ the pointer up over what it hands the callee and back down over what it takes
\ from it, and the two counts differ whenever the callee leaves a different
\ number of values than it takes. It is its own key rather than a second
\ attribute under KEY-DBYTES because a key answers "which pointer, in which
\ direction" for every reader that walks a module without asking which opcode it
\ is looking at, and an operation carrying one key twice would be two answers to
\ one question.
: KEY-DBACK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.dback" IR-BUILD:INTERN-SYMBOL ;

\ The attribute key the call-another-word form requires: the address its branch
\ goes to. It is an attribute and not an operand for the reason a frame slot is:
\ no operation of this dialect computes it, it stands for no register, and two
\ calls naming one address are not naming one definition. It is not the
\ displacement either - a displacement depends on where the CALLING routine is
\ written, which nothing before emission knows - so what the module carries is
\ the callee's own address and the subtraction is the emitter's.
: KEY-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.entry" IR-BUILD:INTERN-SYMBOL ;

\ The attribute key the comparison form requires: which condition it sets its
\ flag on. The condition is the whole content of a comparison, so a comparison
\ without it means nothing, and IR-OP refuses one that omits it.
: KEY-COND ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.cond" IR-BUILD:INTERN-SYMBOL ;

\ The value it carries. It takes a condition of this dialect's vocabulary rather
\ than a number, so a condition no comparison of this dialect makes cannot be
\ spelled at all, and the number it becomes is still held against the field.
: COND-ATTR ( IR-CTX:ctx IR-BUILD:builder A64IR:cond -- IR-ID:ir-attr-id )
   COND-CODE COND IR-BUILD:INTERN-INT-ATTR ;

: DSLOT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DSLOT IR-BUILD:INTERN-INT-ATTR ;

: DBYTES-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DBYTES IR-BUILD:INTERN-INT-ATTR ;

\ The take-back count is the same field in the same instruction form, so it is
\ held against the same bound.
: DBACK-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DBYTES IR-BUILD:INTERN-INT-ATTR ;

\ The address a call-another-word form branches to, held against what an address
\ of CODE on this machine can be: every instruction is four bytes and every
\ instruction is at a multiple of four, so an entry that is not is the address of
\ no instruction and no Bl can be built to it. How FAR it is, is not a question
\ this dialect can answer - the distance depends on where the calling routine
\ lands - so the reach is checked by the emitter, which is the one pass that
\ knows both ends.
: ENTRY-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   ENTRY IR-BUILD:INTERN-INT-ATTR ;

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
      sdiv    OF s" a64.rule.sdiv"    ENDOF
      and     OF s" a64.rule.and"     ENDOF
      orr     OF s" a64.rule.orr"     ENDOF
      eor     OF s" a64.rule.eor"     ENDOF
      lslv    OF s" a64.rule.lslv"    ENDOF
      lsrv    OF s" a64.rule.lsrv"    ENDOF
      mvn     OF s" a64.rule.mvn"     ENDOF
      store    OF s" a64.rule.str"      ENDOF
      load     OF s" a64.rule.ldr"      ENDOF
      reserve  OF s" a64.rule.reserve"  ENDOF
      release  OF s" a64.rule.release"  ENDOF
      dtake    OF s" a64.rule.dtake"    ENDOF
      dload    OF s" a64.rule.dload"    ENDOF
      dstore   OF s" a64.rule.dstore"   ENDOF
      dpublish OF s" a64.rule.dpublish" ENDOF
      aload    OF s" a64.rule.aldr"     ENDOF
      astore   OF s" a64.rule.astr"     ENDOF
      abload   OF s" a64.rule.aldrb"    ENDOF
      abstore  OF s" a64.rule.astrb"    ENDOF
      flag     OF s" a64.rule.flag"     ENDOF
      selz     OF s" a64.rule.selz"     ENDOF
      cmpsel   OF s" a64.rule.cmpsel"   ENDOF
      br       OF s" a64.rule.b"        ENDOF
      brz      OF s" a64.rule.cbz"      ENDOF
      cmpbr    OF s" a64.rule.cmpbr"    ENDOF
      call     OF s" a64.rule.call"     ENDOF
      wordcall OF s" a64.rule.wordcall" ENDOF
      linksave OF s" a64.rule.lnkstr"   ENDOF
      linkload OF s" a64.rule.lnkldr"   ENDOF
      ret      OF s" a64.rule.ret"      ENDOF
      fadd     OF s" a64.rule.fadd" ENDOF
      fsub     OF s" a64.rule.fsub" ENDOF
      fmul     OF s" a64.rule.fmul" ENDOF
      fdiv     OF s" a64.rule.fdiv" ENDOF
      fneg     OF s" a64.rule.fneg" ENDOF
      fabs     OF s" a64.rule.fabs" ENDOF
      fsqrt    OF s" a64.rule.fsqrt" ENDOF
      scvtf    OF s" a64.rule.scvtf" ENDOF
      fcvtzs   OF s" a64.rule.fcvtzs" ENDOF
      fmovxd   OF s" a64.rule.fmovxd" ENDOF
      fmovdx   OF s" a64.rule.fmovdx" ENDOF
      fmovdd   OF s" a64.rule.fmovdd" ENDOF
      fflag    OF s" a64.rule.fflag" ENDOF
      fflagz   OF s" a64.rule.fflagz" ENDOF
      fcmpbr   OF s" a64.rule.fcmpbr" ENDOF
      fcmpbrz  OF s" a64.rule.fcmpbrz" ENDOF
      selzd    OF s" a64.rule.selzd" ENDOF
      cmpseld  OF s" a64.rule.cmpseld" ENDOF
      fcmpsel   OF s" a64.rule.fcmpsel" ENDOF
      fcmpselz  OF s" a64.rule.fcmpselz" ENDOF
      fcmpseld  OF s" a64.rule.fcmpseld" ENDOF
      fcmpselzd OF s" a64.rule.fcmpselzd" ENDOF
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
      sdiv    OF s" a64.render.sdiv"    ENDOF
      and     OF s" a64.render.and"     ENDOF
      orr     OF s" a64.render.orr"     ENDOF
      eor     OF s" a64.render.eor"     ENDOF
      lslv    OF s" a64.render.lslv"    ENDOF
      lsrv    OF s" a64.render.lsrv"    ENDOF
      mvn     OF s" a64.render.mvn"     ENDOF
      store    OF s" a64.render.str"      ENDOF
      load     OF s" a64.render.ldr"      ENDOF
      reserve  OF s" a64.render.reserve"  ENDOF
      release  OF s" a64.render.release"  ENDOF
      dtake    OF s" a64.render.dtake"    ENDOF
      dload    OF s" a64.render.dload"    ENDOF
      dstore   OF s" a64.render.dstore"   ENDOF
      dpublish OF s" a64.render.dpublish" ENDOF
      aload    OF s" a64.render.aldr"     ENDOF
      astore   OF s" a64.render.astr"     ENDOF
      abload   OF s" a64.render.aldrb"    ENDOF
      abstore  OF s" a64.render.astrb"    ENDOF
      flag     OF s" a64.render.flag"     ENDOF
      selz     OF s" a64.render.selz"     ENDOF
      cmpsel   OF s" a64.render.cmpsel"   ENDOF
      br       OF s" a64.render.b"        ENDOF
      brz      OF s" a64.render.cbz"      ENDOF
      cmpbr    OF s" a64.render.cmpbr"    ENDOF
      call     OF s" a64.render.call"     ENDOF
      wordcall OF s" a64.render.wordcall" ENDOF
      linksave OF s" a64.render.lnkstr"   ENDOF
      linkload OF s" a64.render.lnkldr"   ENDOF
      ret      OF s" a64.render.ret"      ENDOF
      fadd     OF s" a64.render.fadd" ENDOF
      fsub     OF s" a64.render.fsub" ENDOF
      fmul     OF s" a64.render.fmul" ENDOF
      fdiv     OF s" a64.render.fdiv" ENDOF
      fneg     OF s" a64.render.fneg" ENDOF
      fabs     OF s" a64.render.fabs" ENDOF
      fsqrt    OF s" a64.render.fsqrt" ENDOF
      scvtf    OF s" a64.render.scvtf" ENDOF
      fcvtzs   OF s" a64.render.fcvtzs" ENDOF
      fmovxd   OF s" a64.render.fmovxd" ENDOF
      fmovdx   OF s" a64.render.fmovdx" ENDOF
      fmovdd   OF s" a64.render.fmovdd" ENDOF
      fflag    OF s" a64.render.fflag" ENDOF
      fflagz   OF s" a64.render.fflagz" ENDOF
      fcmpbr   OF s" a64.render.fcmpbr" ENDOF
      fcmpbrz  OF s" a64.render.fcmpbrz" ENDOF
      selzd    OF s" a64.render.selzd" ENDOF
      cmpseld  OF s" a64.render.cmpseld" ENDOF
      fcmpsel   OF s" a64.render.fcmpsel" ENDOF
      fcmpselz  OF s" a64.render.fcmpselz" ENDOF
      fcmpseld  OF s" a64.render.fcmpseld" ENDOF
      fcmpselzd OF s" a64.render.fcmpselzd" ENDOF
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

\ One register read, one written, and no tie: the two register fields are
\ independent, so the form can put a value somewhere else. The two forms of this
\ dialect with that shape share it.
: DEF-UNARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Mov: the form exists so that the two registers CAN be different, which is what
\ makes it able to put a value where a routine's contract says it has to leave
\ and to hand a block argument the value an edge carries. A copy whose source and
\ destination came out the same register is `orr xd, xzr, xd`, an instruction
\ that does nothing, and one pass does build them:
\ src/compiler/native/select.f splits every argument-carrying edge into one copy
\ per argument, and a copy whose two ends coalesce into one register is exactly
\ that no-op. It is emitted rather than elided because eliding it is a peephole,
\ and the register allocator is what decides whether it is one - not this
\ dialect, and not the emitter.
: DEF-MOV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   A64IR-OPCODE:MOV DEF-UNARY ;

\ Mvn: the bitwise complement, which is `orn xd, xzr, xm` exactly as the copy
\ above is `orr xd, xzr, xm`. It is the whole of what Habu's `invert` compiles
\ to, in one instruction and one register.
: DEF-MVN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   A64IR-OPCODE:MVN DEF-UNARY ;

\ One shifted-register or two-source three-operand form: two registers read, one
\ written. The arithmetic, bitwise and shift opcodes differ only in their names,
\ so they share this shape.
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

\ Signed division: the same two registers in and one out as the three forms
\ above, and the one form of this dialect that may raise. It is three
\ instructions - branch over the trap when the divisor is not zero, the trap,
\ the divide - because that is what the engine's own `/` is, and a routine this
\ chain compiles has to do what the interpreted word does on every input rather
\ than only on the ones a harness pins. The three are one operation for the
\ reason the comparison's three are: the branch and the instruction it guards
\ are inseparable, and nothing may be inserted between them.
: DEF-SDIV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:SDIV OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:SDIV NAMED
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

\ ---- the data-stack forms ----------------------------------------------------
\ The caller's data stack is not the routine's frame: it is a region of the
\ running engine's own memory, reached through the pointer register the engine
\ keeps it in. The space says so, which is how a consumer tells a frame access
\ from a data-stack access without asking which opcode it is looking at. The
\ aliasing is unrestricted because a module of this dialect can now reach that
\ region another way: a64.aldr and a64.astr take their address as a value, and a
\ value can be the address of a data-stack slot. That is the same declaration
\ those two forms make, and the two families share one token chain for exactly
\ this reason.
: DSTACK-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

\ Dtake: the routine moves the data-stack pointer down over the arguments the
\ caller left, and the order of every data-stack access starts here. It reads no
\ token because there is nothing before it.
: DEF-DTAKE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:DTAKE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:DTAKE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Dload: one argument read out of its slot. Result zero is the register, so the
\ value an argument arrives as is asked for exactly like any other value.
: DEF-DLOAD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:DLOAD OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DSLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ DSTACK-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:DLOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Dstore: one result written into its slot, which is how a Habu word publishes.
: DEF-DSTORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:DSTORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DSLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:DSTORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Dpublish: the data-stack pointer moves up over the results, which is the moment
\ they become the caller's. It defines no value, so every data-stack access of
\ the block is ordered before it and none can follow it.
: DEF-DPUBLISH ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:DPUBLISH OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:DPUBLISH NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the two addressed forms -------------------------------------------------
\ The only memory forms of this dialect whose base is a value. They are in the
\ same generic space as the data-stack forms and thread the same chain, because
\ an address the program computed may name a data-stack slot; the aliasing is
\ unrestricted, which is the declaration that forbids a later pass from moving
\ one of these across another access it cannot prove is elsewhere.
: ADDR-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

\ Aldr: the address to read through, and the token that orders this load against
\ every other access. Result zero is the register the cell's contents land in, so
\ a consumer that wants the loaded value asks for the first result the way it
\ does of every other value-producing form. There is no offset attribute: the
\ form encodes at offset zero, which is `[Xn]`, and an offset that is not zero is
\ an addressing mode this dialect does not have rather than a field left at its
\ default.
: DEF-ALDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:ALOAD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:READ ADDR-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:ALOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Astr: the register whose value is being written, the address to write it
\ through, and the token. The value is the FIRST operand and the address the
\ second, which is the order the source dialect's store has them and the order
\ Forth writes them in, so a swapped pair is a wrong program rather than a
\ different spelling of the same one.
: DEF-ASTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:ASTORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:WRITE ADDR-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:ASTORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Aldrb and astrb: the same two addressed accesses one byte wide. Everything
\ about them is the addressed cell forms' - the base is a value, the offset is
\ zero, the space is generic and the aliasing unrestricted, and they thread the
\ one token chain - and the single difference is the number of bytes the
\ instruction moves. That difference is the FORM and not a field: the machine
\ has separate Ldrb and Strb encodings, so a width no encoding exists for cannot
\ be named at all, and every MATCH over this dialect's opcode family has to say
\ what a byte access becomes. The loaded byte arrives zero-extended, because
\ Ldrb writes a W register and writing a W register clears the upper half of the
\ X register - which is what `c@` leaves on a Habu stack; the stored byte is the
\ operand register's lowest, which is what `c!` writes.
: DEF-ALDRB ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:ABLOAD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:READ ADDR-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:ABLOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-ASTRB ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:ABSTORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:WRITE ADDR-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:ABSTORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the comparison form -----------------------------------------------------
\ Flag: two registers compared, and the Habu flag that comparison leaves. It is
\ ONE operation of this dialect and three instructions of the machine - compare,
\ set one on the condition, negate - because the three are inseparable: the
\ condition flags they pass between them are a single architectural resource that
\ no value of this dialect stands for and the register allocator may not hand
\ out, so an IR in which they were three operations would let any later pass put
\ something between them and change what the second one reads. Modelling the
\ flags as a value instead would add a third value class to every pass in the
\ chain to express a lifetime that is always exactly one instruction long. The
\ sequence is the one the engine's own emitter uses for `<` today, so a compiled
\ comparison computes what an interpreted one computes, all bits set or none.
: DEF-FLAG ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FLAG OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:FLAG NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the two conditional-select forms ----------------------------------------
\ A selection whose two answers are already computed does not need a branch at
\ all: the machine's Csel writes one of two registers into a third on a
\ condition, in one instruction and with nothing for a predictor to get wrong.
\ These are the two forms that reach it, and they stand to the two BRANCH forms
\ below exactly as those stand to each other - one tests a register against
\ zero, the other compares two registers under a named condition.
\
\ EACH IS ONE OPERATION AND TWO INSTRUCTIONS, for the reason a64.flag and
\ a64.cmpbr are one operation and three: what passes between the compare and
\ the select is the condition flags, a single architectural resource that no
\ value of this dialect stands for and the register allocator may never hand
\ out. An IR in which the two were two operations would let a later pass - the
\ spill lowering is the one that really inserts instructions - put a load
\ between them, and then the select would read whatever that load left.
\
\ THE POLARITY IS THE SOURCE BRANCH'S, NOT THE MACHINE'S. hir.brz goes to its
\ FIRST successor when the value it tests is zero, so the arm a Habu `if`
\ takes is the SECOND. Both forms below therefore name the
\ condition-holds answer first and the other second, which is the same order
\ a64.cmpbr puts its successors in; src/compiler/native/select.f wires one
\ table for both.
\
\ AND THE CONDITION MEANS WHAT THE INSTRUCTION THAT WROTE THE FLAGS MADE IT
\ MEAN, WHICH IS WHY THE FLAGS-WRITER IS PART OF THE FORM. These two take their
\ flags from a Cmp of two general registers or from a Cmp of one against the
\ immediate zero, so their condition is the integer relation the source
\ comparison names and no NaN can reach them. The four forms whose first
\ instruction is an Fcmp are separate opcodes further down for exactly that
\ reason: after an Fcmp the same four bits mean something else on an unordered
\ comparison, so which instruction wrote the flags cannot be an operand or an
\ attribute - it has to be the opcode.
\
\ TWO OF THE FOUR MOVE A DOUBLE, AND THE ONLY THING THAT MAKES THEM A SECOND
\ PAIR IS THE FILE THE ANSWER LIVES IN. Csel writes a general register and Fcsel
\ writes a D register; the two instructions read the same flags under the same
\ four-bit condition and differ in nothing else, so a64.selzd and a64.cmpseld
\ are a64.selz and a64.cmpsel with the two chosen-between operands and the
\ result moved to the floating file. They cannot be one form each with a
\ file-polymorphic type, because a schema's operand types ARE how every reader
\ downstream learns which file a value belongs to - which is the same reason the
\ four crossing forms above are four and not one.
\
\ HOW THE EIGHT ARE SPELLED, so a form added later has a name rather than an
\ argument. A leading `f` says the values COMPARED are doubles, exactly as it
\ does in a64.fflag and a64.fcmpbr. A trailing `d` says the value CHOSEN is,
\ exactly as it does in a64.fmovxd. That is a square rather than a list: two
\ shapes - a zero test and a two-register compare - times two flags-writers times
\ two answer files, and all eight corners are now filled. This pair is the corner
\ with neither letter; a64.selzd and a64.cmpseld below carry only the trailing
\ `d`; and a64.fcmpsel, a64.fcmpselz, a64.fcmpseld and a64.fcmpselzd, also below,
\ are the four whose flags an Fcmp wrote.

\ Selz: the value tested against zero, the answer when it is NOT zero, the
\ answer when it is. Two instructions: compare the tested register against the
\ immediate zero, then select on `ne`.
: DEF-SELZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:SELZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:SELZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Cmpsel: the two registers compared, the answer when the named relation holds,
\ the answer when it does not. Two instructions: the compare, then the select
\ under the condition the operation carries - which is the same attribute under
\ the same key every other comparing form of this dialect carries.
: DEF-CMPSEL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CMPSEL OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:CMPSEL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the two branch forms ----------------------------------------------------
\ B: control goes to one block, unconditionally. Its operands are the values it
\ hands that block as the block's arguments - design lines 706-708 make a
\ terminator's operands exactly that, and design line 532 makes the verifier
\ match their count and types against the destination - so how many there are is
\ a property of the destination rather than of the form, and the list is one
\ variadic tail.
: DEF-BR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:BR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 1 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:BR NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Cbz: control goes to the first successor when the tested register is zero and
\ to the second when it is not. Its one operand is the register it tests and NOT
\ a block argument, which is why both of its successors must be blocks that take
\ no arguments: with two successors the operation model has no way to say which
\ operand belongs to which destination (src/compiler/ir/verify.f says so where
\ it checks the single-successor case), so a two-way branch of this dialect hands
\ nothing over and an edge that has to carry values goes through a block whose
\ terminator is the unconditional form above. That is ordinary critical-edge
\ splitting, and src/compiler/native/elaborate.f builds every conditional edge
\ that way.
: DEF-BRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:BRZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:BRZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Cmpbr: control goes to the first successor when the two registers stand in the
\ named relation and to the second when they do not. Its two operands are the
\ registers it compares and NOT block arguments, so both of its successors must
\ be blocks that take none - the same rule the two-way branch above keeps, for
\ the same reason: with two successors the operation model has no way to say
\ which operand belongs to which destination.
\
\ THE FIRST SUCCESSOR IS THE CONDITION-HOLDS ONE, AND THAT IS A DECISION THE
\ MACHINE MADE. The emitter lays a conditional branch to the first successor
\ down and an unconditional branch to the second after it, so the first
\ successor is the one reached by a TAKEN conditional and the second is reached
\ by falling into the branch below. A pass fusing a source two-way branch has a
\ free choice of which way round to put them - it can negate the condition and
\ keep the source order, or keep the condition and swap the order - and the two
\ are not equally fast: measured over the eleven-row corpus against
\ byte-identical control rows, putting the CONDITION-TRUE arm first is flat and
\ putting the condition-FALSE arm first costs the loop rows four to six per
\ cent, because it makes the hot path a taken conditional that jumps over the
\ unconditional branch beside it. Keeping the condition-true arm first is also
\ what leaves the unconditional branch pointing at the block laid out next,
\ which is the branch a later elision pass can delete (dot
\ habu-elide-a-branch-74966a02). src/compiler/native/select.f wires the
\ successors accordingly.
\
\ IT DEFINES NO VALUE, WHICH IS THE WHOLE SAVING. The comparison it stands for
\ writes only the condition flags, and the branch beside it reads them there, so
\ nothing is materialised into a register and the register allocator has one
\ fewer live value to place. That is what makes it three instructions and no
\ register where a64.flag followed by a64.cbz is five and one.
\
\ THE CONDITION IS THE WHOLE CONTENT OF THE TEST, so it rides as the attribute
\ under the same key the comparison form uses, and IR-OP refuses an operation
\ that omits it. Which condition a fused source comparison becomes is
\ src/compiler/native/select.f's answer, not this file's: this form only says
\ that the first successor is the one taken when the condition holds.
: DEF-CMPBR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CMPBR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:CMPBR NAMED
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

\ ---- the call, and the link register it costs --------------------------------
\ Call: the routine hands the caller's data stack to the word being compiled and
\ takes it back. THREE instructions and one operation - move the data-stack
\ pointer up over everything the callee is being handed, branch with link to the
\ routine's own entry, move it back down over everything the callee left - for
\ the reason the comparison and the division are one operation each: between the
\ first and the last of the three the machine is in a state no other operation of
\ this dialect is written for. The pointer stands above values that are the
\ CALLEE's, so an access placed in the middle would be reading or writing the
\ callee's stack through the caller's offsets, and an IR in which the three were
\ three operations would let any later pass put one there.
\
\ IT MOVES NO VALUE, AND THAT IS THE WHOLE POINT. The values crossing a call are
\ moved by ordinary a64.dstore and a64.dload operations around it - the same two
\ forms a routine's own entry and exit use, at the same slots counted from the
\ same pointer - so the caller's saved values and the callee's arguments are
\ operations a validator can read rather than an effect this form claims. What is
\ left for the form itself is control, the link register, and the two adjustments,
\ and the two adjustments are its attributes.
\
\ THE TARGET IS THE ROUTINE'S OWN ENTRY AND IS NOT AN OPERAND. A self-call's
\ displacement is known where every other branch's is - at layout, as the
\ distance to a block of this function - so it needs no relocation and no symbol.
\ A call to ANOTHER word does need both, and this dialect has neither: it is
\ refused by the selector rather than approximated here.
: DEF-CALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   c b KEY-DBACK IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE DSTACK-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:CALL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Wordcall: the same three instructions to ANOTHER word. Everything a64.call
\ says about the shape it says about this one - one operation because the machine
\ is in a state no other operation of this dialect is written for between the
\ first instruction and the last, the values crossing it moved by ordinary
\ a64.dstore and a64.dload around it, and the two adjustments as its own
\ attributes. What it adds is the third attribute: the address the branch goes
\ to.
\
\ THE TARGET IS AN ADDRESS AND NOT A BLOCK, WHICH IS THE WHOLE DIFFERENCE. A
\ self-call's target is block zero of the function being emitted, so its
\ displacement falls out of the block layout exactly as a branch's does and no
\ address appears in the module at all. This one's target is somewhere else
\ entirely, so the module carries the address and the emitter subtracts the
\ place the calling instruction lands at. That subtraction needs a fact no
\ earlier pass has - where this routine will be written - and the emitter is
\ told it by the seam that decides it.
\
\ IT CARRIES THE SAME TWO ADJUSTMENTS UNDER THE SAME TWO KEYS, deliberately: a
\ consumer that walks a module to find call sites reads the keys and not the
\ opcodes (src/compiler/native/regalloc-verify.f says so in full), so a form that
\ named its adjustments differently would be a call site that consumer could not
\ see, and the caller-save discipline would go unchecked around it.
: DEF-WORDCALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:WORDCALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   c b KEY-DBACK IR-SCHEMA:ADD-ATTR
   c b KEY-ENTRY IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE DSTACK-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:WORDCALL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Saving and restoring the caller's return address. They are the same Str and Ldr
\ the frame forms above are, against the same stack pointer, and they differ in
\ exactly one thing: the register they move is x30, which is named by the FORM.
\ It has to be, for the reason the stack pointer and the data-stack pointer are:
\ an operand of this dialect is a value, a value is a register the allocator may
\ hand out, and the link register is one it may never - src/compiler/a64-effect.f
\ keeps x30 out of every general-register set, which is what makes "the allocator
\ cannot put a value in the link register" a fact about what a contract can be
\ rather than a rule some pass has to remember.
: DEF-LNKSTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:LINKSAVE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:LINKSAVE NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-LNKLDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:LINKLOAD OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:LINKLOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the floating forms ------------------------------------------------------
\ Seven arithmetic forms, one instruction each, exactly as the engine's own
\ float primitives are: FADD, FSUB, FMUL, FDIV, FNEG, FABS and FSQRT over the
\ D-register file. They are the generic value shapes with the floating type
\ passed in, because a two-source three-operand form and a two-operand form are
\ the same statement whichever file they name - which is what makes the file a
\ property of the TYPE rather than of the shape.
\
\ NONE OF THEM MAY TRAP, and that is a fact about IEEE754 rather than about this
\ dialect: dividing by zero answers an infinity, zero by zero and the square root
\ of a negative answer the default NaN, and nothing raises. The source dialect
\ declares the same thing, so the trap rule the selector applies is satisfied by
\ the two agreeing rather than by either being relaxed.

\ The two rounding conversions, each one instruction and each between the two
\ files: SCVTF reads a general register and writes a floating one, rounding to
\ nearest with ties to even; FCVTZS reads a floating register and writes a
\ general one, truncating toward zero and saturating at the ends. The operand
\ and result types are the two register classes, so a lowering that named the
\ wrong direction is a type error in the module rather than a wrong instruction
\ nobody sees.

\ The two moves, which compute nothing and only change which file the same eight
\ bytes are in. FMOV Dd,Xn and FMOV Xd,Dn are the two crossings the source
\ dialect's reinterpretations lower to, and they are the same instructions the
\ engine's own float primitives use to get a data-stack cell into a floating
\ register and back (src/habu/habu1.f, BF+).

\ THERE IS A FLOATING COPY HERE AND STILL NO FLOATING FRAME ACCESS, AND BOTH
\ HALVES ARE SCOPE STATEMENTS. A double is copied when it crosses a block edge -
\ every argument-carrying edge is split in values by
\ src/compiler/native/select.f - and a double crosses one as soon as an `if` has
\ a float literal in an arm or a loop carries an accumulator, so a64.fmovdd
\ below is a form real programs reach and it is defined. A double reaches a FRAME
\ SLOT only when the allocator has to put one away, which the routine contract's
\ whole floating file makes rare and which nothing in the float corpus reaches: a
\ form with no lowering that no program reaches is a promise and not a schema, so
\ the STR and LDR forms of the D file arrive with the leaf that reaches them -
\ dot habu-spill-a-double-bbd5583f - and src/compiler/native/spill.f refuses a
\ double it would have to put away, by name, until then.

\ One floating binary form: two D operands, one D result.
: DEF-FBINARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ One form with one operand and one result, at two types that may be the same
\ one. The floating unary forms and the copy take a D and answer a D; the two
\ conversions and the two crossings take one file and answer the other. One
\ definer serves all of them because the statement is the same statement with
\ different types in it, which is exactly what a type-parameterised schema is for.
: DEF-FCROSS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder ti:IR-ID:ir-type-id to:IR-ID:ir-type-id
      o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   ti IR-SCHEMA:ADD-OPERAND
   to IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the four float comparison forms -----------------------------------------
\ The floating mirror of a64.flag and a64.cmpbr, and the same argument holds them
\ together: an Fcmp writes only the condition flags, and the instruction that
\ reads them - a Cset or a B.cc - has to be inseparable from it, because the
\ flags are a single architectural resource no value of this dialect stands for
\ and the register allocator may never hand out. So each of these is ONE
\ operation and three instructions of the machine.
\
\ WHAT IS NEW HERE AND WHAT IS NOT. The register file is: the values compared are
\ D registers and the flag a materialising form answers is an X register, so the
\ operand types and the result type are two different classes in one operation.
\ The condition is not new: it rides as the same attribute under the same key,
\ and which condition each source word becomes is src/compiler/native/select.f's
\ answer against the table at the head of this file. What these forms say is that
\ the flags were written by an Fcmp - which is exactly the fact that decides what
\ a condition means for a NaN.

\ Fflag: two D registers compared, and the Habu flag that comparison leaves.
: DEF-FFLAG ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FFLAG OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FFLAG NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Fflagz: the same against the immediate zero, which the instruction carries and
\ the operation therefore does not. One operand, because the form has one.
: DEF-FFLAGZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FFLAGZ OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FFLAGZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Fcmpbr: control goes to the first successor when the two D registers stand in
\ the named relation and to the second when they do not - and when either of them
\ is a NaN the relation does NOT hold, whichever of the three conditions is
\ named, so control goes to the SECOND. That is the whole of how this dialect
\ keeps the engine's NaN rule through a fused branch: the rule is not a check
\ anyone wrote, it is the unordered flag and the choice of conditions above.
\
\ The first successor is the condition-holds one, exactly as it is for the
\ integer form and for the same measured reason; the pass that wires them is
\ src/compiler/native/select.f and it says which way round it puts them. It
\ defines no value, which is the whole saving.
: DEF-FCMPBR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPBR OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPBR NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Fcmpbrz: the same fused branch against the immediate zero.
: DEF-FCMPBRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPBRZ OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPBRZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the two selects that answer a double ------------------------------------
\ The conditional-select pair further up, with the two chosen-between operands
\ and the result in the D file and everything else the same. They live here
\ rather than beside their general partners because the floating type is in
\ scope here and because they need the floating target: the machine they lower
\ to is the machine with an Fcsel in it.
\
\ THE TWO FILES MEET IN ONE OPERATION, exactly as they do in a64.fflag, and the
\ operand types are where the meeting is written down. What decides the arm is a
\ GENERAL register - a value tested against zero, or two values compared - and
\ what is chosen between is a pair of doubles. A form that put the tested value
\ in the floating file would be asking the machine to Cmp a D register, which is
\ not an instruction; a form that put the answers in the general file is
\ a64.selz, which already exists.
\
\ AND THE NaN RULE COMES ACROSS UNTOUCHED, which is the whole reason these two
\ need no NaN argument of their own. Neither form compares a double. The flags
\ they select on were written by an integer Cmp over cells - a Habu flag, or two
\ numbers - so a NaN cannot reach them, and where the tested cell IS the flag a
\ float comparison left, that flag is the one a64.fflag or a64.fflagz computed
\ under a condition already chosen to be false when the unordered flag is set.
\ The select therefore picks on exactly the number the source branch tested, so
\ it takes the arm the source branch took, NaN or not.

\ Selzd: the cell tested against zero, the double taken when it is NOT zero, the
\ double taken when it is. Two instructions: compare the tested register against
\ the immediate zero, then Fcsel on `ne`.
: DEF-SELZD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:SELZD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:SELZD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Cmpseld: the two cells compared, the double taken when the named relation
\ holds, the double taken when it does not. Two instructions: the Cmp, then the
\ Fcsel under the condition the operation carries.
: DEF-CMPSELD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CMPSELD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:CMPSELD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the four selects whose flags an Fcmp wrote ------------------------------
\ The last row of the square. The four forms above choose on flags a Cmp over
\ CELLS left; these four choose on flags an Fcmp over DOUBLES left, which is the
\ shape a source `x y f< if a else b then` really has. Written the long way that
\ is an a64.fflag - Fcmp, Cset, Sub - and then a select that tests the number it
\ answered against zero, five instructions and one register; written as one of
\ these it is the Fcmp and the select, two instructions and no register.
\
\ WHY THE FLAGS-WRITER HAS TO BE THE OPCODE AND NOT A FIELD. A condition is four
\ bits of the select instruction, and what those four bits MEAN depends on which
\ instruction wrote the flags: after an Fcmp of a NaN the unordered condition is
\ set - N=0 Z=0 C=1 V=1 - and `lt` reads TRUE there while `mi` reads false, where
\ after a Subs the two are the same relation. So an operation that carried "which
\ instruction wrote the flags" as an attribute would let a later pass change the
\ meaning of the condition beside it without changing the condition. It is the
\ opcode, and the condition table that pairs a source relation with a condition
\ for THIS row of the square is at the head of src/compiler/native/select.f, with
\ the whole derivation written out beside it.
\
\ THE ZERO FORMS CARRY A CONDITION AND a64.selz DOES NOT, which is the one shape
\ difference a reader would not predict from the names. a64.selz tests a Habu
\ FLAG cell - a number the program computed - so its condition is always `ne`,
\ "the flag is not zero", and there is nothing to say. a64.fcmpselz compares a
\ DOUBLE against the immediate zero, and which relation it is asking about -
\ `f0<` or `f0=` - is the condition. Both zero forms therefore take one compared
\ operand and carry the condition attribute.
\
\ EACH IS ONE OPERATION AND TWO INSTRUCTIONS for the reason every other
\ flags-reading form is: the flags pass between the compare and the select and
\ are a single architectural resource no value of this dialect stands for, so a
\ later pass must have nowhere to put anything between them.

\ Fcmpsel: the two doubles compared, the cell taken when the named relation
\ holds, the cell taken when it does not. Two instructions: the Fcmp, then the
\ Csel under the condition the operation carries.
: DEF-FCMPSEL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPSEL OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPSEL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Fcmpselz: the same against the immediate zero, which the instruction carries
\ and the operation therefore does not.
: DEF-FCMPSELZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPSELZ OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPSELZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Fcmpseld: the two doubles compared, the double taken when the named relation
\ holds, the double taken when it does not. Every operand and the result are of
\ the floating file, which is the only corner of the square where that is so.
: DEF-FCMPSELD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPSELD OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPSELD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Fcmpselzd: the same against the immediate zero. This is the shape RELU-F has.
: DEF-FCMPSELZD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPSELZD OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPSELZD NAMED
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
   c b t DEF-SDIV
   c b t A64IR-OPCODE:AND DEF-BINARY
   c b t A64IR-OPCODE:ORR DEF-BINARY
   c b t A64IR-OPCODE:EOR DEF-BINARY
   c b t A64IR-OPCODE:LSLV DEF-BINARY
   c b t A64IR-OPCODE:LSRV DEF-BINARY
   c b t DEF-MVN
   c b t k DEF-STR
   c b t k DEF-LDR
   c b k DEF-RESERVE
   c b k DEF-RELEASE
   c b k DEF-DTAKE
   c b t k DEF-DLOAD
   c b t k DEF-DSTORE
   c b k DEF-DPUBLISH
   c b t k DEF-ALDR
   c b t k DEF-ASTR
   c b t k DEF-ALDRB
   c b t k DEF-ASTRB
   c b t DEF-FLAG
   c b t DEF-SELZ
   c b t DEF-CMPSEL
   c b t DEF-BR
   c b t DEF-BRZ
   c b t DEF-CMPBR
   c b k DEF-CALL
   c b k DEF-WORDCALL
   c b k DEF-LNKSTR
   c b k DEF-LNKLDR
   c b t DEF-RET
   c b FPR-TYPE {: f:IR-ID:ir-type-id :}
   c b f A64IR-OPCODE:FADD DEF-FBINARY
   c b f A64IR-OPCODE:FSUB DEF-FBINARY
   c b f A64IR-OPCODE:FMUL DEF-FBINARY
   c b f A64IR-OPCODE:FDIV DEF-FBINARY
   c b f f A64IR-OPCODE:FNEG DEF-FCROSS
   c b f f A64IR-OPCODE:FABS DEF-FCROSS
   c b f f A64IR-OPCODE:FSQRT DEF-FCROSS
   c b t f A64IR-OPCODE:SCVTF DEF-FCROSS
   c b f t A64IR-OPCODE:FCVTZS DEF-FCROSS
   c b t f A64IR-OPCODE:FMOVXD DEF-FCROSS
   c b f t A64IR-OPCODE:FMOVDX DEF-FCROSS
   c b f f A64IR-OPCODE:FMOVDD DEF-FCROSS
   c b f t DEF-FFLAG
   c b f t DEF-FFLAGZ
   c b f DEF-FCMPBR
   c b f DEF-FCMPBRZ
   c b t f DEF-SELZD
   c b t f DEF-CMPSELD
   c b t f DEF-FCMPSEL
   c b t f DEF-FCMPSELZ
   c b f DEF-FCMPSELD
   c b f DEF-FCMPSELZD ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
