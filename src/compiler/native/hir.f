\ hir.f - the straight-line HIR dialect: the closed set of operations the
\ resolved Habu IR has for a colon body that only computes with integers.
\
\ docs/compiler-ir-design.md section 7.2 ("Stage N1: HIR - resolved Habu IR")
\ and section 5.3 ("closed-world operation schemas"). Section 5.3 line 229 says
\ each dialect has an exhaustive operation family and one schema table; this
\ file is that family for the straight-line subset, and it fills a module's
\ schema table through the IR-SCHEMA builder that src/compiler/ir/build.f owns.
\ It defines no storage of its own and it does not repeat a single check
\ IR-SCHEMA already makes.
\
\ WHAT THE SUBSET IS. Thirty-nine opcodes, and nothing else. Twenty-seven of them
\ compute with cells:
\   hir.const     an integer literal
\   hir.add       integer addition
\   hir.sub       integer subtraction
\   hir.mul       integer multiplication
\   hir.div       signed integer division, truncating toward zero
\   hir.lt        signed less-than, answering a Habu flag
\   hir.le        signed less-than-or-equal, answering a Habu flag
\   hir.gt        signed greater-than, answering a Habu flag
\   hir.ge        signed greater-than-or-equal, answering a Habu flag
\   hir.eq        equality, answering a Habu flag - the opcode family spells
\                 this member `equal`, because an ENUM that derives a comparison
\                 word has already taken the name `eq`
\   hir.ne        inequality, answering a Habu flag
\   hir.and       bitwise and
\   hir.or        bitwise or
\   hir.xor       bitwise exclusive or
\   hir.lshift    logical shift left by a count the program computed
\   hir.rshift    logical shift right by a count the program computed
\   hir.invert    bitwise complement, the one unary operation of the subset
\   hir.mem       the memory the definition is entered with
\   hir.load      read one cell from an address the program computed
\   hir.store     write one cell to an address the program computed
\   hir.bload     read one BYTE from an address the program computed
\   hir.bstore    write one BYTE to an address the program computed
\   hir.br        go on to one block, handing it the live values
\   hir.brz       go on to one of two blocks, on whether a value is zero
\   hir.call      call the word being compiled, handing it the values it takes
\                 and every value the caller still holds
\   hir.wordcall  call ANOTHER word, at the entry address and under the arity the
\                 operation carries, handing it the same thing
\   hir.return    leave the function with the word's outputs
\ and twelve compute with doubles:
\   hir.fconst    a double literal, carrying the cell the double is
\   hir.fadd      double addition
\   hir.fsub      double subtraction
\   hir.fmul      double multiplication
\   hir.fdiv      double division, which does not trap - it answers an infinity
\                 or the default NaN
\   hir.fneg      negation, the sign bit turned over
\   hir.fabs      absolute value
\   hir.fsqrt     square root
\   hir.int>real  a signed cell rounded to the nearest double
\   hir.real>int  a double truncated toward zero into a signed cell
\   hir.bits>real a data-stack cell read as the double it holds
\   hir.real>bits a double read as the cell it is
\
\ WHY A DOUBLE IS A SECOND TYPE AND NOT A SECOND SET OF OPERATIONS ON CELLS. A
\ Habu stack holds a double unboxed, in one cell, as its own bit pattern, so
\ nothing about its SIZE distinguishes it from an integer. What distinguishes it
\ is that no operation reads one as the other: adding two doubles is not adding
\ two cells and the machine has two register files to prove it. So the difference
\ is carried by the value's type - REAL-TYPE below - which is what makes handing
\ a double to `hir.add` a statement the module cannot hold rather than a mistake
\ some later pass might notice. The two crossings are the last two opcodes, they
\ compute nothing, and they exist because the source language really does cross
\ there: a word's arguments arrive in data-stack cells and `@` answers the cell a
\ double was stored in.
\
\ That is exactly what section 7.2's list needs for a colon body with no
\ control flow, no calls, no locals and no strings. Section 7.2 names many more
\ operations - `quotation`, `execute`, `catch` and the rest - and every one of
\ them is a later leaf of the same chain. An opcode with no elaborator, no
\ lowering and no test would be a promise, not a schema, so none is declared
\ here.
\
\ WHY SIX COMPARISONS AND NOT THREE WITH THE OPERANDS TURNED ROUND. `a > b` is
\ `b < a` and `a >= b` is `b <= a`, so the greater-than pair could have been the
\ less-than pair read backwards, and a source word could have been modelled as
\ "this opcode, with its operands swapped". It is not, for two reasons. The word
\ model of src/compiler/native/hir-word.f binds a source word to ONE opcode and
\ nothing else, so an operand order would have to become a second field on every
\ row and a second thing every reader has to consult before it knows what a row
\ means. And `<>` cannot be reached by swapping at all - it is `=` inverted, and
\ inverting a flag is an operation, not an argument order - so a scheme that
\ turned operands round would still need a member for it and would then have two
\ ways of saying the same kind of thing. Six opcodes for six source relations is
\ one rule: a comparison names its relation, and every stage below reads that
\ name.
\
\ WHY A SHIFT TAKES ITS COUNT AS AN OPERAND. Habu's `lshift` and `rshift` take
\ the count off the stack, so the count is whatever the program computed and not
\ a field of the instruction - which is why they are ordinary binary operations
\ here and lower to the machine's shift-BY-REGISTER forms. What that makes them
\ agree with is the engine: the register forms take the count modulo the register
\ width, so `1 64 lshift` is 1 and not 0, and the engine's own `lshift` answers
\ the same because it is the same instruction. A dialect that declared a count
\ ceiling here would be inventing a rule the interpreted word does not keep.
\
\ AND WHY THE COMPLEMENT IS AN OPERATION RATHER THAN AN EXCLUSIVE OR WITH ALL
\ ONES. `invert` is `-1 xor` and the engine computes it that way, by moving all
\ ones into a register first. In a compiled routine that literal is a value like
\ any other: it is materialised by a move-wide chain, which for all-ones is four
\ instructions, and it occupies a register the allocator then has to place. The
\ machine has a one-instruction complement, so modelling `invert` as its own
\ operation is what lets the chain reach it - and the unary shape is the honest
\ one anyway, because `invert` reads one value and answers one.
\
\ WHY MEMORY NEEDS THREE OPCODES AND NOT TWO. A load and a store say what a
\ program does to memory; they do not say in which order it does it, and the
\ order is the whole of what `!` then `@` to one address means. In an SSA module
\ an order is a value: each access takes the memory as it stood and answers the
\ memory as it now stands, so a load written after a store reads that store's
\ answer and no later pass may lift it above. That value has to start somewhere,
\ and hir.mem is where - it is the memory the definition is entered with. It
\ computes nothing, reads nothing and writes nothing, so it is declared pure and
\ carries only the token it mints; src/compiler/native/select.f gives it no
\ instruction at all, because on this machine the routine's memory order already
\ begins where the routine takes the caller's operands.
\
\ WHY THE WIDTH OF AN ACCESS IS A FORM AND NOT AN ATTRIBUTE. `@` and `c@` reach
\ the same address space in the same order, and they differ only in how many
\ bytes they move - so a width field on ONE memory opcode looks like the smaller
\ change. It is the wrong one. An operation's schema is what says what the
\ operation IS: its operand and result types, its effect on memory, the
\ instruction a selector may lower it to. A width carried as an attribute would
\ leave one schema standing for two accesses that write different numbers of
\ bytes, so every consumer - the selector, the emitter, and any later pass that
\ has to know whether two accesses overlap - would have to read an attribute
\ before it knew what it was looking at, and a schema-driven check could no
\ longer tell a wrong lowering from a right one. It would also make an
\ unwritable state writable: an attribute is a number, and a number can be a
\ width no machine form of this target has. Two forms make the closed world of
\ design line 229 do the work instead - `hir.bload` and `hir.bstore` are members
\ of the opcode family, every MATCH over the family has to answer for them, and
\ a width the machine cannot encode cannot be spelled at all. That is exactly
\ how the cell forms were built, and the byte forms are built the same way.
\
\ THE ADDRESS IS AN OPERAND, WHICH IS WHY THIS IS NOT THE FRAME. The two memory
\ forms of the machine dialect that existed before this one reach a frame slot,
\ whose base is the stack pointer and whose offset is a field of the
\ instruction - nothing the program computes. These two reach wherever the
\ program says: the address is an ordinary value of this dialect, defined by
\ whatever computed it, so `BUMP-CELL @` and `a i + c@` are the same shape with
\ different arithmetic in front of them. The effect is therefore declared in the
\ generic address space with unrestricted aliasing: an address a program
\ computed may name any cell it can reach, and a dialect that claimed otherwise
\ would license a reordering nothing proved.
\
\ WHY DUP, DROP, SWAP AND OVER ARE NOT OPCODES. Section 7.3 line 758 is explicit:
\ those words "produce no SIR operation and therefore no runtime instruction",
\ because they only rearrange the compile-time value vector. An `hir.dup`
\ opcode would create an operation whose whole job is to be deleted again one
\ stage later, and the old emitter's stack traffic is precisely what this
\ pipeline exists to stop emitting. They are modeled instead as compile-time
\ stack renames in src/compiler/native/hir-word.f, which is the other half of
\ this dialect: this file says which operations exist, that one says what a
\ Habu source word means.
\
\ THE TRAP FLAG IS THE UNIT'S NUMERIC POLICY, NOT A CONSTANT. Design line 240
\ records whether an operation may trap, and design section 5.5 puts the
\ numerical policy on the compilation unit. Whether integer overflow traps is
\ therefore not a fact about addition; it is a fact about the binding this
\ context was created with. REGISTER reads CNUM:OVERFLOW@ off the context's
\ bound policy, so the same three arithmetic opcodes register as may-trap under
\ a trapping policy and as total under a wrapping one. Nothing here re-derives
\ that policy or carries a default for it.
\
\ THE TARGET IS AARCH64, DELIBERATELY. Design line 241 makes a schema declare
\ the architecture and features its operation needs, and IR-SCHEMA validates
\ that declaration against the context's bound target contract. This is the
\ native pipeline, so its dialect requires `aarch64` with the baseline feature
\ set: integer add, subtract and multiply need no floating point and no SIMD.
\ The consequence is intended - registering this dialect against a GPU binding
\ is refused by IR-SCHEMA rather than quietly accepted.
\
\ WHY THE DIALECT IDENTITY LIVES HERE. A schema table's dialect name and schema
\ version are header fields of the module (design line 1714), fixed when the
\ builder is created, and only the dialect knows them. NEW-BUILDER supplies
\ them, so a module that is going to hold these opcodes is created with this
\ dialect's own name and version instead of a caller's spelling of them. That
\ used to be all: a caller who created a builder through IR-BUILD directly, and
\ named its table something else, could still register these rows into it, and
\ nothing but the usage rule said not to. REGISTER now reads the table's own
\ dialect and version back off the live module through IR-BUILD's live readers
\ and refuses a table that is not this dialect's, so the rule is a check.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/type.f
require src/compiler/ir/schema.f
require src/compiler/ir/build.f

package HIR
public

\ The whole operation family of the straight-line subset. It is an ENUM and not
\ a list of names because design line 229's closed world is then a property of
\ the type: a later stage that binds a source word, or matches on what an
\ operation is, cannot name an operation this dialect does not have, and every
\ MATCH over it has to answer for all five.
ENUM opcode DERIVE eq
   const
   add
   sub
   mul
   div
   lt
   le
   gt
   ge
   equal
   ne
   and
   or
   xor
   lshift
   rshift
   invert
   mem
   load
   store
   bload
   bstore
   br
   brz
   call
   wordcall
   return
   fconst
   fadd
   fsub
   fmul
   fdiv
   fneg
   fabs
   fsqrt
   intreal
   realint
   bitsreal
   realbits
;ENUM

\ What a structured control word does to the blocks a definition is made of.
\ Each member names one Habu source word, and the pairs are openers and closers
\ of one structure: `if` closes with `then`, `begin` with either `until` or
\ `repeat`, `?do` with `loop`. `index` is the loop index `i`, which is neither -
\ it reads the innermost open counted loop's index and stages no operation of
\ its own. `self-call` is `RECURSE`: it calls the word being compiled. It is a
\ control action rather than an operation word because how many values it takes
\ and leaves is the DEFINITION's arity, which no schema-driven staging can read
\ off an opcode - the elaborator stages it by hand, exactly as it stages the
\ return. It is an ENUM for the same reason the opcode family is: a table that
\ binds a source word to a control action cannot name an action this dialect has
\ no construction for, and every MATCH over it has to answer for all of them.
\
\ TWO MEMBERS ARE NEITHER AN OPENER NOR A CLOSER, AND THEY ARE SPELLED `mid-`.
\ `mid-while` is `while`, which stands inside an open `begin` loop and leaves it
\ when its test is false; `mid-else` is `else`, which stands inside an open `if`
\ and starts its second arm. Both END the block they stand in and start another
\ one without touching the control stack's depth, which is exactly what makes
\ them a third kind rather than a badly-named opener: a structure that has met
\ one of them is still open and still has to be closed by the closer it began
\ with.
ENUM ctrl DERIVE eq
   open-if
   mid-else
   close-if
   open-begin
   mid-while
   close-until
   close-repeat
   open-do
   close-loop
   index
   drop-loop
   early-exit
   self-call
;ENUM

\ What a Habu source word, or a source-tape token, means to this dialect.
\ `literal` is a token's meaning and never a word's: an integer literal is not a
\ call, and the tape's own token kind is what makes it one. The other three are
\ a word's meaning and never a token kind's. `op` elaborates to one operation
\ above, `rename` only rearranges the compile-time value vector and produces no
\ operation at all (section 7.3 line 758), and `unmodeled` is a named boundary
\ checked source may not compile yet. src/compiler/native/hir-word.f is the
\ table that binds a word to one of them; the vocabulary lives here because it
\ is the dialect's, not the table's.
\ `fixed` is the meaning of a word that pushes one value and nothing else, which
\ is what a `create`d data word does: its address is decided once, when the word
\ is created, and every mention of it is that number. The row carries the value,
\ so the caller that builds the word model states it - the address of a data
\ word in this process is not something this dialect can look up yet, and dot
\ habu-resolve-a-data-a1c8067f is where that lookup lands.
\ `open-locals` and `close-locals` are the two halves of a `{: … :}` group. A
\ group binds names to values the body then reads by name, and a bound name is
\ nothing but a value of the compile-time vector - so the group stages no
\ operation, exactly as a rename does, and the names between the two halves are
\ the PROGRAM's rather than this dialect's. That is why they are two meanings
\ and not one: what the opener does is start reading names, and what the closer
\ does is take one value off the vector per name read, right to left.
\ `callable` is the meaning of a word that is CALLED: a word already compiled and
\ published, whose entry address and declared arity the row carries. It is not a
\ `control` action the way `RECURSE` is, because `RECURSE` needs no name of its
\ own - it means the definition being compiled - while a callable word is a
\ different routine for every row, so what it means IS the row's payload.
\ `real-literal` is the second token meaning, and it is a meaning of its own
\ rather than `literal` with a type beside it for the same reason `hir.fconst` is
\ an opcode of its own: what a token MEANS is what the elaborator stages for it,
\ and the two stage two different operations leaving values of two different
\ types. The tape's own token kind is what makes a token one or the other, and
\ neither is ever a word's meaning.
ENUM meaning DERIVE eq
   literal
   real-literal
   op
   const-op
   control
   rename
   fixed
   callable
   open-locals
   close-locals
   unmodeled
;ENUM

private

\ ---- the dialect's own symbols -----------------------------------------------
\ Every symbol this dialect mints is spelled `hir.`-something. One module
\ interner holds opcode names, attribute keys, semantic-rule names, renderer
\ names and - once src/compiler/native/hir-word.f fills its table - the
\ spellings of Habu source words, so the prefix is what keeps a dialect symbol
\ and a source word from ever being the same interned symbol.

: CELL-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ Design line 240 with design section 5.5: whether integer overflow traps is the
\ compilation unit's numeric policy, so the schema reads it rather than fixing
\ it.
: TRAPS? ( IR-CTX:ctx -- bool )
   IR-CTX:BINDING@ CBIND:POLICY@ CNUM:OVERFLOW@
   CNUM-OVERFLOW:TRAP CNUM-OVERFLOW:EQ ;

\ Design line 241: the native pipeline's architecture, with the baseline feature
\ set, because integer arithmetic needs nothing more.
: TARGET ( -- )
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET ;

\ Design line 241 again, for the operations that need a floating unit. It is a
\ different requirement and it is declared as one: a binding whose target
\ contract has no floating-point feature is refused by IR-SCHEMA at the first
\ float schema rather than compiling a body it cannot execute.
: FP-TARGET ( -- )
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH
   IR-SCHEMA:SET-TARGET ;

\ Design lines 236-238: a value-producing straight-line operation ends no block,
\ names no successor, holds no region, and carries no effect token.
: PURE-VALUE ( -- )
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE ;

public

\ ---- the dialect identity ----------------------------------------------------
: NAME ( -- ptr u8 n )
   s" hir" ;

\ Version 0.1: the straight-line subset is not the whole of section 7.2, and the
\ major version stays at zero until it is.
0 constant MAJOR
1 constant MINOR

\ ---- the opcode names --------------------------------------------------------
\ This module's interned symbol for one opcode. Interning deduplicates, so
\ asking twice answers the same identity, and this is the symbol both
\ IR-SCHEMA's readers and IR-BUILD:BEGIN-OP take.
: OPCODE ( IR-CTX:ctx IR-BUILD:builder HIR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      const  OF s" hir.const"  ENDOF
      add    OF s" hir.add"    ENDOF
      sub    OF s" hir.sub"    ENDOF
      mul    OF s" hir.mul"    ENDOF
      div    OF s" hir.div"    ENDOF
      lt     OF s" hir.lt"     ENDOF
      le     OF s" hir.le"     ENDOF
      gt     OF s" hir.gt"     ENDOF
      ge     OF s" hir.ge"     ENDOF
      equal  OF s" hir.eq"     ENDOF
      ne     OF s" hir.ne"     ENDOF
      and    OF s" hir.and"    ENDOF
      or     OF s" hir.or"     ENDOF
      xor    OF s" hir.xor"    ENDOF
      lshift OF s" hir.lshift" ENDOF
      rshift OF s" hir.rshift" ENDOF
      invert OF s" hir.invert" ENDOF
      mem    OF s" hir.mem"    ENDOF
      load   OF s" hir.load"   ENDOF
      store  OF s" hir.store"  ENDOF
      bload  OF s" hir.bload"  ENDOF
      bstore OF s" hir.bstore" ENDOF
      br     OF s" hir.br"     ENDOF
      brz    OF s" hir.brz"    ENDOF
      call   OF s" hir.call"   ENDOF
      wordcall OF s" hir.wordcall" ENDOF
      return OF s" hir.return" ENDOF
      fconst   OF s" hir.fconst"    ENDOF
      fadd     OF s" hir.fadd"      ENDOF
      fsub     OF s" hir.fsub"      ENDOF
      fmul     OF s" hir.fmul"      ENDOF
      fdiv     OF s" hir.fdiv"      ENDOF
      fneg     OF s" hir.fneg"      ENDOF
      fabs     OF s" hir.fabs"      ENDOF
      fsqrt    OF s" hir.fsqrt"     ENDOF
      intreal  OF s" hir.int>real"  ENDOF
      realint  OF s" hir.real>int"  ENDOF
      bitsreal OF s" hir.bits>real" ENDOF
      realbits OF s" hir.real>bits" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

\ Design line 479: the attribute key `hir.const` requires. The literal's value
\ is the whole content of a constant, so an `hir.const` without it means
\ nothing, and IR-OP refuses one that omits it.
: KEY-VALUE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.value" IR-BUILD:INTERN-SYMBOL ;

\ The three attribute keys `hir.wordcall` requires. A call to another word means
\ nothing without all three: where the callee starts, how many values it takes,
\ and how many it leaves. They are three keys and not one packed number because
\ a key answers "which fact, in which units", and a reader that had to unpack a
\ triple could get the fields in the wrong order without any authority noticing.
\
\ WHY THE ARITY IS ON THE OPERATION AND NOT DERIVED FROM ITS LISTS. Both of the
\ operation's lists are variadic - how many values are live across a call is the
\ call site's fact and how many the callee moves is the routine's - so the two
\ counts cannot be told apart by counting. The arity is what splits them, and it
\ has to be carried.
: KEY-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.entry" IR-BUILD:INTERN-SYMBOL ;

: KEY-IN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.in" IR-BUILD:INTERN-SYMBOL ;

: KEY-OUT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.out" IR-BUILD:INTERN-SYMBOL ;

\ ---- the type of the memory order --------------------------------------------
\ The order of the definition's memory accesses, as a value they pass along. It
\ lives in no register and stands for no number: it is what makes "this load
\ happens after that store" a dependency the module holds rather than a property
\ of the printed order. Every stage that has to tell a general value from an
\ ordering value reads this identity, which is why it is one public reader here
\ rather than a type interned at each use site.
: MEM-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-DOMAIN:DATA-MEM IR-BUILD:INTERN-TOKEN ;

\ ---- the type of a double ----------------------------------------------------
\ The second value type of this dialect. A Habu stack holds a double in one
\ unboxed cell as its own bit pattern, so a double and a cell are the same eight
\ bytes and the same slot - but they are NOT the same value: no arithmetic of
\ this dialect reads one as the other, and which of the two a value is decides
\ which register file can hold it and which instruction may compute with it. So
\ the difference is a TYPE and not a convention, and this reader is the one place
\ that says it, exactly as MEM-TYPE is for the memory order.
\
\ WHY THE TWO ARE BRIDGED BY OPERATIONS RATHER THAN BY A CONVERSION. `hir.bits>real`
\ and `hir.real>bits` below are the two crossings, and they compute nothing: they
\ are the same eight bytes read as the other type. They exist because the SOURCE
\ language crosses there - `@` answers the cell a double was stored in, `!` puts
\ one back, and a word's arguments and results reach it through data-stack cells -
\ so the crossing is a real event of a real program and a dialect that hid it
\ would be claiming the machine moves nothing.
: REAL-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-FMT:DOUBLE IR-BUILD:INTERN-FLT ;

private

\ ---- the schema definitions --------------------------------------------------
\ Design lines 242 and 243 require a semantic-rule identifier and a renderer
\ identifier per schema, so a later pass dispatches on an identity rather than
\ on a string comparison. Each opcode names its own, derived from its own
\ spelling. Neither is public: the schema table is the authority on what an
\ opcode's rule and renderer are, and IR-SCHEMA:RULE@ and RENDERER@ answer it.
: RULE ( IR-CTX:ctx IR-BUILD:builder HIR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      const  OF s" hir.rule.const"  ENDOF
      add    OF s" hir.rule.add"    ENDOF
      sub    OF s" hir.rule.sub"    ENDOF
      mul    OF s" hir.rule.mul"    ENDOF
      div    OF s" hir.rule.div"    ENDOF
      lt     OF s" hir.rule.lt"     ENDOF
      le     OF s" hir.rule.le"     ENDOF
      gt     OF s" hir.rule.gt"     ENDOF
      ge     OF s" hir.rule.ge"     ENDOF
      equal  OF s" hir.rule.eq"     ENDOF
      ne     OF s" hir.rule.ne"     ENDOF
      and    OF s" hir.rule.and"    ENDOF
      or     OF s" hir.rule.or"     ENDOF
      xor    OF s" hir.rule.xor"    ENDOF
      lshift OF s" hir.rule.lshift" ENDOF
      rshift OF s" hir.rule.rshift" ENDOF
      invert OF s" hir.rule.invert" ENDOF
      mem    OF s" hir.rule.mem"    ENDOF
      load   OF s" hir.rule.load"   ENDOF
      store  OF s" hir.rule.store"  ENDOF
      bload  OF s" hir.rule.bload"  ENDOF
      bstore OF s" hir.rule.bstore" ENDOF
      br     OF s" hir.rule.br"     ENDOF
      brz    OF s" hir.rule.brz"    ENDOF
      call   OF s" hir.rule.call"   ENDOF
      wordcall OF s" hir.rule.wordcall" ENDOF
      return OF s" hir.rule.return" ENDOF
      fconst   OF s" hir.rule.fconst"    ENDOF
      fadd     OF s" hir.rule.fadd"      ENDOF
      fsub     OF s" hir.rule.fsub"      ENDOF
      fmul     OF s" hir.rule.fmul"      ENDOF
      fdiv     OF s" hir.rule.fdiv"      ENDOF
      fneg     OF s" hir.rule.fneg"      ENDOF
      fabs     OF s" hir.rule.fabs"      ENDOF
      fsqrt    OF s" hir.rule.fsqrt"     ENDOF
      intreal  OF s" hir.rule.int>real"  ENDOF
      realint  OF s" hir.rule.real>int"  ENDOF
      bitsreal OF s" hir.rule.bits>real" ENDOF
      realbits OF s" hir.rule.real>bits" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

: RENDERER ( IR-CTX:ctx IR-BUILD:builder HIR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      const  OF s" hir.render.const"  ENDOF
      add    OF s" hir.render.add"    ENDOF
      sub    OF s" hir.render.sub"    ENDOF
      mul    OF s" hir.render.mul"    ENDOF
      div    OF s" hir.render.div"    ENDOF
      lt     OF s" hir.render.lt"     ENDOF
      le     OF s" hir.render.le"     ENDOF
      gt     OF s" hir.render.gt"     ENDOF
      ge     OF s" hir.render.ge"     ENDOF
      equal  OF s" hir.render.eq"     ENDOF
      ne     OF s" hir.render.ne"     ENDOF
      and    OF s" hir.render.and"    ENDOF
      or     OF s" hir.render.or"     ENDOF
      xor    OF s" hir.render.xor"    ENDOF
      lshift OF s" hir.render.lshift" ENDOF
      rshift OF s" hir.render.rshift" ENDOF
      invert OF s" hir.render.invert" ENDOF
      mem    OF s" hir.render.mem"    ENDOF
      load   OF s" hir.render.load"   ENDOF
      store  OF s" hir.render.store"  ENDOF
      bload  OF s" hir.render.bload"  ENDOF
      bstore OF s" hir.render.bstore" ENDOF
      br     OF s" hir.render.br"     ENDOF
      brz    OF s" hir.render.brz"    ENDOF
      call   OF s" hir.render.call"   ENDOF
      wordcall OF s" hir.render.wordcall" ENDOF
      return OF s" hir.render.return" ENDOF
      fconst   OF s" hir.render.fconst"    ENDOF
      fadd     OF s" hir.render.fadd"      ENDOF
      fsub     OF s" hir.render.fsub"      ENDOF
      fmul     OF s" hir.render.fmul"      ENDOF
      fdiv     OF s" hir.render.fdiv"      ENDOF
      fneg     OF s" hir.render.fneg"      ENDOF
      fabs     OF s" hir.render.fabs"      ENDOF
      fsqrt    OF s" hir.render.fsqrt"     ENDOF
      intreal  OF s" hir.render.int>real"  ENDOF
      realint  OF s" hir.render.real>int"  ENDOF
      bitsreal OF s" hir.render.bits>real" ENDOF
      realbits OF s" hir.render.real>bits" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

\ The two fields every schema of this dialect names the same way.
: NAMED ( IR-CTX:ctx IR-BUILD:builder HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:HIR:opcode :}
   c b o RULE IR-SCHEMA:SET-RULE
   c b o RENDERER IR-SCHEMA:SET-RENDERER ;

\ An integer literal: no operands, one cell of result, and the value it holds.
: DEF-CONST ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b HIR-OPCODE:CONST OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-VALUE IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:CONST NAMED
   c b IR-BUILD:DEFINE-OP ;

\ One binary integer operation: two cells in, one cell out. The three arithmetic
\ opcodes differ only in their names, so they share this shape, and each one's
\ may-trap flag is the compilation unit's overflow policy.
: DEF-BINARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:HIR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   c TRAPS? IR-SCHEMA:SET-TRAP
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Division: the same two cells in and one cell out, and it is the one arithmetic
\ operation of this dialect that may trap whatever the unit's numeric policy
\ says. The policy is about OVERFLOW, and a division does not overflow the way a
\ sum does; what it does is divide by zero, and the engine's own `/` traps on
\ that unconditionally - src/habu/habu1.f BDIV0? branches over a `brk` when the
\ divisor is not zero. So the may-trap flag is declared true here rather than
\ read off the policy, and the machine dialect's lowering has to reproduce the
\ trap rather than drop it.
: DEF-DIV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b HIR-OPCODE:DIV OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:DIV NAMED
   c b IR-BUILD:DEFINE-OP ;

\ One binary operation that cannot raise: two cells in, one cell out, and the
\ may-trap flag declared false rather than read off the unit's numeric policy.
\ Eleven opcodes have this shape - the six comparisons, whose answer is a Habu
\ flag of all bits set or none, and the three bitwise combinations and two
\ shifts. They are one definer and not two because the schema of a comparison and
\ the schema of a bitwise combination are the same statement: two cells in, one
\ cell out, pure, total.
\
\ AND WHAT MAKES THEM TOTAL IS THE SAME FACT IN BOTH CASES. The unit's policy is
\ about OVERFLOW, and none of these eleven can overflow - a comparison answers
\ one of two values, a bitwise combination answers a function of its arguments'
\ bits, and a shift takes its count modulo the register width. Declaring them
\ through DEF-BINARY would make them trapping under a trapping unit and oblige
\ the machine stage to reproduce a trap that cannot happen.
: DEF-TOTAL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:HIR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The one unary operation of this subset: one cell in, one cell out, and total.
\ `invert` reads one value and answers its complement, and the schema says so -
\ which is what stops a caller staging it with the two operands every other
\ computing operation here takes.
: DEF-UNARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:HIR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the memory forms --------------------------------------------------------
\ Design lines 238 and 239: an operation that touches memory declares the domain,
\ the address space and the alias behaviour, and carries the token that orders it
\ against the others. The address these two forms reach is a value the program
\ computed, so it may name any cell the program can reach: the space is the
\ generic one and the aliasing is unrestricted, which is the declaration that
\ forbids a later pass from moving a load across a store to "another" address it
\ cannot prove is another.
: GENERIC-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

\ The memory the definition is entered with. It reads nothing and writes
\ nothing - it is where the order STARTS, not an access - so it is declared pure
\ and its whole content is the token it mints. Being pure is what lets it carry
\ a token result with no token operand: there is nothing before it to take one
\ from.
: DEF-MEM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b HIR-OPCODE:MEM OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:MEM NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Reading one cell: the address, and the memory as it stands. It answers the
\ cell's contents and the memory it read them out of, so the access after it
\ takes that answer and cannot be lifted above this one. The token is the LAST
\ operand and the LAST result of both forms, which is this dialect's own
\ convention and is why src/compiler/native/elaborate.f finds it by TYPE rather
\ than by position.
: DEF-LOAD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b HIR-OPCODE:LOAD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:READ GENERIC-MEM
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:LOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Writing one cell: the value, the address, and the memory as it stands. Forth
\ writes `value address !`, so the value is the deeper of the two on the data
\ stack and therefore the first operand - the same rule every other binary
\ operation of this dialect follows, and the reason a swapped pair is a wrong
\ program rather than a wrong index.
: DEF-STORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b HIR-OPCODE:STORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:WRITE GENERIC-MEM
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:STORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Reading one byte: the same address and order the cell load takes, and the same
\ two answers. What differs is the number of bytes the access moves, and that is
\ the whole content of the form - the value it answers is the byte widened into a
\ cell, which is what `c@` leaves on a Habu stack. The operand and result types
\ are the cell type for that reason: a byte is not a type of this dialect, it is
\ a width of an access.
: DEF-BLOAD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b HIR-OPCODE:BLOAD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:READ GENERIC-MEM
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:BLOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Writing one byte: the value, the address, and the memory as it stands, in the
\ order Forth writes `value address c!`. Only the value's lowest byte reaches
\ memory, which is what the machine form encodes and what the engine's own `c!`
\ does; the operand is still a cell, because a cell is what the program has.
: DEF-BSTORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b HIR-OPCODE:BSTORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:WRITE GENERIC-MEM
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:BSTORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Going on to one block, handing it the values that are still live. Design lines
\ 706-708 make a terminator's operands the successor's block arguments and
\ design line 532 makes the verifier match their count and types against the
\ destination, so how many there are is a property of the destination and the
\ list is one variadic tail.
: DEF-BR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b HIR-OPCODE:BR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 1 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:BR NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Going on to the first successor when the tested value is zero and to the
\ second when it is not, which is how every structured control word of this
\ subset asks its question. Its one operand is the value it tests and not a
\ block argument: with two successors the operation model has no way to say
\ which operand belongs to which destination, so both successors take no
\ arguments and an edge that has to carry values goes through a block whose
\ terminator is the unconditional form above.
: DEF-BRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b HIR-OPCODE:BRZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:BRZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Leaving the function. Design line 237 makes this a terminator, and design
\ lines 706-708 hand the outputs to the one exit block as its arguments, so it
\ has operands and no results. A word's output count is a property of the word,
\ not of the opcode, so the operand list is one variadic cell: zero or more
\ outputs, each a cell.
: DEF-RETURN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b HIR-OPCODE:RETURN OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:RETURN NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Calling the word being compiled, which is what `RECURSE` is. Its operands are
\ the memory order and EVERY value the caller still holds, bottom first, with the
\ arguments the callee takes as the top of them; its results are the order again
\ and those same values, with the callee's outputs in place of its arguments.
\
\ WHY THE WHOLE LIVE VECTOR CROSSES THE OPERATION AND NOT JUST THE ARGUMENTS. The
\ callee is this same routine, so every register the caller could be holding a
\ value in is a register the callee writes: a Habu word's contract destroys
\ exactly the registers the allocator hands out, and there is no role in it for a
\ register that is written and put back. So a value the caller still needs does
\ not survive the call in a register, and the honest way to say that in the source
\ dialect is that the call CONSUMES it and ANSWERS it again - a different value,
\ defined by the call, which whatever computes with it afterwards reads instead.
\ The machine stage then has somewhere real to put it (the caller's own data
\ stack) and the register allocator sees the two lifetimes it really has rather
\ than one lifetime spanning a call that ends it.
\
\ WHY BOTH LISTS ARE VARIADIC. How many values are live across a call is a fact
\ about the call site, and how many the callee takes and leaves is a fact about
\ the routine - neither is a fact about the opcode, so neither can be a fixed
\ count in a schema. The order is the one fixed operand and the one fixed result,
\ so a reader finds it by position as well as by type.
\
\ AND WHY IT MAY TRAP. A call runs the callee, and whatever the callee can do the
\ call can do: the corpus's own recursive word divides nothing, but a call that
\ declared itself total would be claiming something about a routine rather than
\ about an operation. The machine lowering reproduces it exactly, by being the
\ same call, which is what src/compiler/native/select.f's trap rule requires.
: DEF-CALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b HIR-OPCODE:CALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND-TAIL
   k IR-SCHEMA:ADD-RESULT
   t IR-SCHEMA:ADD-RESULT-TAIL
   IR--SCHEMA-EFFECT:READ-WRITE GENERIC-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:CALL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Calling ANOTHER word. Everything about the shape is `hir.call`'s - the memory
\ order and every live value in, the order and those values out, both lists
\ variadic, read-write on generic memory, and may-trap because whatever the
\ callee can do the call can do. What it adds is the three fields that say WHICH
\ routine: its entry address and its declared arity.
\
\ WHY IT IS A SECOND OPERATION AND NOT A FIELD ON THE FIRST. The two differ in
\ their TARGET, and the two targets are not the same kind of thing. A self-call
\ goes to a block of the function being compiled, which is a label: its
\ displacement is known when the blocks are laid out, exactly as a branch's is,
\ and no address exists anywhere in the module. A call to another word goes to an
\ address, which is a number the module has to carry. Making one operation hold
\ either would mean a field that is sometimes meaningless, and a reader would
\ have to know which case it was in before it knew what the field meant - the
\ same argument that made the byte and cell accesses two forms rather than one
\ form with a width.
\
\ AND WHY THE CALLER'S SAVE DISCIPLINE IS UNCHANGED. The operation consumes every
\ live value and answers it again, exactly as `hir.call` does, so no register
\ crosses it whatever the callee destroys. That is what makes the discipline
\ correct against a callee this compiler did not produce: it assumes nothing about
\ the callee's registers, only that the callee keeps the convention a Habu word is
\ entered and left through.
: DEF-WORDCALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b HIR-OPCODE:WORDCALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND-TAIL
   k IR-SCHEMA:ADD-RESULT
   t IR-SCHEMA:ADD-RESULT-TAIL
   c b KEY-ENTRY IR-SCHEMA:ADD-ATTR
   c b KEY-IN IR-SCHEMA:ADD-ATTR
   c b KEY-OUT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE GENERIC-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:WORDCALL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the float forms ---------------------------------------------------------
\ A double literal: no operands, one double of result, and the value it holds -
\ which is the literal's own bit pattern, because the cell IS the double. It is a
\ second opcode rather than `hir.const` with a float result for the reason the
\ byte and cell accesses are two forms: a schema is what says what an operation
\ IS, and one schema standing for two operations that leave values of two
\ different types would leave every reader consulting the result type before it
\ knew what it was looking at.
: DEF-FCONST ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b HIR-OPCODE:FCONST OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-RESULT
   c b KEY-VALUE IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   FP-TARGET
   c b HIR-OPCODE:FCONST NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Two doubles in, one double out. The four arithmetic words share this shape, and
\ ALL FOUR ARE TOTAL - including the division, which is the one place this
\ dialect's float rules and its integer rules disagree on purpose. An integer
\ division by zero traps, and `hir.div` declares it; a float division by zero
\ answers an infinity and a zero over a zero answers the default NaN, neither of
\ which is a trap (survey (5) and (6) at the head of
\ tools/codegen-compare-corpus3.f, measured on this engine). A float operation
\ that declared itself trapping would oblige the machine stage to reproduce a
\ trap the hardware does not raise.
: DEF-FBINARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id o:HIR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   FP-TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ One double in, one double out: negate, absolute value and square root. The
\ square root of a negative is the default NaN rather than a raise, so it is
\ total with the rest of them.
: DEF-FUNARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id o:HIR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   FP-TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ One value of one type in, one value of the other out. Four operations have this
\ shape and they are FOUR and not two, because two of them compute and two do
\ not. `hir.int>real` rounds a signed cell to the nearest double and
\ `hir.real>int` truncates a double toward zero, saturating at the ends and
\ answering zero for a NaN - two different roundings, which is why the corpus
\ pins them in two rows (survey (8)). `hir.bits>real` and `hir.real>bits` round
\ nothing at all: they are the same eight bytes read as the other type, which is
\ what crossing between a data-stack cell and a double IS on this machine.
\ Modelling a rounding conversion and a reinterpretation as one operation with a
\ flag would let a wrong lowering read as a right one.
: DEF-CROSS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder ti:IR-ID:ir-type-id to:IR-ID:ir-type-id
      o:HIR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   ti IR-SCHEMA:ADD-OPERAND
   to IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   FP-TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the table this dialect may fill -----------------------------------------
\ Design line 229's closed world is per dialect, so an operation family may only
\ be defined into the schema table of the dialect it belongs to. The table's
\ dialect name and schema version are fixed when the module is created and
\ nothing can change them afterwards, so reading them back off the live module
\ decides it: the name is compared byte for byte through the module's own
\ interner, which appends nothing, and the version has to be the exact version
\ these definitions were written for. A module of another dialect, or of a later
\ or earlier version of this one, is refused before the first opcode is defined.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  NAME IR-BUILD:SYMBOL-IS?
   0= if E-HIR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MAJOR@ MAJOR <> if E-HIR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MINOR@ MINOR <> if E-HIR-DIALECT throw then ;

public

\ ---- creation and registration -----------------------------------------------
\ Create a builder for a module of this dialect. The staged IR-BUILD plan is
\ consumed here exactly as IR-BUILD:NEW-BUILDER consumes it; what this word
\ adds is the dialect's own name and schema version, which no caller should be
\ spelling out.
: NEW-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   NAME MAJOR MINOR IR-BUILD:NEW-BUILDER ;

\ Define the whole straight-line operation family into this builder's schema
\ table. Nearly every check belongs to IR-SCHEMA:DEFINE - the module owns each
\ symbol and type, the target contract admits the requirement, no opcode is
\ defined twice, the ceilings hold - so registering twice, or against a module
\ or a target that cannot hold these schemas, is refused there and this word
\ repeats none of it. The one check that is this dialect's own is the first
\ line: a schema table belongs to exactly one dialect at one schema version, and
\ IR-SCHEMA cannot make it because it has no opinion about which dialect its
\ caller is. Definition is one opcode at a time, so a refusal leaves the opcodes
\ that were already defined and defines no more.
: REGISTER ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b DIALECT-CK
   c b CELL-TYPE {: t:IR-ID:ir-type-id :}
   c b MEM-TYPE {: k:IR-ID:ir-type-id :}
   c b t DEF-CONST
   c b t HIR-OPCODE:ADD DEF-BINARY
   c b t HIR-OPCODE:SUB DEF-BINARY
   c b t HIR-OPCODE:MUL DEF-BINARY
   c b t DEF-DIV
   c b t HIR-OPCODE:LT DEF-TOTAL
   c b t HIR-OPCODE:LE DEF-TOTAL
   c b t HIR-OPCODE:GT DEF-TOTAL
   c b t HIR-OPCODE:GE DEF-TOTAL
   c b t HIR-OPCODE:EQUAL DEF-TOTAL
   c b t HIR-OPCODE:NE DEF-TOTAL
   c b t HIR-OPCODE:AND DEF-TOTAL
   c b t HIR-OPCODE:OR DEF-TOTAL
   c b t HIR-OPCODE:XOR DEF-TOTAL
   c b t HIR-OPCODE:LSHIFT DEF-TOTAL
   c b t HIR-OPCODE:RSHIFT DEF-TOTAL
   c b t HIR-OPCODE:INVERT DEF-UNARY
   c b k DEF-MEM
   c b t k DEF-LOAD
   c b t k DEF-STORE
   c b t k DEF-BLOAD
   c b t k DEF-BSTORE
   c b t DEF-BR
   c b t DEF-BRZ
   c b t k DEF-CALL
   c b t k DEF-WORDCALL
   c b t DEF-RETURN
   c b REAL-TYPE {: f:IR-ID:ir-type-id :}
   c b f DEF-FCONST
   c b f HIR-OPCODE:FADD DEF-FBINARY
   c b f HIR-OPCODE:FSUB DEF-FBINARY
   c b f HIR-OPCODE:FMUL DEF-FBINARY
   c b f HIR-OPCODE:FDIV DEF-FBINARY
   c b f HIR-OPCODE:FNEG DEF-FUNARY
   c b f HIR-OPCODE:FABS DEF-FUNARY
   c b f HIR-OPCODE:FSQRT DEF-FUNARY
   c b t f HIR-OPCODE:INTREAL DEF-CROSS
   c b f t HIR-OPCODE:REALINT DEF-CROSS
   c b t f HIR-OPCODE:BITSREAL DEF-CROSS
   c b f t HIR-OPCODE:REALBITS DEF-CROSS ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
