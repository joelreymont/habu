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
\ WHAT THE STRAIGHT-LINE SUBSET IS. Five opcodes, and nothing else:
\   hir.const     an integer literal
\   hir.add       integer addition
\   hir.sub       integer subtraction
\   hir.mul       integer multiplication
\   hir.return    leave the function with the word's outputs
\ That is exactly what section 7.2's list needs for a colon body with no
\ control flow, no calls, no locals, no memory and no strings. Section 7.2 names
\ many more operations - `if`, `loop`, `quotation`, `execute`, `catch` and the
\ rest - and every one of them is a later leaf of the same chain. An opcode with
\ no elaborator, no lowering and no test would be a promise, not a schema, so
\ none is declared here.
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
\ dialect's own name and version instead of a caller's spelling of them. A
\ caller that creates a builder through IR-BUILD directly and then registers
\ here would name its table something else and still get these rows; making that
\ structurally impossible needs a live dialect reader on IR-BUILD, which this
\ leaf does not own. Dot habu-expose-live-ir-f0eaed6b tracks it.

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
   return
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
ENUM meaning DERIVE eq
   literal
   op
   rename
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
      return OF s" hir.return" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

\ Design line 479: the attribute key `hir.const` requires. The literal's value
\ is the whole content of a constant, so an `hir.const` without it means
\ nothing, and IR-OP refuses one that omits it.
: KEY-VALUE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.value" IR-BUILD:INTERN-SYMBOL ;

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
      return OF s" hir.rule.return" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

: RENDERER ( IR-CTX:ctx IR-BUILD:builder HIR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      const  OF s" hir.render.const"  ENDOF
      add    OF s" hir.render.add"    ENDOF
      sub    OF s" hir.render.sub"    ENDOF
      mul    OF s" hir.render.mul"    ENDOF
      return OF s" hir.render.return" ENDOF
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

public

\ ---- creation and registration -----------------------------------------------
\ Create a builder for a module of this dialect. The staged IR-BUILD plan is
\ consumed here exactly as IR-BUILD:NEW-BUILDER consumes it; what this word
\ adds is the dialect's own name and schema version, which no caller should be
\ spelling out.
: NEW-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   NAME MAJOR MINOR IR-BUILD:NEW-BUILDER ;

\ Define the whole straight-line operation family into this builder's schema
\ table. Every check belongs to IR-SCHEMA:DEFINE - the module owns each symbol
\ and type, the target contract admits the requirement, no opcode is defined
\ twice, the ceilings hold - so registering twice, or against a module or a
\ target that cannot hold these schemas, is refused there and this word adds no
\ check of its own. Definition is one opcode at a time, so a refusal leaves the
\ opcodes that were already defined and defines no more.
: REGISTER ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b CELL-TYPE {: t:IR-ID:ir-type-id :}
   c b t DEF-CONST
   c b t HIR-OPCODE:ADD DEF-BINARY
   c b t HIR-OPCODE:SUB DEF-BINARY
   c b t HIR-OPCODE:MUL DEF-BINARY
   c b t DEF-RETURN ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
