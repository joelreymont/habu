\ hir.f - the straight-line HIR dialect: the closed set of operations the
\ resolved Habu IR has for a colon body that only computes with integers.
\
\ The subset is CLOSED: an opcode with no elaborator, no lowering and no test
\ would be a promise rather than a schema, so none is declared here.
\
\ Every one of the five float comparisons answers FALSE for a NaN, both ways
\ round, measured on this engine. That is a fact about CONTROL FLOW, and
\ src/compiler/native/a64ir.f names the conditions that keep it.
\
\ A shift takes its count as an operand and lowers to the shift-BY-REGISTER
\ form, which takes the count modulo the register width - so `1 64 lshift` is
\ 1, exactly as the engine's own `lshift` answers.
\
\ hir.mem is where the memory order STARTS. It gets no instruction, because the
\ routine's order already begins where it takes the caller's operands.
\
\ An access width is a FORM and never an attribute, so a width the machine
\ cannot encode cannot be spelled. The address is an ordinary operand in the
\ GENERIC space with unrestricted aliasing, which forbids moving a load across
\ a store to an address nothing proved is another.
\
\ dup, drop, swap and over stage no operation at all: they are compile-time
\ renames in src/compiler/native/hir-word.f.
\
\ Whether an arithmetic opcode may trap is the unit's numeric policy, read off
\ the context's bound policy rather than fixed here. The target is aarch64 with
\ the baseline features, so registering against another binding is refused.

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

\ An ENUM, so a later stage cannot name an operation this dialect does not have
\ and every MATCH over it has to answer for every member. The member for `=` is
\ spelled `equal`, because a derived comparison word has already taken `eq`.
\ Six integer comparisons and not three read backwards, because a word model
\ binds a source word to ONE opcode; five float ones, because the engine's whole
\ float comparison vocabulary is five words.
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
   quot
   return
   trap
   fconst
   fadd
   fsub
   fmul
   fdiv
   fneg
   fabs
   fsqrt
   flt
   fgt
   feq
   fltz
   feqz
   intreal
   realint
   bitsreal
   realbits
;ENUM

\ One member per Habu control word. `mid-while` and `mid-else` end their block
\ and start another without changing the control stack's depth, which is what
\ makes them a third kind. `close-again` opens no block at all, and the engine
\ and the checker both refuse a `while` inside a `begin … again`. `do` runs its
\ body at least once where `?do` may run it none, which is why they are two
\ openers with one closer. `index` and `outer-index` are two members because
\ `k` is not a word of this Forth, so a depth field would have two values for
\ ever. `of` and `endof` are one member each, because the engine and the checker
\ tell the MATCH form from the case form by which structure is open.
ENUM ctrl DERIVE eq
   open-if
   mid-else
   close-if
   open-begin
   mid-while
   close-until
   close-repeat
   close-again
   open-do
   open-do-skip
   close-loop
   index
   outer-index
   drop-loop
   early-leave
   early-exit
   self-call
   open-match
   match-arm
   close-arm
   close-match
   open-case
   close-case
   make-bundle
   open-quot
   close-quot
   bind-defer
   exec
   catch
;ENUM

\ Three actions and not two, because a peek is not a pop: `fetch-r` copies the
\ return stack's top cells without taking them off.
ENUM rmove DERIVE eq
   to-r
   from-r
   fetch-r
;ENUM

\ `literal` and `real-literal` are token meanings and never a word's; the rest
\ are a word's and never a token kind's. `rename`, `open-locals`/`close-locals`
\ and `rstack` stage no operation at all - they only move value ids at compile
\ time, which is sound because the checker has already proved the return row.
ENUM meaning DERIVE eq
   literal
   real-literal
   string-literal
   op
   const-op
   control
   rename
   rstack
   fixed
   callable
   open-locals
   close-locals
   unmodeled
;ENUM

private

\ ---- the dialect's own symbols -----------------------------------------------
\ it.
\ Every symbol this dialect mints is spelled `hir.`-something, which is what
\ keeps a dialect symbol and a source word from being one interned symbol.
: TRAPS? ( IR-CTX:ctx -- bool )
   IR-CTX:BINDING@ CBIND:POLICY@ CNUM:OVERFLOW@
   CNUM-OVERFLOW:TRAP CNUM-OVERFLOW:EQ ;

\ The native pipeline's architecture, with the baseline feature set.
: TARGET ( -- )
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET ;

\ A different requirement, declared as one: a binding with no floating-point
\ feature is refused at the first float schema.
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

\ Every consumer compares the version exactly, so a table with a new required
\ attribute and one without are two different tables.
0 constant MAJOR
5 constant MINOR

\ ---- the opcode names --------------------------------------------------------
\ Interning deduplicates, so asking twice answers the same identity.
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
      quot   OF s" hir.quot"   ENDOF
      return OF s" hir.return" ENDOF
      trap   OF s" hir.trap"   ENDOF
      fconst   OF s" hir.fconst"    ENDOF
      fadd     OF s" hir.fadd"      ENDOF
      fsub     OF s" hir.fsub"      ENDOF
      fmul     OF s" hir.fmul"      ENDOF
      fdiv     OF s" hir.fdiv"      ENDOF
      fneg     OF s" hir.fneg"      ENDOF
      fabs     OF s" hir.fabs"      ENDOF
      fsqrt    OF s" hir.fsqrt"     ENDOF
      flt      OF s" hir.flt"       ENDOF
      fgt      OF s" hir.fgt"       ENDOF
      feq      OF s" hir.feq"       ENDOF
      fltz     OF s" hir.fltz"      ENDOF
      feqz     OF s" hir.feqz"      ENDOF
      intreal  OF s" hir.int>real"  ENDOF
      realint  OF s" hir.real>int"  ENDOF
      bitsreal OF s" hir.bits>real" ENDOF
      realbits OF s" hir.real>bits" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

\ The literal's value is the whole content of a constant.
: KEY-VALUE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.value" IR-BUILD:INTERN-SYMBOL ;

\ WHAT the number is, as against what it equals. Two things this dialect stages
\ as an integer literal are addresses, and a number carries no evidence of being
\ one; the elaborator is the last place that still knows, so it records it here.
: KEY-ADDR ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.addr" IR-BUILD:INTERN-SYMBOL ;

\ A small enumeration rather than a flag, so a CODE kind is a new member and not
\ a second attribute.
0 constant ADDR-NONE
1 constant ADDR-DATA
ADDR-DATA constant ADDR-KIND-MAX

\ Refused where the attribute is BUILT rather than where it is read.
: ADDR-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   dup 0 < over ADDR-KIND-MAX > or if E-HIR-ADDR throw then
   IR-BUILD:INTERN-INT-ATTR ;

\ Three keys and not one packed number, because a reader could get the fields of
\ a triple in the wrong order without any authority noticing.
: KEY-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.entry" IR-BUILD:INTERN-SYMBOL ;

: KEY-IN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.in" IR-BUILD:INTERN-SYMBOL ;

: KEY-OUT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.out" IR-BUILD:INTERN-SYMBOL ;

\ An ordinal and not an address, because there is no address yet: where the body
\ lands is the emitter's answer.
: KEY-FUN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.fun" IR-BUILD:INTERN-SYMBOL ;

\ ---- the type of an ordinary value -------------------------------------------
\ One signed 64-bit integer per stack value, which every schema here declares.
: CELL-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ ---- the type of the memory order --------------------------------------------
\ It lives in no register and stands for no number: it is what makes "this load
\ happens after that store" a dependency the module holds.
: MEM-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-DOMAIN:DATA-MEM IR-BUILD:INTERN-TOKEN ;

\ ---- the type of a double ----------------------------------------------------
\ A double and a cell are the same eight bytes and NOT the same value: which of
\ the two decides which register file may hold it. The two crossings compute
\ nothing - they are the same bytes read as the other type.
: REAL-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-FMT:DOUBLE IR-BUILD:INTERN-FLT ;

private

\ ---- the schema definitions --------------------------------------------------
\ Neither is public: IR-SCHEMA:RULE@ and RENDERER@ are the authority.
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
      quot   OF s" hir.rule.quot"   ENDOF
      return OF s" hir.rule.return" ENDOF
      trap   OF s" hir.rule.trap"   ENDOF
      fconst   OF s" hir.rule.fconst"    ENDOF
      fadd     OF s" hir.rule.fadd"      ENDOF
      fsub     OF s" hir.rule.fsub"      ENDOF
      fmul     OF s" hir.rule.fmul"      ENDOF
      fdiv     OF s" hir.rule.fdiv"      ENDOF
      fneg     OF s" hir.rule.fneg"      ENDOF
      fabs     OF s" hir.rule.fabs"      ENDOF
      fsqrt    OF s" hir.rule.fsqrt"     ENDOF
      flt      OF s" hir.rule.flt"       ENDOF
      fgt      OF s" hir.rule.fgt"       ENDOF
      feq      OF s" hir.rule.feq"       ENDOF
      fltz     OF s" hir.rule.fltz"      ENDOF
      feqz     OF s" hir.rule.feqz"      ENDOF
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
      quot   OF s" hir.render.quot"   ENDOF
      return OF s" hir.render.return" ENDOF
      trap   OF s" hir.render.trap"   ENDOF
      fconst   OF s" hir.render.fconst"    ENDOF
      fadd     OF s" hir.render.fadd"      ENDOF
      fsub     OF s" hir.render.fsub"      ENDOF
      fmul     OF s" hir.render.fmul"      ENDOF
      fdiv     OF s" hir.render.fdiv"      ENDOF
      fneg     OF s" hir.render.fneg"      ENDOF
      fabs     OF s" hir.render.fabs"      ENDOF
      fsqrt    OF s" hir.render.fsqrt"     ENDOF
      flt      OF s" hir.render.flt"       ENDOF
      fgt      OF s" hir.render.fgt"       ENDOF
      feq      OF s" hir.render.feq"       ENDOF
      fltz     OF s" hir.render.fltz"      ENDOF
      feqz     OF s" hir.render.feqz"      ENDOF
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
   c b KEY-ADDR IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:CONST NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Each one's may-trap flag is the compilation unit's overflow policy.
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

\ The one arithmetic operation that may trap whatever the policy says: the
\ policy is about OVERFLOW, and the engine's `/` traps on a zero divisor.
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

\ Two cells in, one out, pure and TOTAL: none of the eleven can overflow, so
\ declaring them through DEF-BINARY would oblige the machine stage to reproduce
\ a trap that cannot happen.
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

\ One cell in, one cell out, which is what stops a caller staging it with two.
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
\ The address is a value the program computed, so it may name any cell it can
\ reach: the generic space with unrestricted aliasing.
: GENERIC-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

\ Pure, which is what lets it carry a token result with no token operand: there
\ is nothing before it to take one from.
: DEF-MEM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b HIR-OPCODE:MEM OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:MEM NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The token is the LAST operand and the LAST result of both forms, which is why
\ elaborate.f finds it by TYPE rather than by position.
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

\ Forth writes `value address !`, so the value is the deeper of the two and
\ therefore the first operand.
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

\ The value it answers is the byte widened into a cell: a byte is not a type of
\ this dialect, it is a width of an access.
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

\ Only the value's lowest byte reaches memory; the operand is still a cell.
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

\ A terminator's operands are the successor's block arguments, so how many
\ there are is a property of the destination and the list is one variadic tail.
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

\ Its one operand is the value it tests and not a block argument: with two
\ successors nothing could say which operand belongs to which destination.
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

\ A word's output count is a property of the word and not of the opcode, so the
\ operand list is one variadic cell.
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

\ Its one operand is the family ORDINAL, because a name cannot be a pointer -
\ the type-family string pool is grown by doubling and moves. It carries no
\ address, because there is exactly one trap routine tree-wide.
: DEF-TRAP ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b HIR-OPCODE:TRAP OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:TRAP NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Operands are the memory order and EVERY live value, results the order and
\ those values again: no register survives the call, so the call CONSUMES each
\ value and ANSWERS it, which is the two lifetimes the allocator really has.
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

\ A second operation and not a field, because a self-call goes to a LABEL and
\ this goes to an ADDRESS the module has to carry. The save discipline is
\ unchanged, so it assumes nothing about a callee this compiler did not produce.
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

\ One cell out, because a schema names ONE result type and the tree's
\ quotations have four different signatures; which routine it is rides in an
\ attribute, as the callee's ORDINAL, because the body is not emitted yet.
: DEF-QUOT ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b HIR-OPCODE:QUOT OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-FUN IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   TARGET
   c b HIR-OPCODE:QUOT NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the float forms ---------------------------------------------------------
\ The value is the literal's own bit pattern, because the cell IS the double.
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

\ ALL FOUR ARE TOTAL, including the division: a float division by zero answers
\ an infinity and zero over zero the default NaN, neither of which is a trap.
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

\ The square root of a negative is the default NaN rather than a raise.
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

\ A CELL out and not a double: a flag in the floating file is one no branch of
\ this machine can read. Total, because comparing against a NaN answers false.
: DEF-FCOMPARE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id t:IR-ID:ir-type-id
      o:HIR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   FP-TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The zero is not an operand because the INSTRUCTION does not take one: FCMP
\ has a form whose second operand is the immediate zero.
: DEF-FCOMPARE0 ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id t:IR-ID:ir-type-id
      o:HIR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   false IR-SCHEMA:SET-TRAP
   FP-TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Four and not two, because two of them compute and two do not: the rounding
\ pair rounds, and the bit pair is the same eight bytes read as the other type.
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
\ The table's dialect name and version are fixed when the module is created, so
\ reading them back off the live module decides whose table it is.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  NAME IR-BUILD:SYMBOL-IS?
   0= if E-HIR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MAJOR@ MAJOR <> if E-HIR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MINOR@ MINOR <> if E-HIR-DIALECT throw then ;

public

\ ---- creation and registration -----------------------------------------------
\ It adds the dialect's own name and schema version, which no caller spells.
: NEW-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   NAME MAJOR MINOR IR-BUILD:NEW-BUILDER ;

\ Definition is one opcode at a time, so a refusal leaves the opcodes already
\ defined and defines no more.
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
   c b t DEF-QUOT
   c b t DEF-RETURN
   c b t DEF-TRAP
   c b REAL-TYPE {: f:IR-ID:ir-type-id :}
   c b f DEF-FCONST
   c b f HIR-OPCODE:FADD DEF-FBINARY
   c b f HIR-OPCODE:FSUB DEF-FBINARY
   c b f HIR-OPCODE:FMUL DEF-FBINARY
   c b f HIR-OPCODE:FDIV DEF-FBINARY
   c b f HIR-OPCODE:FNEG DEF-FUNARY
   c b f HIR-OPCODE:FABS DEF-FUNARY
   c b f HIR-OPCODE:FSQRT DEF-FUNARY
   c b f t HIR-OPCODE:FLT DEF-FCOMPARE
   c b f t HIR-OPCODE:FGT DEF-FCOMPARE
   c b f t HIR-OPCODE:FEQ DEF-FCOMPARE
   c b f t HIR-OPCODE:FLTZ DEF-FCOMPARE0
   c b f t HIR-OPCODE:FEQZ DEF-FCOMPARE0
   c b t f HIR-OPCODE:INTREAL DEF-CROSS
   c b f t HIR-OPCODE:REALINT DEF-CROSS
   c b t f HIR-OPCODE:BITSREAL DEF-CROSS
   c b f t HIR-OPCODE:REALBITS DEF-CROSS ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
