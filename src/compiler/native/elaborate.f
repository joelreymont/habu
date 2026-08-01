\ elaborate.f - the straight-line elaborator: it walks one sealed source tape and
\ builds the operations of a colon definition into a module under construction.
\
\ docs/compiler-ir-design.md section 7.2 with section 7.12's definition
\ transaction. This is the step between "capture source tape" and "freeze HIR":
\ everything before it records what the compiler read, everything after it reads
\ what the compiler built, and this file is the only place that turns one into
\ the other. It is the first pass of the native chain that translates a program.
\
\ WHAT IT TRANSLATES. One colon definition of the straight-line subset: the
\ defined name and a body of integer literals, modeled arithmetic words,
\ compile-time stack renames, the structured control words, the two cell-width
\ memory words, and one `{: … :}` group of typed locals read by name. Nothing
\ else.
\
\ THE UNIT IS THE DEFINITION, AND THAT IS WHY THERE IS NO FRAME TO FIND. The
\ tape this pass reads is produced by src/compiler/native/feed.f from the
\ checker's own reader, and that reader never sees a definition frame: the
\ engine hands the check hook the definition it reconstructed - name, declared
\ signature, body - with the opening `:` and the closing `;` already consumed.
\ So there is no `:` row to match and no `;` row to stop at, and there never
\ will be. What the tape does record is the parser mode each token was consumed
\ in, and that draws the boundary exactly: `:` parses the defined name from the
\ outer interpreter before it switches the parser to compiling, so the name is
\ the one token of a definition read while INTERPRETING and every token of the
\ body was read while COMPILING. This pass therefore reads the first row as the
\ name, walks the rest as the body, and ends the body where the tape ends. It
\ holds no spelling of its own: a program whose compiler spells its definition
\ frame differently produces the same tape and elaborates the same way.
\
\ THE COMPILE-TIME VALUE VECTOR IS THE WHOLE IDEA. Design section 7.3 keeps a
\ vector of the values the data stack holds at each point of the body, and says
\ that `DUP`, `DROP`, `SWAP` and `OVER` "produce no SIR operation and therefore
\ no runtime instruction". This file is where that stops being a plan: a rename
\ reorders the vector and stages nothing, so RENAME below does not even take a
\ builder. `: SQUARE ( n -- n ) dup * ;` becomes exactly two operations - one
\ multiply and one return - and the multiply's two operands are the same value.
\ That is the measured difference from the old emitter, which spends an
\ instruction on every stack word.
\
\ WHERE THE WORD'S INPUTS COME FROM. The entry block's arguments are the word's
\ declared inputs, one value each, and they are the vector's contents when the
\ body starts. When the body ends the vector must hold exactly the declared
\ outputs, and they become the operands of `hir.return` bottom first - the order
\ the caller's stack has them.
\
\ WHAT THIS PASS IS TOLD RATHER THAN READS. Two facts come in as arguments: how
\ many values the word takes and how many it leaves. They belong to the checker's
\ accepted stack effect, which section 7.2 requires the elaborated operations to
\ correspond to. The checker knows them - it parses the declared signature during
\ the very scan this tape was recorded from - but it publishes them only through
\ a name lookup into its live effect store, which answers about whatever word
\ carries that name now rather than about the definition this tape is. Binding
\ them to the recorded unit is the frozen checker environment's work, dot
\ habu-bind-checker-env-ed4f9f87, reached through habu-bind-the-colon-ea509e61.
\ Until that lands the caller states the arity at ONE seam, COLON's last two
\ arguments, and the elaborator checks the body against it - which is why a body
\ that leaves the wrong number of values is refused here rather than discovered
\ later. One more fact is this file's own for the same reason: a definition
\ compiles as exported, while whether it is visible outside its package is the
\ package system's fact, and the same dot moves it. Linkage and convention need
\ no such dot: a colon definition is a definition of this module and it is called
\ the Habu way.
\
\ WHY IT ASKS THREE AUTHORITIES AND OWNS NONE OF THEM. The tape says what the
\ tokens are and which mode each was read in; src/compiler/native/hir-word.f says
\ what each body word means, and for a rename says exactly which values it puts
\ back; src/compiler/ir/build.f's schema readers say how many operands an opcode
\ takes and how many results it defines. This file repeats none of those facts.
\ It owns the value vector and the shape of a definition on a tape - and nothing
\ else.
\
\ THE PARSER MODE IS CHECKED, NOT DECORATION. The mode is what the frame reading
\ above rests on, so it is verified at every row and not sampled: the first row
\ must say interpreting and every later row must say compiling. A tape with a
\ second interpreting row is a tape of something other than one definition, and
\ it is refused loudly rather than compiled into something else.
\
\ WHAT A REFUSAL LEAVES BEHIND. A refused elaboration leaves the builder holding
\ whatever function, block and operation stages it had opened. That is the
\ builder's own fail-closed state: FREEZE refuses a module with an open stage
\ with E-IR-BUILD-OPEN, so nothing half-built can be published, and the caller
\ gives the module up with IR-BUILD:ABORT, which returns every stage to its
\ authority. Unwinding them here would need to catch and rethrow, and a checked
\ `catch` takes a stack-neutral quotation, so it could not carry the handles it
\ would need to unwind with.
\
\ ONE DEFINITION AT A TIME. The value vector is a fixed package-owned array
\ rather than a heap object, so this pass compiles one definition at a time -
\ which is the single-task compilation discipline the rest of the compiler
\ already keeps. It is not a staging protocol with a begin and an end: the whole
\ walk is one call, so the vector is emptied when that call starts and nothing a
\ refused call left behind can be read by the next one.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/type.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/tape.f
require src/compiler/native/hir.f
require src/compiler/native/hir-word.f
require src/compiler/native/frozen.f

package NELAB
private

\ ---- what one elaboration is working on --------------------------------------
\ The context, the builder, the sealed tape and the module key, held for the
\ length of one call. Everything the control words below do needs all four, and a
\ word that took them as arguments would take four cells to ask one question.
\ This is the same package-owned staging every other pass of the native chain
\ keeps under the single-task compilation discipline: COLON writes all four
\ before it reads anything, so an elaboration abandoned by a refusal leaves
\ nothing a later one could read.
1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-VW IR-ARENA:view
1 TYPED-BUFFER S-KEY IR-ID:ir-module-key

: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: VW ( -- IR-ARENA:view )            0 S-VW @ ;
: MKEY ( -- IR-ID:ir-module-key )    0 S-KEY @ ;

\ ---- the compile-time value vector -------------------------------------------
\ How deep the data stack may get inside one straight-line body. Sixty-four is
\ far past anything hand-written Forth reaches; a body that wants more is a
\ capability to raise here, not a ceiling to widen silently.
64 constant VMAX

here CELL 1- and CELL swap - CELL 1- and allot
variable VN                          \ how many values the vector holds
VMAX TYPED-BUFFER VSTK IR-ID:ir-value-id
VMAX TYPED-BUFFER VWIN IR-ID:ir-value-id

: VRESET ( -- )
   0 VN ! ;

: VPUSH ( IR-ID:ir-value-id -- )
   {: val:IR-ID:ir-value-id :}
   VN @ VMAX >= if E-NELAB-CAP throw then
   val VN @ VSTK !
   VN @ 1+ VN ! ;

\ The i-th value from the bottom. Every reader of the vector goes through here,
\ so an index outside what the vector holds is one refusal rather than several.
: VAT ( n -- IR-ID:ir-value-id )
   {: i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   i VSTK @ ;

: VDROP ( n -- )
   {: k:n :}
   k 0 < k VN @ > or if E-NELAB-UNDER throw then
   VN @ k - VN ! ;

\ ---- compile-time stack renames ----------------------------------------------
\ The consumed window is copied aside before the vector is shortened, because a
\ rename may put a value back below where it read it from: `swap` writes its
\ first pick over the cell its second pick reads. The window is as large as the
\ vector, so a rename that fits in the vector fits in the window, and there is no
\ second ceiling to keep in step with the word model's.
\
\ A pick names its input by depth in the consumed window with zero being the top,
\ so the value it names sits at window position in-1-depth. Picks are listed
\ bottom first, which is the order they are pushed.
: RENAME ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena sym:IR-ID:ir-symbol-id :}
   r sym HIR-WORD:INPUTS@ {: in:n :}
   r sym HIR-WORD:PICKS {: picks:n :}
   in VN @ > if E-NELAB-UNDER throw then
   VN @ in - {: base:n :}
   in 0 ?do
      base i + VAT  i VWIN !
   loop
   in VDROP
   VN @ picks + VMAX > if E-NELAB-CAP throw then
   picks 0 ?do
      in 1- p r sym i HIR-WORD:PICK@ -  VWIN @  VPUSH
   loop ;

\ ---- the names a `{: … :}` group binds ---------------------------------------
\ A typed local is a named SSA VALUE and nothing else. It is bound once, when
\ the group closes, to a value that is already on the compile-time vector, and
\ every later mention of the name puts that same value back on the vector. So a
\ local needs no memory, no frame slot and no operation of any dialect: reading
\ one is exactly as free as `dup` is, and the value it names is what travels
\ across a branch or a loop edge, carried as a block argument like every other
\ live value. That is why nothing downstream of this file learns a new concept.
\
\ WHAT THE TAPE REALLY CARRIES, WHICH IS WHERE THE SHAPE COMES FROM. The
\ engine's own reader consumes `{:`, then one token per declared local spelled
\ `name:type`, then `:}` - test/compiler/native-feed.f records exactly that grid
\ off a real compilation, so this file reads the shape the producer makes rather
\ than one it hopes for. The body then spells the bare name, so the annotation
\ is cut off when the name is declared; where it is cut off is
\ src/compiler/native/hir-word.f's LOCAL-NAME-LEN, because this file holds no
\ spelling of its own.
\
\ WHY THE GROUP IS FOUND BEFORE THE WALK. Two walks read the body - the skeleton
\ that counts blocks and the build that makes them - and both of them meet
\ tokens that are neither dialect words nor literals: the declared names, and
\ every later mention of one. Asking the word model about either is a refusal,
\ so both walks have to know which rows are the group and which names are
\ locals before they start. The pre-pass answers both by recording the group's
\ two ENDS as tape indices, so the two walks share one derivation of where the
\ group is instead of each keeping a state machine that could drift; and the
\ build checks its own arrival at the closer against the index the pre-pass
\ recorded, which is the same two-derivations discipline SKELETON keeps.
\
\ ONE GROUP, AT THE TOP LEVEL, READ-ONLY. That is what the corpus needs and it
\ is all that is built: a second group in one definition, a group inside a
\ control structure and an unclosed group are refused by name. Rebinding a local
\ and taking its address need no refusal here at all - no such word is in the
\ dialect's vocabulary, so `to` and `^` are already refused as words this
\ dialect cannot compile. Dots habu-rebind-a-typed-b2a3e369 and
\ habu-take-the-addr-18a38b4f carry the two capabilities.
16 constant LMAX                     \ locals one definition may declare
64 constant LNAME-CAP                \ bytes one declaration spelling may hold

here CELL 1- and CELL swap - CELL 1- and allot
variable LN                          \ how many locals were declared
variable LG-FROM                     \ the tape row the `{:` is on, or -1
variable LG-TO                       \ the tape row the `:}` is on, or -1
variable LBOUND                      \ whether the group has closed and bound
LMAX TYPED-BUFFER LNAME IR-ID:ir-symbol-id
LMAX TYPED-BUFFER LVAL IR-ID:ir-value-id
create LBUF LNAME-CAP allot

: LRESET ( -- )
   0 LN !
   -1 LG-FROM !
   -1 LG-TO !
   0 LBOUND ! ;

: LAT ( n -- n )
   dup 0 < over LN @ >= or if E-NELAB-LOCAL throw then ;

\ Is this tape row part of the declaration - the opener, or one of the names
\ after it? The closer is not: it is the row that does the binding.
: IN-DECL? ( n -- bool )
   {: ix:n :}
   LG-FROM @ 0 <  if false exit then
   ix LG-FROM @ >=  ix LG-TO @ <  and ;

\ Which declared local this row names, or a negative answer. The comparison is
\ between interned identities of one module, so it is an identity question and
\ not a search for text.
: LOCAL-OF ( n -- n )
   {: ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if -1 exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   -1
   LN @ 0 ?do
      sy i LNAME @ NFROZEN:SAME-SYM? if drop i leave then
   loop ;

\ ---- the definition's memory order -------------------------------------------
\ A load and a store say what a definition does to memory; the order they do it
\ in is a value they pass along, and this is where the walk keeps the one it has
\ reached. It is minted lazily, by the first body word whose operation takes it,
\ so a definition that touches no memory contains no operation for it and the
\ modules the other corpus words compile to are unchanged.
\
\ ONE ORDER PER DEFINITION, AND IT TRAVELS LIKE EVERY OTHER LIVE VALUE. The order
\ is an SSA value, so the way it reaches a block that control can arrive at twice
\ is the way every other live value reaches one: as a BLOCK ARGUMENT. TERM-BR
\ hands it over with the rest of the vector and OPEN-ARGS takes it back, so a
\ loop body's load reads the order the previous turn left and the block after a
\ branch reads whichever arm ran. Nothing else in the chain learns a new concept:
\ the token has a type, a class and an allocation rule already.
\
\ WHY IT IS MINTED AT ENTRY AND NOT AT THE FIRST MEMORY WORD. Minting lazily -
\ where the first memory word happens to be - is what made a memory word inside a
\ structure impossible: the order would be defined in a block that does not
\ dominate the loop header the next turn reads it through, and the freeze
\ verifier refused it by name. A value handed across an edge has to exist BEFORE
\ the edge, so the order is minted in the entry block, which dominates every
\ block of the definition. It is still minted only when the definition needs one:
\ MEM-SCAN below reads the body once and asks the schema table whether any word
\ of it takes an order, so a definition that touches no memory contains no
\ operation for it, carries no extra block argument, and compiles to exactly the
\ module it compiled to before.
\
\ WHAT "CONSUMED EXACTLY ONCE" MEANS ONCE THERE ARE EDGES. A two-way branch
\ carries no values, so both of its successors read the order the block above
\ them left - two USES of one value. They are not two consumptions: only one of
\ the two blocks runs. The rule the allocation validator keeps is therefore
\ per-path rather than per-module, and src/compiler/native/regalloc-verify.f
\ states and checks it.
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
variable TOK-LIVE                    \ whether an order has been minted yet
variable TOK-NEED                    \ whether the body has a word that takes one
variable OPJ                         \ general operands taken so far by the open staging

: TOK-RESET ( -- )
   0 TOK-LIVE !
   0 TOK-NEED ! ;

: TOK ( -- IR-ID:ir-value-id )
   0 S-TOK @ ;

: TOK! ( IR-ID:ir-value-id -- )
   0 S-TOK !
   1 TOK-LIVE ! ;

\ ---- staging one operation ---------------------------------------------------
\ The cell type of this subset: one signed 64-bit integer per stack value. The
\ dialect declares the same type in every schema it registers, and this is the
\ same interned identity because interning deduplicates - and if the two ever
\ disagreed, IR-OP:END-OP would refuse the first operation whose operand type is
\ not the one its schema declares, so the restatement is checked rather than
\ trusted.
: CELL-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ Every operation this pass stages carries the span of the token that produced
\ it, which is what makes a later diagnostic point at source the programmer
\ wrote. The span comes off the tape, so it is the real byte range and not a
\ placeholder.
: OPEN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      ix:n op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:BEGIN-OP
   c b  v key ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN ;

\ ---- which positions of a form carry the order --------------------------------
\ A general value comes off the compile-time vector and the memory order comes
\ off the slot above; which position is which is the schema table's answer, read
\ by TYPE and not by position, so this file assumes nothing about where a
\ dialect puts its token. One order at a time is this file's own limit and it is
\ stated as a refusal: a form declaring two of them would need two orders, and
\ there is no rule here for that. That refusal is fail-closed rather than
\ reachable and says so - no schema of this dialect declares two - but a check
\ that only looks at the shapes it expects to see is not a check, and the walk
\ below would otherwise hand one order over twice in silence.
: TOKEN? ( IR-ID:ir-type-id -- bool )
   {: t:IR-ID:ir-type-id :}
   t  CTX BLD HIR:MEM-TYPE  NFROZEN:SAME-TYPE? ;

: TOKEN-CK ( n -- n )
   dup 1 > if E-NELAB-TOKEN throw then ;

: TOKEN-OPERANDS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-OPERANDS {: k:n :}
   0
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-OPERAND@ TOKEN? if 1+ then
   loop
   TOKEN-CK ;

: TOKEN-RESULTS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   0
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-RESULT@ TOKEN? if 1+ then
   loop
   TOKEN-CK ;

\ The operands the opcode's schema declares. A general operand is taken off the
\ vector, deepest first, so `-` on a stack holding a then b subtracts b from a,
\ exactly as the source reads; the memory order is not on the vector and is
\ handed over from the slot instead. The count is the schema's fixed operand
\ list: no source word of this subset binds to an opcode with a variadic tail,
\ and a word model that bound one would be refused downstream by name - IR-OP
\ measures a staged operation against the same schema, and IR-FUN refuses a
\ terminator that is not the block's last operation.
: OPERANDS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-OPERANDS {: k:n :}
   k  c b op TOKEN-OPERANDS  - {: v:n :}
   v VN @ > if E-NELAB-UNDER throw then
   VN @ v - {: base:n :}
   0 OPJ !
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-OPERAND@ TOKEN? if
         c b TOK IR-BUILD:ADD-OPERAND
      else
         c b  base OPJ @ + VAT  IR-BUILD:ADD-OPERAND
         OPJ @ 1+ OPJ !
      then
   loop
   v VDROP ;

: RESULTS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op TOKEN-RESULTS drop
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-RESULT@ TOKEN? if
         c b  c b HIR:MEM-TYPE  IR-BUILD:ADD-RESULT
      else
         c b  c b CELL-TYPE  IR-BUILD:ADD-RESULT
      then
   loop ;

\ Close the operation and keep what it defined: a general value goes on the
\ vector and the memory order goes in the slot, so the next access threads this
\ one's answer. The values are the operation's own, read back off its row, so
\ nothing here has to know which value ordinals the store happened to mint.
: CLOSE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-RESULT@ TOKEN? if
         c b id i IR-BUILD:OP-RESULT@ TOK!
      else
         c b id i IR-BUILD:OP-RESULT@ VPUSH
      then
   loop ;

\ ---- the things a body token becomes -----------------------------------------
\ One integer literal, staged at the span of the token named. The value is the
\ whole content of a constant, so it rides as the attribute the opcode's schema
\ requires. It takes the value rather than reading it off the token, because a
\ constant-and-operation word's constant is the word model's and not the tape's.
: EMIT-LIT ( n n -- )
   {: ix:n val:n :}
   CTX BLD HIR-OPCODE:CONST HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-VALUE  CTX BLD val IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE ;

\ The memory the definition is entered with, staged at the span of the token that
\ first needed it. It takes nothing and answers the order every later access
\ threads.
: EMIT-MEM ( n -- )
   {: ix:n :}
   CTX BLD HIR-OPCODE:MEM HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD op CLOSE ;

\ An operation that takes an order has to find one. It is minted in the entry
\ block by COLON, before the walk starts, when MEM-SCAN saw that this body needs
\ one - so reaching a memory word with no order live means the pre-scan and the
\ walk disagree about what the body contains, and that is refused rather than
\ patched up by minting one here in whatever block the walk has reached.
: TOKEN-READY ( IR-ID:ir-symbol-id -- )
   {: op:IR-ID:ir-symbol-id :}
   CTX BLD op TOKEN-OPERANDS 0= if exit then
   TOK-LIVE @ 0= if E-NELAB-TOKEN throw then ;

\ One operation of this dialect, staged at the span of the token named. How many
\ operands it takes off the vector and how many results it puts back is the
\ schema table's answer, never this file's.
: EMIT-OPCODE ( n HIR:opcode -- )
   {: ix:n k:HIR:opcode :}
   CTX BLD k HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   op TOKEN-READY
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD op CLOSE ;

\ An integer literal the tape carries.
: EMIT-CONST ( n -- )
   {: ix:n :}
   ix  VW ix NTAPE:LIT@  EMIT-LIT ;

\ A word the dialect has an operation for. Which operation is the word model's
\ answer.
: EMIT-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix  r  VW MKEY ix NTAPE:SPELL@  HIR-WORD:OPCODE@  EMIT-OPCODE ;

\ A word that pushes one fixed value - the address a `create`d data word names.
\ The value is the word model's, so this stages the same operation an integer
\ literal in the source would.
: EMIT-FIXED ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix  r  VW MKEY ix NTAPE:SPELL@  HIR-WORD:FIXED-VALUE@  EMIT-LIT ;

\ A word that is one constant and one operation - `1-` is `1` then `-`. Both
\ halves come off the word model's row, so a second opcode meaning the same
\ thing is not needed and the source stays one token.
: EMIT-CONST-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:CONST-VALUE@  EMIT-LIT
   ix  r sy HIR-WORD:CONST-OPCODE@  EMIT-OPCODE ;

\ Leaving the word. The outputs are the whole vector, bottom first, and the
\ vector has to hold exactly as many as the word declares - one too few or one
\ too many is a body that does not match its effect, and it is refused here
\ rather than turned into a return of the wrong width. `hir.return` declares a
\ variadic operand tail, so the count is the word's and not the opcode's. The
\ return has no token of its own on a produced tape - the `;` that used to carry
\ it was consumed before the checker read anything - so it answers for the span
\ of the definition's name, which is the definition itself.
: EMIT-RETURN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      out:n :}
   VN @ out <> if E-NELAB-ARITY throw then
   c b HIR-OPCODE:RETURN HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b v key 0 op OPEN
   out 0 ?do
      c b  i VAT  IR-BUILD:ADD-OPERAND
   loop
   out VDROP
   c b IR-BUILD:END-OP drop ;

\ ---- the blocks a definition is made of --------------------------------------
\ Blocks are minted by IR-BUILD in the order they are closed, so the ordinal of
\ the block being built now is the number of blocks closed before it. Every
\ successor a terminator names is one of those ordinals, and a terminator names
\ its successors when it is built - which is why the join a structure branches
\ forward to is worked out before the walk starts, by SKELETON below.
variable NB                          \ blocks closed so far; also the open block's ordinal

\ ---- leaving from the middle of a definition ---------------------------------
\ `exit` leaves the word from wherever it is written, and a definition has ONE
\ place control leaves through: the block that holds `hir.return`. So an `exit`
\ is a branch to that block, handing it the values the word leaves - exactly
\ what the fall-through at the end of the body does. A definition that contains
\ an `exit` therefore gets a block of its own for the return, taking the outputs
\ as its arguments; a definition without one keeps the shape it had, where the
\ return is the last operation of the last block.
\
\ WHY ONE RETURN BLOCK AND NOT TWO RETURNS. Two blocks ending in `hir.return`
\ would be two places control leaves, and everything downstream is written
\ against one: the register allocator finds the routine's exit by looking for the
\ block with no successor and refuses a module with two, because a convention
\ that says where a result is left has nowhere to leave it twice. Branching to a
\ shared exit block is the ordinary structured answer and needs no new concept -
\ the outputs cross the edge as block arguments, like every other live value.
\
\ WHAT AN `exit` MAY NOT DO YET, AND IT IS REFUSED BY NAME. It has to be the
\ last word of the `if` arm it is in. The words after it would be unreachable,
\ and this elaborator has no way to say "unreachable" - it would have to invent a
\ block with no predecessor and values for its arms to hand on. So `exit` outside
\ an `if`, or with anything but `then` after it, is E-NELAB-CTRL, and dot
\ habu-exit-from-anywhere-in-a-body carries the general case.
variable OUT-N                       \ values the definition leaves
variable EXIT-USED                   \ whether the body has an `exit` at all
variable EXIT-ORD                    \ the block every `exit` and the fall-through reach
variable EXIT-PENDING                \ an `exit` closed the arm; only its `then` may follow

: EXIT-RESET ( -- )
   0 EXIT-USED !
   -1 EXIT-ORD !
   0 EXIT-PENDING ! ;

: BLOCK-ORD ( n -- IR-ID:ir-block-id )
   {: k:n :}
   k 0 < k NFROZEN:BMAX >= or if E-NELAB-BLOCK throw then
   MKEY k IR-ID:PACK-BLOCK ;

: CLOSE-BLOCK ( -- )
   CTX BLD IR-BUILD:END-BLOCK drop
   NB @ 1+ {: k:n :}
   k NFROZEN:BMAX > if E-NELAB-BLOCK throw then
   k NB ! ;

\ A block that takes its live values as arguments. Every value the vector held
\ is handed over by the branch that reached it, so the vector is replaced by the
\ arguments: a join is the one place where two different definitions of "the
\ value in this stack slot" meet, and a block argument is what SSA calls that.
\
\ The memory order is the LAST argument when the definition has one. It is not on
\ the value vector - Forth's data stack does not hold it - but it is live across
\ the edge for exactly the same reason the vector's values are, so it crosses the
\ same way and by the same mechanism. Putting it last is this file's convention
\ and the one TERM-BR hands the operands in, so the two always line up; the
\ verifier matches a terminator's operands against the destination's arguments
\ position by position and would refuse them if they did not.
: OPEN-ARGS ( n n -- )
   {: ix:n n:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   n 0 ?do
      CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG VPUSH
   loop
   TOK-LIVE @ 0<> if
      CTX BLD  CTX BLD HIR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG TOK!
   then ;

\ A block that takes no arguments and keeps the vector it inherits. Its only
\ predecessor is the two-way branch just above it, and a two-way branch hands
\ nothing over, so every value the vector holds was defined in a block that
\ dominates this one and may be read here by name. That is the dominance rule
\ the freeze verifier already enforces, not a licence this file takes.
: OPEN-PLAIN ( n -- )
   {: ix:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN ;

\ Hand the whole value vector to one block and end this one. The operands are
\ the vector bottom first, which is the order the destination's arguments are in,
\ and the memory order last when the definition has one - the position OPEN-ARGS
\ gives it.
: TERM-BR ( n n -- )
   {: ix:n t:n :}
   CTX BLD  CTX BLD HIR-OPCODE:BR HIR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN
   VN @ 0 ?do
      CTX BLD  i VAT  IR-BUILD:ADD-OPERAND
   loop
   TOK-LIVE @ 0<> if
      CTX BLD TOK IR-BUILD:ADD-OPERAND
   then
   CTX BLD  t BLOCK-ORD  IR-BUILD:ADD-SUCCESSOR
   CTX BLD IR-BUILD:END-OP drop
   CLOSE-BLOCK ;

\ Test the top value and end this block: the first successor when it is zero and
\ the second when it is not. The tested value leaves the vector, because the
\ question has been asked and nothing downstream reads the answer again.
: TERM-BRZ ( n n n -- )
   {: ix:n z:n o:n :}
   VN @ 1- VAT {: f:IR-ID:ir-value-id :}
   1 VDROP
   CTX BLD  CTX BLD HIR-OPCODE:BRZ HIR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN
   CTX BLD f IR-BUILD:ADD-OPERAND
   CTX BLD  z BLOCK-ORD  IR-BUILD:ADD-SUCCESSOR
   CTX BLD  o BLOCK-ORD  IR-BUILD:ADD-SUCCESSOR
   CTX BLD IR-BUILD:END-OP drop
   CLOSE-BLOCK ;

\ A whole block that does nothing but hand the live values on. Every edge that
\ leaves a two-way branch and has to carry values goes through one of these,
\ because a two-way branch carries none: that is ordinary critical-edge
\ splitting, and it is what makes the arms of a structure agree at their join.
: STUB ( n n -- )
   {: ix:n t:n :}
   ix OPEN-PLAIN
   ix t TERM-BR ;

\ ---- the open control structures ----------------------------------------------
\ One frame per structure the walk is inside. `depth` is how deep the value
\ vector was when the structure opened, which is what every arm has to leave it
\ at; `join` is the block the structure's paths meet in, or - for `begin` - the
\ header they go back to; `head` is the counted loop's header; and the index and
\ the limit are the counted loop's own two values, which live here rather than on
\ the value vector because Forth's loop parameters are not on the data stack.
32 constant CMAX

here CELL 1- and CELL swap - CELL 1- and allot
variable CS-N
CMAX TYPED-BUFFER CS-KIND HIR:ctrl
create CS-DEPTH CMAX cells allot
create CS-JOIN CMAX cells allot
create CS-HEAD CMAX cells allot
CMAX TYPED-BUFFER CS-IDX IR-ID:ir-value-id
CMAX TYPED-BUFFER CS-LIM IR-ID:ir-value-id

: CS-RESET ( -- )
   0 CS-N ! ;

: CS-AT ( n -- n )
   dup 0 < over CS-N @ >= or if E-NELAB-CTRL throw then ;

: CS-TOP ( -- n )
   CS-N @ 1- CS-AT ;

: CS-PUSH ( HIR:ctrl n n -- )
   {: k:HIR:ctrl d:n j:n :}
   CS-N @ CMAX >= if E-NELAB-BLOCK throw then
   CS-N @ {: t:n :}
   k t CS-KIND !
   d t cells CS-DEPTH + !
   j t cells CS-JOIN + !
   t 1+ CS-N ! ;

: CS-POP ( -- )
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-N @ 1- CS-N ! ;

\ The closer has met the opener it belongs to. A `then` over an open `begin`, or
\ a `loop` with nothing open at all, is refused here and named as what it is.
: CS-OPENER-CK ( HIR:ctrl -- n )
   {: want:HIR:ctrl :}
   CS-TOP {: t:n :}
   t CS-KIND @ want HIR-CTRL:EQ 0= if E-NELAB-CTRL throw then
   t ;

: CS-DEPTH@ ( n -- n )    cells CS-DEPTH + @ ;
: CS-JOIN@ ( n -- n )     cells CS-JOIN + @ ;
: CS-HEAD@ ( n -- n )     cells CS-HEAD + @ ;

\ ---- reading the definition frame --------------------------------------------
: NAME-CK ( IR-ARENA:view n -- )
   NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-SHAPE throw then ;

: MODE-CK ( IR-ARENA:view n NTAPE:mode -- )
   {: v:IR-ARENA:view ix:n want:NTAPE:mode :}
   v ix NTAPE:MODE@ want NTAPE-MODE:EQ 0= if E-NELAB-MODE throw then ;

\ ---- finding the locals group ------------------------------------------------
\ One walk of the body before either of the other two, recording where the group
\ is and which names it declares. It asks the word model only about rows the
\ model could answer for: MODELS? is the one reader here that treats an
\ undeclared word as an ordinary answer rather than a refusal, which is exactly
\ what a name the program chose is.
: MODELED-AS? ( IR-ARENA:arena n HIR:meaning -- bool )
   {: r:IR-ARENA:arena ix:n m:HIR:meaning :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ m HIR-MEANING:EQ ;

: DUP-LOCAL? ( IR-ID:ir-symbol-id -- bool )
   {: sy:IR-ID:ir-symbol-id :}
   false
   LN @ 0 ?do
      sy i LNAME @ NFROZEN:SAME-SYM? or
   loop ;

\ One declared local: its bare name, interned into this module so that every
\ later mention of it in the body is the same identity. The annotation is cut
\ off by the word model, which owns how a source word of this dialect is
\ spelled. A name the dialect already models is refused rather than allowed to
\ shadow it: `{: i:n :}` inside a counted loop would otherwise make `i` mean two
\ things, and which one it means is a rule this file has no business inventing
\ (dot habu-decide-what-a-9f38a8f6).
: DECLARE-LOCAL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   LN @ LMAX >= if E-NELAB-LOCAL-CAP throw then
   CTX BLD  VW MKEY ix NTAPE:SPELL@  LBUF LNAME-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   LBUF u HIR-WORD:LOCAL-NAME-LEN {: nu:n :}
   nu 1 < if E-NELAB-LOCAL throw then
   CTX BLD LBUF nu IR-BUILD:INTERN-SYMBOL {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? if E-NELAB-LOCAL throw then
   sy DUP-LOCAL? if E-NELAB-LOCAL throw then
   sy LN @ LNAME !
   LN @ 1+ LN ! ;

\ One row of the pre-pass. Before the group, the only row that matters is an
\ opener; inside it, every row is a declared name until the closer; after it, a
\ second opener is refused, because one group per definition is what this
\ elaborator binds.
: SCAN-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   LG-FROM @ 0 < if
      r ix HIR-MEANING:OPEN-LOCALS MODELED-AS? if ix LG-FROM ! then exit
   then
   LG-TO @ 0 >= if
      r ix HIR-MEANING:OPEN-LOCALS MODELED-AS? if E-NELAB-LOCAL throw then exit
   then
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-LOCAL throw then
   r ix HIR-MEANING:CLOSE-LOCALS MODELED-AS? if ix LG-TO ! exit then
   r ix HIR-MEANING:OPEN-LOCALS MODELED-AS? if E-NELAB-LOCAL throw then
   r ix DECLARE-LOCAL ;

: LOCALS-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   LRESET
   n 1 ?do
      r i SCAN-STEP
   loop
   LG-FROM @ 0 >=  LG-TO @ 0 <  and if E-NELAB-LOCAL throw then ;

\ ---- does this definition touch memory at all? -------------------------------
\ One walk of the body, before the blocks are counted, asking the SCHEMA TABLE
\ whether any word of it stages an operation that takes a memory order. It asks
\ the table rather than listing the memory words, so a form added to the dialect
\ is answered here without this file being edited - which is the same rule
\ OPERANDS+ follows when it decides which operand is the order.
\
\ It is deliberately quiet about words it cannot answer for. A row may be a
\ declared local's name, a mention of one, or a word this dialect does not model
\ at all; the first two are not words, and the third is refused by
\ HIR-WORD:ADMIT when the walk reaches it. Answering "no order" for them is
\ right: this pass decides whether an order is needed, not whether the body is
\ compilable.
: WORD-ORDER? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:OP HIR-MEANING:EQ if
      CTX BLD  CTX BLD  r sy HIR-WORD:OPCODE@  HIR:OPCODE  TOKEN-OPERANDS
      0<> exit
   then
   m HIR-MEANING:CONST-OP HIR-MEANING:EQ if
      CTX BLD  CTX BLD  r sy HIR-WORD:CONST-OPCODE@  HIR:OPCODE  TOKEN-OPERANDS
      0<> exit
   then
   false ;

: MEM-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 TOK-NEED !
   n 1 ?do
      i IN-DECL? 0=  i LOCAL-OF 0 <  and if
         r i WORD-ORDER? if 1 TOK-NEED ! then
      then
   loop ;

\ ---- the block skeleton ------------------------------------------------------
\ A structure's opener has to branch to the block its paths meet in, and that
\ block does not exist yet: IR-BUILD mints a block ordinal when the block is
\ CLOSED, and the join closes last. A Forth compiler patches the branch
\ afterwards; a module cannot be patched, because a frozen module is what every
\ reader downstream depends on and a builder's operations only grow at the end.
\ So the ordinal is worked out first, by walking the same tokens with the same
\ block-creation rules and counting. That is this pass, and it records one number
\ per opener: the ordinal of the block its forward branch goes to.
\
\ THE RULES IT COUNTS WITH ARE THE ONES BELOW, AND THEY ARE CHECKED. `if` makes
\ two blocks, `then` one, `begin` one, `until` two, `?do` three and `loop` three;
\ everything else makes none. Getting one of them wrong here would put a branch
\ somewhere else, so every closer compares the ordinal the build really reached
\ against the one the opener branched to, and a disagreement is refused by name.
\ Two derivations of one number, and they have to agree.
256 constant TMAX                    \ body tokens one definition may have

here CELL 1- and CELL swap - CELL 1- and allot
create JOIN-TAB TMAX cells allot

: TOK-CK ( n -- n )
   dup 0 < over TMAX >= or if E-NELAB-BLOCK throw then ;

: JOIN-OF ( n -- n )
   TOK-CK cells JOIN-TAB + @ ;

: JOIN! ( n n -- )
   {: ix:n j:n :}
   j ix TOK-CK cells JOIN-TAB + ! ;

\ During the skeleton the control stack holds the opener's TOKEN index where a
\ built frame holds its join ordinal, because that is what the closer has to
\ write the answer against. The depth is unused: no value is staged here.
: SK-PUSH ( HIR:ctrl n -- )
   {: k:HIR:ctrl ix:n :}
   k 0 ix CS-PUSH ;

: SK-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   VW MKEY r ix HIR-WORD:ADMIT-TOKEN
   HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if exit then
   r  VW MKEY ix NTAPE:SPELL@  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if     OF HIR-CTRL:OPEN-IF ix SK-PUSH  NB @ 2 + NB ! ENDOF
      close-if    OF HIR-CTRL:OPEN-IF CS-OPENER-CK CS-JOIN@
                     EXIT-PENDING @ 0= if NB @ 1+ NB ! then
                     0 EXIT-PENDING !
                     NB @ JOIN!  CS-POP ENDOF
      open-begin  OF HIR-CTRL:OPEN-BEGIN ix SK-PUSH  NB @ 1+ NB ! ENDOF
      close-until OF HIR-CTRL:OPEN-BEGIN CS-OPENER-CK drop
                     NB @ 2 + NB !  CS-POP ENDOF
      open-do     OF HIR-CTRL:OPEN-DO ix SK-PUSH  NB @ 3 + NB ! ENDOF
      close-loop  OF HIR-CTRL:OPEN-DO CS-OPENER-CK CS-JOIN@
                     NB @ 3 + NB !  NB @ JOIN!  CS-POP ENDOF
      index       OF ENDOF
      drop-loop   OF ENDOF
      early-exit  OF NB @ 1+ NB !  1 EXIT-USED !  1 EXIT-PENDING ! ENDOF
   ;MATCH ;

\ Walk the body once, counting. A structure left open at the end of the body is
\ refused here rather than at the return, because the walk that follows would
\ otherwise build blocks against a join nobody ever named.
: SKELETON ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   n TMAX > if E-NELAB-BLOCK throw then
   0 NB !
   CS-RESET
   EXIT-RESET
   n 1 ?do
      r i SK-STEP
   loop
   CS-N @ 0<> if E-NELAB-CTRL throw then
   EXIT-PENDING @ 0<> if E-NELAB-CTRL throw then
   EXIT-USED @ 0<> if NB @ 1+ EXIT-ORD ! then
   EXIT-USED @ 0<> if NB @ 1+ else NB @ then
   NFROZEN:BMAX > if E-NELAB-BLOCK throw then
   0 NB !
   CS-RESET ;

\ ---- what each structured control word builds --------------------------------
\ Each of the six below is one block construction, written out once. They share
\ three shapes: a two-way branch whose successors carry nothing, a stub block
\ that hands the live values on, and a join block that takes them as arguments.
\ Nothing here decides what the program computes; it decides which blocks the
\ program is made of and which values cross between them.
\
\ EVERY OPENER CHECKS THE PRE-SCAN AND EVERY CLOSER CHECKS ITSELF. A forward
\ branch names a block that does not exist yet, so its ordinal comes from
\ SKELETON, which walked the same tokens with the same rules before the build
\ started. The closer then compares the ordinal the build really reached with the
\ one the opener branched to. That is two independent derivations of the same
\ number, so a skeleton that disagreed with the build is a named refusal rather
\ than a branch into the middle of somewhere.

\ `if` ( flag -- ): the flag decides which of two paths runs, and both of them
\ end at the join. The false path is a stub, because the two-way branch carries
\ no values and the join needs them.
: DO-OPEN-IF ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   NB @ {: c:n :}
   ix JOIN-OF {: j:n :}
   HIR-CTRL:OPEN-IF  VN @ 1-  j  CS-PUSH
   ix  c 1+  c 2 +  TERM-BRZ
   ix j STUB
   ix OPEN-PLAIN ;

\ `then`: the true path reaches the join too, and the join takes as many
\ arguments as the vector was deep when the structure opened. An arm that left
\ the stack a different depth is refused here: the two paths would be handing the
\ same block different numbers of values.
: DO-CLOSE-IF ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-DEPTH@ {: d:n :}
   t CS-JOIN@ {: j:n :}
   EXIT-PENDING @ 0<> if
      0 EXIT-PENDING !
   else
      VN @ d <> if E-NELAB-JOIN throw then
      ix j TERM-BR
   then
   NB @ j <> if E-NELAB-CTRL throw then
   ix d OPEN-ARGS
   CS-POP ;

\ `begin`: the loop header is a block of its own, because control reaches it
\ twice - once from here and once from the latch - and the values it holds are
\ different each time. That is exactly what a block argument is for.
: DO-OPEN-BEGIN ( n -- )
   {: ix:n :}
   NB @ 1+ {: h:n :}
   VN @ {: d:n :}
   HIR-CTRL:OPEN-BEGIN d h CS-PUSH
   ix h TERM-BR
   ix d OPEN-ARGS ;

\ `until` ( flag -- ): leave when the flag is true, go round when it is false.
\ The latch is a stub, for the same reason the false arm of `if` is one.
: DO-CLOSE-UNTIL ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-DEPTH@ {: d:n :}
   t CS-JOIN@ {: h:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- d <> if E-NELAB-JOIN throw then
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix h STUB
   ix OPEN-PLAIN
   CS-POP ;

\ `?do` ( limit start -- ): run the body once per index from start up to limit,
\ and not at all when the two are equal - which is the whole difference between
\ `?do` and `do`, and it is the engine's own rule. The test is the subtraction of
\ the two, which is zero exactly when they are equal, wrap-around included. The
\ index and the limit then travel as the header's last two arguments, because
\ they change on every turn and the header is reached more than once.
: DO-OPEN-DO ( n -- )
   {: ix:n :}
   VN @ 2 < if E-NELAB-UNDER throw then
   VN @ 1- VAT {: st:IR-ID:ir-value-id :}
   VN @ 2 - VAT {: lm:IR-ID:ir-value-id :}
   ix HIR-OPCODE:SUB EMIT-OPCODE
   VN @ 1- {: d:n :}
   NB @ {: c:n :}
   ix JOIN-OF {: j:n :}
   HIR-CTRL:OPEN-DO d j CS-PUSH
   c 3 + CS-TOP cells CS-HEAD + !
   st CS-TOP CS-IDX !
   lm CS-TOP CS-LIM !
   ix  c 1+  c 2 +  TERM-BRZ
   ix j STUB
   ix OPEN-PLAIN
   st VPUSH
   lm VPUSH
   ix  c 3 +  TERM-BR
   ix  d 2 +  OPEN-ARGS
   VN @ 1- VAT CS-TOP CS-LIM !
   VN @ 2 - VAT CS-TOP CS-IDX !
   2 VDROP ;

\ `loop`: the index goes up by one, and the body runs again while it is still
\ below the limit - the engine's own signed test. The exit is a stub because the
\ join takes the live values, and the latch is a stub because the header does.
: DO-CLOSE-LOOP ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-DO CS-OPENER-CK {: t:n :}
   t CS-DEPTH@ {: d:n :}
   t CS-JOIN@ {: j:n :}
   t CS-HEAD@ {: h:n :}
   t CS-IDX @ {: iv:IR-ID:ir-value-id :}
   t CS-LIM @ {: lv:IR-ID:ir-value-id :}
   VN @ d <> if E-NELAB-JOIN throw then
   iv VPUSH
   ix 1 EMIT-LIT
   ix HIR-OPCODE:ADD EMIT-OPCODE
   VN @ 1- VAT {: nx:IR-ID:ir-value-id :}
   lv VPUSH
   ix HIR-OPCODE:LT EMIT-OPCODE
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix j STUB
   ix OPEN-PLAIN
   nx VPUSH
   lv VPUSH
   ix h TERM-BR
   ix d OPEN-ARGS
   NB @ j <> if E-NELAB-CTRL throw then
   CS-POP ;

\ `i`: the index of the innermost counted loop the walk is inside. A `begin`
\ between it and the `?do` changes nothing - Forth's `i` names the innermost
\ COUNTED loop - so the frame is searched for rather than assumed to be on top.
: DO-FRAME-IS? ( n -- bool )
   CS-KIND @ HIR-CTRL:OPEN-DO HIR-CTRL:EQ ;

: DO-FRAME ( -- n )
   -1
   CS-N @ 0 ?do
      CS-N @ 1- i - DO-FRAME-IS? if
         drop CS-N @ 1- i - leave
      then
   loop
   dup 0 < if E-NELAB-CTRL throw then ;

: DO-INDEX ( -- )
   DO-FRAME CS-IDX @ VPUSH ;

\ `unloop`: this dialect carries a counted loop's index and limit as block
\ arguments, so there is no frame to drop and nothing is staged. What it does is
\ insist that a counted loop IS open, which is the one thing the word means that
\ can be wrong: `unloop` outside a loop is refused by DO-FRAME, by name.
: DO-UNLOOP ( -- )
   DO-FRAME drop ;

\ `exit`: leave the word from here. The vector has to hold exactly the values the
\ word declares it leaves - one too few or one too many is a body that does not
\ match its effect, and it is refused here rather than turned into a branch that
\ hands the exit block the wrong number of values - and the branch carries them
\ to the block the return is in. The arm is finished: EXIT-PENDING says so, and
\ the only word that may follow is the `then` that closes it.
: DO-EXIT ( n -- )
   {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP CS-KIND @ HIR-CTRL:OPEN-IF HIR-CTRL:EQ 0= if E-NELAB-CTRL throw then
   VN @ OUT-N @ <> if E-NELAB-ARITY throw then
   EXIT-ORD @ 0 < if E-NELAB-CTRL throw then
   ix EXIT-ORD @ TERM-BR
   1 EXIT-PENDING ! ;

\ ---- binding and reading the locals ------------------------------------------
\ `:}`: take one value off the compile-time vector per declared name, RIGHT TO
\ LEFT. Forth pops the top value into the LAST name, so the first name declared
\ is the deepest value - which is the same order the entry block's arguments are
\ in, and the same order the caller's stack has them. Reading the values off the
\ vector bottom first and binding them in declaration order is exactly that
\ rule, and it is the one thing a locals group has to get right: `{: a b t :}`
\ over a stack holding a, b, t must bind a to the deepest, not to the top.
\
\ The group's own place is checked twice. The row this closer is on has to be
\ the row the pre-pass recorded, so the two walks agree about where the group
\ is; and no control structure may be open, because a group inside one would
\ bind names on a path that does not dominate the rest of the body and this
\ elaborator has no scoping rule for that (dot habu-scope-a-locals-2faa3d7a).
: DO-CLOSE-LOCALS ( n -- )
   {: ix:n :}
   ix LG-TO @ <> if E-NELAB-LOCAL throw then
   LBOUND @ 0<> if E-NELAB-LOCAL throw then
   CS-N @ 0<> if E-NELAB-LOCAL throw then
   LN @ {: k:n :}
   k VN @ > if E-NELAB-UNDER throw then
   VN @ k - {: base:n :}
   k 0 ?do
      base i + VAT  i LVAL !
   loop
   k VDROP
   1 LBOUND ! ;

\ A mention of a bound local in the body: the value it names goes back on the
\ vector. It produces no operation, exactly as a rename does, because the value
\ already exists - whatever computed it - and this only says where it is used.
: LOCAL-READ? ( n -- bool )
   {: ix:n :}
   ix LOCAL-OF {: k:n :}
   k 0 < if false exit then
   LBOUND @ 0= if E-NELAB-LOCAL throw then
   k LAT LVAL @ VPUSH
   true ;

\ The whole control table. Every arm names the blocks one source control word
\ builds; nothing else in this file decides what a control word means.
: DO-CONTROL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r  VW MKEY ix NTAPE:SPELL@  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if     OF ix DO-OPEN-IF ENDOF
      close-if    OF ix DO-CLOSE-IF ENDOF
      open-begin  OF ix DO-OPEN-BEGIN ENDOF
      close-until OF ix DO-CLOSE-UNTIL ENDOF
      open-do     OF ix DO-OPEN-DO ENDOF
      close-loop  OF ix DO-CLOSE-LOOP ENDOF
      index       OF DO-INDEX ENDOF
      drop-loop   OF DO-UNLOOP ENDOF
      early-exit  OF ix DO-EXIT ENDOF
   ;MATCH ;

\ ---- the walk ----------------------------------------------------------------
variable IX                          \ the body token the walk stands on

\ One body token. The word model answers what it is; a literal, an operation word
\ and a constant-and-operation word each stage operations, a rename stages none,
\ and a control word builds blocks. `unmodeled` never reaches the match -
\ HIR-WORD:ADMIT refuses it first - and the arm throws the same refusal rather
\ than inventing a second name for it.
: AFTER-EXIT-CK ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CONTROL MODELED-AS? 0= if E-NELAB-CTRL throw then
   r  VW MKEY ix NTAPE:SPELL@  HIR-WORD:CTRL@
   HIR-CTRL:CLOSE-IF HIR-CTRL:EQ 0= if E-NELAB-CTRL throw then ;

: STEP ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   EXIT-PENDING @ 0<> if r ix AFTER-EXIT-CK then
   ix IN-DECL? if exit then
   ix LOCAL-READ? if exit then
   VW MKEY r ix HIR-WORD:ADMIT-TOKEN
   MATCH HIR:meaning
      literal      OF ix EMIT-CONST ENDOF
      op           OF r ix EMIT-OP ENDOF
      const-op     OF r ix EMIT-CONST-OP ENDOF
      fixed        OF r ix EMIT-FIXED ENDOF
      control      OF r ix DO-CONTROL ENDOF
      rename       OF p r  VW MKEY ix NTAPE:SPELL@  RENAME ENDOF
      open-locals  OF E-NELAB-LOCAL throw ENDOF
      close-locals OF ix DO-CLOSE-LOCALS ENDOF
      unmodeled    OF E-HIR-UNMODELED throw ENDOF
   ;MATCH ;

\ Walk the body: every row after the name, to the end of the tape. The tape's
\ end is the definition's end, because the tape IS one definition - the unit the
\ producer opened and sealed around one scan - so there is nothing to look for
\ and nothing can follow.
: WALK ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena n:n :}
   1 IX !
   begin
      IX @ n <
   while
      p r IX @ STEP
      IX @ 1+ IX !
   repeat ;

\ ---- opening the function ----------------------------------------------------
\ The word's declared effect as a code-reference type: one cell in per input and
\ one cell out per output.
: SIGNATURE ( IR-CTX:ctx IR-BUILD:builder n n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder in:n out:n :}
   c b CELL-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   c b IR-BUILD:INTERN-CODE-REF ;

: ARITY-CK ( n n -- )
   {: in:n out:n :}
   in 0 < out 0 < or if E-NELAB-ARITY throw then
   in VMAX > out VMAX > or if E-NELAB-ARITY throw then ;

\ The defined word becomes a function whose span is the span of its name. A
\ colon definition is a definition in this module, so its linkage is defined and
\ its convention is Habu's.
: OPEN-FUN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      in:n out:n :}
   c b  v key 0 NTAPE:SPELL@  IR-BUILD:BEGIN-FUN
   c b  c b in out SIGNATURE  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  v key 0 NTAPE:SPAN@  IR-BUILD:SET-FUN-SPAN ;

\ The entry block, whose arguments are the word's inputs and whose span is the
\ definition's name - the only token a produced tape has that stands for the
\ definition as a whole. The arguments enter the value vector in declaration
\ order, so the first input is the deepest value, exactly as the caller's stack
\ has them.
: OPEN-BLOCK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      in:n :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  v key 0 NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   in 0 ?do
      c b  c b CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG VPUSH
   loop ;

\ The one row the definition frame is: the name the definition gives, read while
\ the parser was still interpreting because `:` parses it before it switches the
\ parser to compiling. A tape whose first row is a literal names nothing, and one
\ whose first row was read while compiling is not a top-level definition at all.
: NAME-READ ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v 0 NAME-CK
   v 0 NTAPE-MODE:INTERPRETING MODE-CK ;

public

\ Elaborate the one colon definition this sealed tape holds, and answer the
\ function it became. The arenas are, in order, the tape's sealed view, the word
\ model's pick pool and the word model's rows; the two counts are the values the
\ word takes and the values it leaves. Every identity read off the tape is
\ checked against this builder's module by the table that owns it, so a tape of
\ another module cannot be elaborated into this one.
: COLON ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena n n -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena in:n out:n :}
   in out ARITY-CK
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c 0 S-CTX !
   b 0 S-BLD !
   v 0 S-VW !
   key 0 S-KEY !
   v NTAPE:TOKENS {: n:n :}
   n 1 < if E-NELAB-SHAPE throw then
   v NAME-READ
   TOK-RESET
   out OUT-N !
   r n LOCALS-SCAN
   r n MEM-SCAN
   r n SKELETON
   c b v key in out OPEN-FUN
   c b v key in OPEN-BLOCK
   TOK-NEED @ 0<> if 0 EMIT-MEM then
   0 EXIT-PENDING !
   p r n WALK
   CS-N @ 0<> if E-NELAB-CTRL throw then
   EXIT-PENDING @ 0<> if E-NELAB-CTRL throw then
   EXIT-USED @ 0<> if
      VN @ out <> if E-NELAB-ARITY throw then
      0 EXIT-ORD @ TERM-BR
      NB @ EXIT-ORD @ <> if E-NELAB-CTRL throw then
      0 out OPEN-ARGS
   then
   c b v key out EMIT-RETURN
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
