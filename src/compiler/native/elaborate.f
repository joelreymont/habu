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
\ defined name and a body of integer literals, modeled arithmetic words and
\ compile-time stack renames. Nothing else.
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

\ The operands the opcode's schema declares, taken off the vector. The deepest
\ one is the operation's first operand, so `-` on a stack holding a then b
\ subtracts b from a, exactly as the source reads. The count is the schema's
\ fixed operand list: no source word of this subset binds to an opcode with a
\ variadic tail, and a word model that bound one would be refused downstream by
\ name - IR-OP measures a staged operation against the same schema, and IR-FUN
\ refuses a terminator that is not the block's last operation.
: OPERANDS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-OPERANDS {: k:n :}
   k VN @ > if E-NELAB-UNDER throw then
   VN @ k - {: base:n :}
   k 0 ?do
      c b  base i + VAT  IR-BUILD:ADD-OPERAND
   loop
   k VDROP ;

: RESULTS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b  c b CELL-TYPE  IR-BUILD:ADD-RESULT
   loop ;

\ Close the operation and push what it defined. The values are the operation's
\ own, read back off its row, so nothing here has to know which value ordinals
\ the store happened to mint.
: CLOSE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b id i IR-BUILD:OP-RESULT@ VPUSH
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

\ One operation of this dialect, staged at the span of the token named. How many
\ operands it takes off the vector and how many results it puts back is the
\ schema table's answer, never this file's.
: EMIT-OPCODE ( n HIR:opcode -- )
   {: ix:n k:HIR:opcode :}
   CTX BLD k HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
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
: OPEN-ARGS ( n n -- )
   {: ix:n n:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   n 0 ?do
      CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG VPUSH
   loop ;

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
\ the vector bottom first, which is the order the destination's arguments are in.
: TERM-BR ( n n -- )
   {: ix:n t:n :}
   CTX BLD  CTX BLD HIR-OPCODE:BR HIR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN
   VN @ 0 ?do
      CTX BLD  i VAT  IR-BUILD:ADD-OPERAND
   loop
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
   VW MKEY r ix HIR-WORD:ADMIT-TOKEN
   HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if exit then
   r  VW MKEY ix NTAPE:SPELL@  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if     OF HIR-CTRL:OPEN-IF ix SK-PUSH  NB @ 2 + NB ! ENDOF
      close-if    OF HIR-CTRL:OPEN-IF CS-OPENER-CK CS-JOIN@
                     NB @ 1+ NB !  NB @ JOIN!  CS-POP ENDOF
      open-begin  OF HIR-CTRL:OPEN-BEGIN ix SK-PUSH  NB @ 1+ NB ! ENDOF
      close-until OF HIR-CTRL:OPEN-BEGIN CS-OPENER-CK drop
                     NB @ 2 + NB !  CS-POP ENDOF
      open-do     OF HIR-CTRL:OPEN-DO ix SK-PUSH  NB @ 3 + NB ! ENDOF
      close-loop  OF HIR-CTRL:OPEN-DO CS-OPENER-CK CS-JOIN@
                     NB @ 3 + NB !  NB @ JOIN!  CS-POP ENDOF
      index       OF ENDOF
   ;MATCH ;

\ Walk the body once, counting. A structure left open at the end of the body is
\ refused here rather than at the return, because the walk that follows would
\ otherwise build blocks against a join nobody ever named.
: SKELETON ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   n TMAX > if E-NELAB-BLOCK throw then
   0 NB !
   CS-RESET
   n 1 ?do
      r i SK-STEP
   loop
   CS-N @ 0<> if E-NELAB-CTRL throw then
   NB @ NFROZEN:BMAX > if E-NELAB-BLOCK throw then
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
   VN @ d <> if E-NELAB-JOIN throw then
   ix j TERM-BR
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
   ;MATCH ;

\ ---- the walk ----------------------------------------------------------------
variable IX                          \ the body token the walk stands on

\ One body token. The word model answers what it is; a literal, an operation word
\ and a constant-and-operation word each stage operations, a rename stages none,
\ and a control word builds blocks. `unmodeled` never reaches the match -
\ HIR-WORD:ADMIT refuses it first - and the arm throws the same refusal rather
\ than inventing a second name for it.
: STEP ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   VW MKEY r ix HIR-WORD:ADMIT-TOKEN
   MATCH HIR:meaning
      literal   OF ix EMIT-CONST ENDOF
      op        OF r ix EMIT-OP ENDOF
      const-op  OF r ix EMIT-CONST-OP ENDOF
      control   OF r ix DO-CONTROL ENDOF
      rename    OF p r  VW MKEY ix NTAPE:SPELL@  RENAME ENDOF
      unmodeled OF E-HIR-UNMODELED throw ENDOF
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
   r n SKELETON
   c b v key in out OPEN-FUN
   c b v key in OPEN-BLOCK
   p r n WALK
   CS-N @ 0<> if E-NELAB-CTRL throw then
   c b v key out EMIT-RETURN
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
