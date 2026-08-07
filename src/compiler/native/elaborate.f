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
\ defined name and a body of integer and real literals, the modeled arithmetic,
\ comparison and bitwise words, the float words, compile-time stack renames, the
\ structured control words, the cell and byte memory words, `RECURSE`, a call to
\ a declared callee or a declared data word, and one `{: … :}` group of typed
\ locals read by name. Nothing else.
\
\ WHAT IT DOES NOT TRANSLATE, because a reader deciding whether a program can
\ compile here should not have to infer it from silence: string and character
\ literals, `case`, ADT `match` and `construct`, quotations, `does>`, plain
\ `do`, `+loop`, `leave`, `j`, the return-stack words, and `execute`. The
\ modeled vocabulary is the table in src/compiler/native/hir-word.f and it is
\ the authority; anything absent from it is E-HIR-UNMODELED.
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
require src/compiler/native/inline.f
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

\ ---- naming the token a refusal was about ------------------------------------
\ WHY THE CHAIN HAS TO SAY WHICH TOKEN. A body word the dialect cannot compile is
\ refused with E-HIR-UNMODELED and a body token kind the subset does not model
\ with E-HIR-KIND, and neither code carries the token it is about. A caller told
\ only "somewhere in this body is a word I cannot compile" cannot act on that: to
\ find the word it would have to lex the source again and guess, which is a
\ SECOND opinion about what a token is - the one thing this chain keeps a single
\ producer for. So the answer is taken where the elaboration already stands, off
\ the tape it is reading.
\
\ WHAT THE RECORD IS ABOUT, EXACTLY: the token this file was asking the word
\ model about. Both refusals belong to that one question -
\ src/compiler/native/hir-word.f ADMIT-TOKEN refuses the kind and ADMIT refuses
\ the word - and ADMIT-AT below is the only place this file asks it. Everything
\ else the elaboration can refuse is refused for a reason that is not about one
\ token's spelling: a declared arity, the shape of the tape, a control structure
\ left open, a vector that ran out. Those leave no record at all rather than a
\ record that points at whichever token the pass happened to have reached.
\
\ WHY IT IS TAKEN ON THE WAY OUT RATHER THAN ON THE WAY IN. The refusal is not
\ this file's, so the caller of ADMIT-TOKEN never gets an answer back to write
\ down. Writing the SPELLING down before each admit would copy bytes for every
\ token of every definition that compiles, to describe a refusal nobody ever
\ asks about; catching the refusal costs the compiling path nothing beyond the
\ one integer store ADMIT-AT makes, and takes the record only when there is a
\ refusal to describe.
\
\ AND IT HAS TO BE TAKEN HERE, because the tape does not outlive the call. The
\ migration runs the whole chain inside one IR-CTX context and gives every arena
\ back as it leaves (src/compiler/native/migrate.f BODY), so by the time the
\ caller has caught the refusal the tape that names the token is gone. A reader
\ that answered by reaching back into the tape would be reading released memory.
\ These bytes are this package's own and outlive the context.
\
\ THE RECORD DESCRIBES THE LAST ELABORATION AND NOTHING ELSE, and two cells make
\ that structural rather than careful. RF-AT is non-negative exactly while an
\ admit is in flight - ADMIT-AT writes the row before the call and takes it back
\ the instant the call returns - so the only way it can still hold a row is that
\ the admit threw. RF-ROW is the row a refusal was really taken for, or -1 for
\ "there is no record", and every reader below is gated on it, so the kind and
\ the bytes cannot contradict it: the same discipline
\ src/compiler/native/tape.f keeps for a token's literal, where the kind decides
\ and there is no second flag to disagree with it. COLON clears both before it
\ reads anything, so every attempt that REACHES this file starts with no record
\ and a definition that compiles leaves none behind.
\
\ AND THE CLEAR CANNOT COVER WHAT NEVER ARRIVES, which is the honest limit of it.
\ A definition can be refused before any elaboration begins - the engine rejects
\ the source while `evaluate` is still resolving names, and no tape is ever
\ sealed - and then COLON is not entered, so nothing here runs and the record is
\ still the last definition that DID reach it. That is not a state this file can
\ observe: it is the driver, not the elaborator, that knows an attempt was made.
\ So the clear is published as REFUSED-RESET, and a driver running many
\ definitions in one process calls it before each attempt. Then a refusal raised
\ before elaboration reads as what it is - no record - instead of as the previous
\ definition's word.
\
\ A SPELLING THAT DOES NOT FIT IS NOT NAMED. The bytes are the token's interned
\ spelling, and for a string literal that is the string's own text, which has no
\ ceiling - so no buffer can promise to hold every one. Truncating would answer a
\ name that denotes some OTHER word, which is worse than answering none, so what
\ does not fit is recorded as nothing: the row and the kind still say where the
\ refusal was and what shape the token had, and the spelling comes back empty,
\ which no real spelling is.
128 constant RF-CAP                  \ bytes of one refused spelling the record holds

here CELL 1- and CELL swap - CELL 1- and allot
variable RF-AT                       \ the row of the admit in flight, or -1
variable RF-ROW                      \ the row the record was taken for, or -1
variable RF-U                        \ how many of that row's spelling bytes are held
1 TYPED-BUFFER RF-KIND NTAPE:kind
create RF-BUF RF-CAP allot

: RF-RESET ( -- )
   -1 RF-AT !
   -1 RF-ROW ! ;

\ The one place this file asks the word model what a body token means, and the
\ one integer store that lets the refusal it may throw be described.
: ADMIT-AT ( IR-ARENA:arena n -- HIR:meaning )
   {: r:IR-ARENA:arena ix:n :}
   ix RF-AT !
   VW MKEY r ix HIR-WORD:ADMIT-TOKEN
   -1 RF-AT ! ;

\ Everything the record reads off the tape, in the order that keeps a partial
\ read honest: the kind, then an empty spelling, then the row - which is the
\ gate, so the record exists from that store onwards - and the bytes last,
\ because they are the only part that can be refused. A refused copy therefore
\ leaves a record whose spelling is empty rather than one carrying the length of
\ some earlier refusal's bytes.
: RF-TAKE ( -- )
   VW RF-AT @ NTAPE:KIND@ 0 RF-KIND !
   0 RF-U !
   RF-AT @ RF-ROW !
   CTX BLD  VW MKEY RF-AT @ NTAPE:SPELL@  RF-BUF RF-CAP IR-BUILD:SYMBOL-COPY
   RF-U ! ;

\ The record, taken so that it cannot BECOME the refusal. Two things make the
\ code caught here different from a swallowed error. A refusal is already in
\ flight - RF-RECORD is only ever reached from a handler that is about to rethrow
\ it - so nothing is lost by dropping this one; and letting it out would replace
\ the reason the caller asked about with the interner's reason for not being able
\ to spell it, which is the one failure a diagnostic must not have. What the code
\ decides instead is the CONTENT of the record: an over-long spelling leaves the
\ row and the kind standing and no bytes at all. Emptying the spelling here as
\ well as in the take is what makes that answer independent of how far the take
\ got, so a copier that ever wrote bytes before refusing still could not leave a
\ length behind for a caller to read them by.
: RF-RECORD ( -- )
   RF-AT @ 0 < if exit then
   [: RF-TAKE ;] catch {: rc:n :}
   rc 0= if exit then
   0 RF-U ! ;

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

\ Put another value in the place one already on the vector holds. The one writer
\ is the crossing below, which replaces a value with the same value read as the
\ other type - so the vector's DEPTH never changes here, and a caller that has
\ already worked out which position an operand comes from does not have to work
\ it out again.
: VAT! ( IR-ID:ir-value-id n -- )
   {: val:IR-ID:ir-value-id i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   val i VSTK ! ;

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
create LCROSS LMAX cells allot       \ whether a call can reach a mention of this local
create LBUF LNAME-CAP allot

: LRESET ( -- )
   0 LN !
   -1 LG-FROM !
   -1 LG-TO !
   0 LBOUND !
   LMAX 0 ?do  0 i cells LCROSS + !  loop ;

: LAT ( n -- n )
   dup 0 < over LN @ >= or if E-NELAB-LOCAL throw then ;

\ Whether this local's value has to survive a call - which is what makes it
\ travel. CROSS-SCAN below decides it for the whole definition before the walk
\ starts, and the section above CS-PENDING says what turns on the answer.
: LCROSS? ( n -- bool )
   LAT cells LCROSS + @ 0<> ;

: LCROSS+ ( n -- )
   LAT cells LCROSS +  1 swap ! ;

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
variable CALL-NEED                   \ whether the body calls anything at all
variable TAIL-NEED                   \ whether the last thing the body does is a call it need not come back from
variable CALL-BACK                   \ whether the body makes a call control comes BACK from
variable TAIL-ENTRY                  \ where the callee it would leave through starts
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

\ ---- the two value types, asked of the one authority --------------------------
\ A value's type is a fact the MODULE holds - IR-OP recorded it when the value
\ was minted - so this file asks the module rather than keeping a second record
\ beside the compile-time vector. A second record is what could disagree, and it
\ would be the one believed, because the vector is what every reader here walks.
: VTYPE-OF ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: val:IR-ID:ir-value-id :}
   CTX BLD val IR-BUILD:VALUE-TYPE@ ;

: REAL-T? ( IR-ID:ir-type-id -- bool )
   {: t:IR-ID:ir-type-id :}
   t  CTX BLD HIR:REAL-TYPE  NFROZEN:SAME-TYPE? ;

: CELL-T? ( IR-ID:ir-type-id -- bool )
   {: t:IR-ID:ir-type-id :}
   t  CTX BLD CELL-TYPE  NFROZEN:SAME-TYPE? ;

: REAL-VALUE? ( IR-ID:ir-value-id -- bool )
   VTYPE-OF REAL-T? ;

\ No double anywhere on the compile-time vector. It is asked where a value leaves
\ this compilation for a sixty-four-bit slot somebody else reads - a call's
\ operands - AFTER the crossing that puts every double into a cell has run, so
\ what it states is that the crossing ran: nothing here is meant to be reachable,
\ and a check that only looks at the shapes it expects to see is not a check.
\ Reaching it means a double was staged onto the vector between the crossing and
\ the operation, which would be eight bytes read by the wrong instruction at the
\ other end.
: NO-REAL-CK ( -- )
   VN @ 0 ?do
      i VAT REAL-VALUE? if E-NELAB-TYPE throw then
   loop ;

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

\ The results the opcode's schema declares, at the types it declares them. The
\ type is READ off the schema rather than restated here, which is the one
\ authority rule this file follows everywhere else: an operation that answers a
\ double and one that answers a cell differ in exactly this, and a stage that
\ wrote the type down itself would have to know which opcodes are which.
: RESULTS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op TOKEN-RESULTS drop
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b  c b op i IR-BUILD:SCHEMA-RESULT@  IR-BUILD:ADD-RESULT
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

\ ---- the two crossings between a cell and a double ----------------------------
\ A double lives in one unboxed cell holding its own bit pattern, so a program
\ that keeps doubles in data-stack cells and reads them back with float words is
\ crossing between two readings of the same eight bytes. Each crossing is an
\ operation - `hir.bits>real` one way and `hir.real>bits` the other - and it is
\ staged HERE, in front of the operation that wants the other reading, because a
\ staged operation cannot be opened inside another one and because the crossing
\ has to be a value the later operation reads rather than something the later
\ operation does.
\
\ NEITHER CROSSING COMPUTES ANYTHING, and that is the fact every use of them
\ below rests on. FMOV between the two register files moves eight bytes and reads
\ none of them, so a value that goes across and back is the same value to the
\ bit - which is what lets a double travel through a data-stack cell at a call,
\ through a block argument at a join, and back into a caller's slot at a return,
\ and arrive as the double it was.
: CROSS-VALUE ( n IR-ID:ir-value-id HIR:opcode -- IR-ID:ir-value-id )
   {: ix:n v:IR-ID:ir-value-id kop:HIR:opcode :}
   CTX BLD kop HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD v IR-BUILD:ADD-OPERAND
   CTX BLD  CTX BLD op 0 IR-BUILD:SCHEMA-RESULT@  IR-BUILD:ADD-RESULT
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ;

\ The same crossing over a value the vector holds. IT REPLACES THE VALUE IN
\ PLACE: the crossing consumes one value and answers one, so the vector's depth
\ does not change and the position an operand comes from is the position it came
\ from before.
: CROSS1 ( n n HIR:opcode -- )
   {: ix:n k:n kop:HIR:opcode :}
   ix  k VAT  kop CROSS-VALUE  k VAT! ;

\ A RUN of vector positions as CELLS. It is the crossing a value takes on its way
\ OUT of this compilation's register files and into a sixty-four-bit slot
\ somebody else reads: the caller's stack at a return, the data stack at a call,
\ which is where the machine stage puts every value a call site hands over, and
\ the argument positions of a body copied in from a record, which was compiled
\ against the cells a routine's own entry block takes. A cell is already a cell
\ and crosses nothing.
: CELL-CROSS-RUN ( n n n -- )
   {: ix:n base:n k:n :}
   k 0 ?do
      base i + VAT REAL-VALUE? if ix base i + HIR-OPCODE:REALBITS CROSS1 then
   loop ;

\ Every one of the bottom `n` vector values as a cell, which is what a return and
\ a call want: both of them hand over everything that is live.
: CELL-CROSS ( n n -- )
   {: ix:n n:n :}
   ix 0 n CELL-CROSS-RUN ;

\ Make the value at one vector position answer to the type the position wants.
\ ONE difference is a crossing and every other difference is a refusal, and the
\ asymmetry is the source language's rather than a convenience:
\
\   a CELL where a DOUBLE is wanted is crossed. The only cells a checked body can
\   hand to a float word are cells that hold doubles - the word's own arguments,
\   which arrive in data-stack cells, a cell read out of memory, and a local
\   naming one of those - because the checker has already refused every body that
\   hands a genuine integer to `f+`. So the crossing states what the program
\   already means.
\
\   a DOUBLE where a CELL is wanted is REFUSED. Nothing in this dialect computes
\   with a double read as an integer, so a double reaching `hir.add` is a wrong
\   program and not a conversion to invent. The one place the source really does
\   put a double back into a cell is the definition's outputs, and EMIT-RETURN
\   crosses there by name; putting one into MEMORY is the same crossing at
\   `hir.store` and belongs with the leaf that compiles a float body with memory
\   in it (dot habu-store-a-double-a31b313e).
\
\ WHEN THE CHECKER'S OWN TYPES REACH A RECORDED UNIT (dot
\ habu-bind-checker-env-ed4f9f87) the first half tightens too: an argument
\ declared `r` would arrive as a double and the crossing would be gone rather
\ than assumed.
: COERCE1 ( n n IR-ID:ir-type-id -- )
   {: ix:n k:n want:IR-ID:ir-type-id :}
   k VAT VTYPE-OF {: have:IR-ID:ir-type-id :}
   have want NFROZEN:SAME-TYPE? if exit then
   want REAL-T? have CELL-T? and 0= if E-NELAB-TYPE throw then
   ix k HIR-OPCODE:BITSREAL CROSS1 ;

\ Every general operand position of the operation about to be staged, against the
\ value that will fill it. The walk is the one OPERANDS+ makes, in the same
\ order and off the same schema, so the position a value is checked at is the
\ position it is handed over at.
: COERCE-OPERANDS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id ix:n :}
   c b op IR-BUILD:SCHEMA-OPERANDS {: k:n :}
   k  c b op TOKEN-OPERANDS  - {: v:n :}
   v VN @ > if E-NELAB-UNDER throw then
   VN @ v - {: base:n :}
   0 OPJ !
   k 0 ?do
      c b op i IR-BUILD:SCHEMA-OPERAND@ TOKEN? 0= if
         ix  base OPJ @ +  c b op i IR-BUILD:SCHEMA-OPERAND@  COERCE1
         OPJ @ 1+ OPJ !
      then
   loop ;

\ ---- the block-local literal memo ---------------------------------------------
\ One number written twice in one block is one value. A body that says `3` in two
\ places used to get two hir.const operations and, two lowerings later, two
\ move-wide chains; the second computes what the first already holds. This memo
\ makes the second reference read the first value instead.
\
\ WHY IT IS DOMINANCE-LOCAL, WHICH IS THE WHOLE SOUNDNESS ARGUMENT. Reusing a
\ value is only legal where its definition dominates the reference. Every entry
\ here was defined by an operation staged into a block that dominates the block
\ being built, and a later position in a dominated block is dominated by it - so
\ the rule needs no dominance query and no verifier licence.
\
\ THE MEMO IS CLEARED AT THE TWO OPENERS WHOSE NEW BLOCK IS NOT DOMINATED BY THE
\ OLD ONE. OPEN-ARGS-H opens a join, which is reached by an edge from every path
\ that ends there, so no block a walker has just left dominates it; OPEN-BLOCK
\ opens a definition's entry block, which no block reaches at all. Either could
\ name a value from a block that does not dominate the reader, so neither keeps
\ anything.
\
\ IT IS CARRIED THROUGH OPEN-PLAIN, BY THAT SAME CONSTRUCTION AND NOT BY AN
\ EXTENSION OF IT. The block OPEN-PLAIN opens has exactly one predecessor - the
\ two-way branch just above it - so that predecessor dominates it, and so does
\ everything that dominates the predecessor. This is the same dominance the
\ inherited value vector already rides: OPEN-PLAIN keeps the vector for precisely
\ this reason, and the memo is a second reader of that one fact rather than a
\ second rule. What it buys is the fold across a structure's boundary - the `3`
\ before an `if` and the `3` at the top of its arm are one value.
\
\ THE STUB IS THE ONE PLACE THE INDUCTION NEEDS HELP, AND STUB-H IS WHERE IT GETS
\ IT. A stub and the block after the branch are SIBLINGS: both are reached from
\ the same two-way branch and neither dominates the other. Letting the memo out
\ of a stub would therefore let the sibling name a value the stub defined, which
\ is the one thing this rule forbids. So STUB-H marks the memo before it opens
\ the stub and releases it after the stub closes, and the block after the branch
\ inherits the memo exactly as the branch left it. That holds whatever a stub
\ ever comes to stage; it does not rest on today's stubs staging no literal.
\
\ WHAT IT COSTS, MEASURED, SO NOBODY REDISCOVERS IT. Folding two references
\ EXTENDS the surviving value's live range, and a longer live range is more
\ register pressure. On the corpus that is a win; in a deliberately starved frame
\ it can turn one spilled value into two, which is what
\ test/compiler/native-chain.f RSPILL-CASE guards - see the note on its body.
\
\ Only integer literals reach here; a real literal is its own path. Overflowing
\ the memo stops it remembering more, which loses folds and changes nothing else:
\ this is a cache, and a body with more than LITMAX distinct literals in one block
\ is past anything the corpus or hand-written Forth reaches.
64 constant LITMAX

create LIT-VAL LITMAX cells allot     \ the number
LITMAX TYPED-BUFFER LIT-ID IR-ID:ir-value-id
variable LIT-N

: LIT-RESET ( -- )
   0 LIT-N ! ;

\ The memo as a scope, which is what a stub needs. Rows are only ever appended,
\ so the count IS the mark: releasing to a mark drops exactly the rows added
\ since it was taken and leaves every earlier row the value it already held.
: LIT-MARK ( -- n )
   LIT-N @ ;

: LIT-RELEASE ( n -- )
   LIT-N ! ;

\ Which memo row holds this number, or -1.
: LIT-FIND ( n -- n )
   {: val:n :}
   -1
   LIT-N @ 0 ?do
      i cells LIT-VAL + @ val = if drop i leave then
   loop ;

: LIT-REMEMBER ( n IR-ID:ir-value-id -- )
   {: val:n id:IR-ID:ir-value-id :}
   LIT-N @ LITMAX >= if exit then
   val LIT-N @ cells LIT-VAL + !
   id LIT-N @ LIT-ID !
   LIT-N @ 1+ LIT-N ! ;

\ ---- the things a body token becomes -----------------------------------------
\ One integer literal, staged at the span of the token named. The value is the
\ whole content of a constant, so it rides as the attribute the opcode's schema
\ requires. It takes the value rather than reading it off the token, because a
\ constant-and-operation word's constant is the word model's and not the tape's.
\ A number this block has already staged is not staged again: the memo above
\ answers with the value the first one defined.
: EMIT-LIT ( n n -- )
   {: ix:n val:n :}
   val LIT-FIND {: j:n :}
   j 0 >= if j LIT-ID @ VPUSH exit then
   CTX BLD HIR-OPCODE:CONST HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-VALUE  CTX BLD val IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE
   val  VN @ 1- VAT  LIT-REMEMBER ;

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
   CTX BLD op ix COERCE-OPERANDS
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD op CLOSE ;

\ An integer literal the tape carries.
: EMIT-CONST ( n -- )
   {: ix:n :}
   ix  VW ix NTAPE:LIT@  EMIT-LIT ;

\ One double literal, staged at the span of the token named. The value the tape
\ carries is the cell the double IS, so it rides in the same integer attribute an
\ integer literal's value rides in - a double's bit pattern is a number and there
\ is nothing else to carry. What makes it a double is the opcode, whose schema
\ answers a double, and not the shape of the attribute.
: EMIT-FLIT ( n n -- )
   {: ix:n val:n :}
   CTX BLD HIR-OPCODE:FCONST HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD op ix COERCE-OPERANDS
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-VALUE  CTX BLD val IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE ;

: EMIT-FCONST ( n -- )
   {: ix:n :}
   ix  VW ix NTAPE:LIT@  EMIT-FLIT ;

\ ---- a word named by its spelling rather than by a tape row ------------------
\ THE THREE WORD FORMS BELOW COME IN PAIRS, AND THE PAIR IS WHAT LETS A COPIED
\ BODY REACH THEM. The token a body word is written on says two things: WHICH
\ word it is, and WHERE in the source it stands. For a token of the tape being
\ walked those are one row; for a token of a callee's body copied into this
\ definition they are not - the word is the callee's and the place is the CALL
\ SITE, which is the token this definition really wrote. So each form takes the
\ symbol and the span's token separately, and the tape-reading half is the one
\ that says they are the same row.
\ A word the dialect has an operation for. Which operation is the word model's
\ answer.
: EMIT-OP-SYM ( IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:OPCODE@  EMIT-OPCODE ;

: EMIT-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix  VW MKEY ix NTAPE:SPELL@  EMIT-OP-SYM ;

\ A word that pushes one fixed value - the address a `create`d data word names.
\ The value is the word model's, so this stages the same operation an integer
\ literal in the source would.
: EMIT-FIXED-SYM ( IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:FIXED-VALUE@  EMIT-LIT ;

: EMIT-FIXED ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix  VW MKEY ix NTAPE:SPELL@  EMIT-FIXED-SYM ;

\ A word that is one constant and one operation - `1-` is `1` then `-`. Both
\ halves come off the word model's row, so a second opcode meaning the same
\ thing is not needed and the source stays one token.
: EMIT-CONST-OP-SYM ( IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:CONST-VALUE@  EMIT-LIT
   ix  r sy HIR-WORD:CONST-OPCODE@  EMIT-OPCODE ;

: EMIT-CONST-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix  VW MKEY ix NTAPE:SPELL@  EMIT-CONST-OP-SYM ;

\ Leaving the word. The outputs are the whole vector, bottom first, and the
\ vector has to hold exactly as many as the word declares - one too few or one
\ too many is a body that does not match its effect, and it is refused here
\ rather than turned into a return of the wrong width. `hir.return` declares a
\ variadic operand tail, so the count is the word's and not the opcode's. The
\ return has no token of its own on a produced tape - the `;` that used to carry
\ it was consumed before the checker read anything - so it answers for the span
\ of the definition's name, which is the definition itself.
\ A double the word leaves goes back into a data-stack cell, which is where the
\ caller will find it: a Habu word leaves result j in slot j of the caller's
\ stack and a slot is a cell. So this is the second half of the crossing the
\ arguments took on the way in, and it is stated here rather than assumed,
\ because `hir.return` takes cells and a double handed to it unchanged would be
\ eight bytes the caller's next instruction reads with the wrong register file.
: RETURN-CROSS ( n -- )
   0 swap CELL-CROSS ;

: EMIT-RETURN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      out:n :}
   VN @ out <> if E-NELAB-ARITY throw then
   out RETURN-CROSS
   c b HIR-OPCODE:RETURN HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b v key 0 op OPEN
   out 0 ?do
      c b  i VAT  IR-BUILD:ADD-OPERAND
   loop
   out VDROP
   c b IR-BUILD:END-OP drop ;

\ ---- the open control structures ----------------------------------------------
\ One frame per structure the walk is inside. `depth` is how deep the value
\ vector was when the structure opened, which is what every arm has to leave it
\ at; `join` is the block the structure's paths meet in, or - for `begin` - the
\ header they go back to; `head` is the counted loop's header; and the index and
\ the limit are the counted loop's own two values, which live here rather than on
\ the value vector because Forth's loop parameters are not on the data stack.
\
\ FOUR MORE FIELDS, ALL FOR THE WORDS THAT STAND IN THE MIDDLE OF A STRUCTURE.
\ `arm` is what an `else` recorded, `-1` before one is met; `nw` is how many
\ `while`s the open loop has met; `xd` is how deep the vector was when the first
\ of them left the loop, which is what its exit block takes; and `exit` is the
\ ordinal of that exit block. They are frame fields rather than single variables
\ because a structure nests inside another one and each has its own answer.
\
\ TWO OF THEM ANSWER DIFFERENTLY IN THE TWO WALKS, WHICH IS THE CONVENTION `join`
\ ALREADY KEEPS. During the skeleton `join` holds the opener's TOKEN index, and
\ `arm` holds the `else`'s token index, because a token index is what a forward
\ ordinal has to be written against; during the build both hold block ordinals
\ and `arm` holds the depth the first arm left. Either way each field means "what
\ this walk has to remember about that word", and `-1` means it has not met one.
32 constant CMAX

here CELL 1- and CELL swap - CELL 1- and allot
variable CS-N
CMAX TYPED-BUFFER CS-KIND HIR:ctrl
create CS-DEPTH CMAX cells allot
create CS-JOIN CMAX cells allot
create CS-HEAD CMAX cells allot
create CS-ARM CMAX cells allot
create CS-NW CMAX cells allot
create CS-XD CMAX cells allot
create CS-EXIT CMAX cells allot
CMAX TYPED-BUFFER CS-IDX IR-ID:ir-value-id
CMAX TYPED-BUFFER CS-LIM IR-ID:ir-value-id

: CS-RESET ( -- )
   0 CS-N ! ;

: CS-AT ( n -- n )
   dup 0 < over CS-N @ >= or if E-NELAB-CTRL throw then ;

: CS-TOP ( -- n )
   CS-N @ 1- CS-AT ;

\ Opening a structure clears every field the words inside it may write, so a
\ frame never answers with what the structure that stood at this depth before it
\ left behind. `head`, the index and the limit are not cleared here because
\ `?do` writes all three before its own frame is read, and clearing them would
\ need a value id this file has no way to mint.
: CS-PUSH ( HIR:ctrl n n -- )
   {: k:HIR:ctrl d:n j:n :}
   CS-N @ CMAX >= if E-NELAB-BLOCK throw then
   CS-N @ {: t:n :}
   k t CS-KIND !
   d t cells CS-DEPTH + !
   j t cells CS-JOIN + !
   -1 t cells CS-ARM + !
   0 t cells CS-NW + !
   -1 t cells CS-XD + !
   -1 t cells CS-EXIT + !
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
: CS-ARM@ ( n -- n )      cells CS-ARM + @ ;
: CS-NW@ ( n -- n )       cells CS-NW + @ ;
: CS-XD@ ( n -- n )       cells CS-XD + @ ;
: CS-EXIT@ ( n -- n )     cells CS-EXIT + @ ;

: CS-JOIN! ( n n -- )     cells CS-JOIN + ! ;
: CS-ARM! ( n n -- )      cells CS-ARM + ! ;
: CS-XD! ( n n -- )       cells CS-XD + ! ;
: CS-EXIT! ( n n -- )     cells CS-EXIT + ! ;

\ One more `while` has been met by the loop this frame is.
: CS-WHILE+ ( n -- )
   {: t:n :}
   t CS-NW@ 1+  t cells CS-NW + ! ;

\ Whether an `else` has been met, which is the one question both walks ask of
\ `arm` whatever each of them stores in it.
: CS-ELSE? ( n -- bool )
   CS-ARM@ 0 >= ;

\ What `then` still owes an answer for. `else` answered the `if`'s forward
\ branch when it opened the second arm, so what is left unanswered is the branch
\ the `else` itself made; with no `else` it is still the `if`'s.
: CS-PENDING ( n -- n )
   {: t:n :}
   t CS-ELSE? if t CS-ARM@ exit then
   t CS-JOIN@ ;

\ ---- what is live at a point in the body -------------------------------------
\ FOUR LISTS AND NO FIFTH. What this walk holds live is the compile-time value
\ vector, the index and the limit of every counted loop it is inside, the value
\ of every bound local, and the memory order. The first is the Forth data stack;
\ the second is what `?do` took off it and the body may no longer see; the third
\ is what a `{: … :}` group named; the fourth is what a load and a store pass
\ along. Everything in this file that CARRIES live values - a branch's operands,
\ a block's arguments, and a call's operands and results - carries all four, in
\ that order: vector, loop counters, locals, order.
\
\ WHY THE MIDDLE TWO HAVE TO TRAVEL LIKE THE OTHER TWO. A loop's counters and a
\ local's value are ordinary SSA values in ordinary registers. Everything
\ downstream of this file learns what is live only from the lists this file
\ writes: the machine stage's call site saves exactly the values the call
\ operation names, and the block-argument machinery hands over exactly the values
\ a branch names. A value this file holds somewhere none of those lists reads is
\ a value nothing downstream can save or hand on - and a callee that keeps its
\ declared contract destroys the whole register pool, so it wipes it. That is the
\ bug this section exists to make impossible: a chain-compiled callee in a `?do`
\ body used to come back having overwritten the loop's own counter, and one in a
\ body with locals used to come back having overwritten a local.
\
\ WHAT CROSSES AN EDGE, WHICH IS NOT ALWAYS THE WHOLE OF WHAT IS LIVE. A call is
\ the only thing that renames a counter or a local: without one, the counters are
\ defined in the loop header and the locals in the entry block, and both dominate
\ every read, so nothing has to be handed anywhere and the module is exactly the
\ one this pass built before. With a call anywhere in the body a rename can
\ happen in a block that dominates nothing after it, so both lists cross every
\ edge. CALL-SCAN answers that question once for the definition, the same way
\ MEM-SCAN answers it for the memory order and for the same reason.
\
\ AND THREE SEAMS CROSS WITH A DIFFERENT SET OF LOOPS THAN ARE OPEN. The block
\ after `loop` and the return block an `exit` branches to are OUTSIDE the loop,
\ so its counters are dead on the way there; and the edge into a loop's header
\ carries that loop's counters whether or not anything renames them, because the
\ header is reached twice with a different index each time. So a carrier is told
\ a RANGE of open loops - the first one and how many, counting outermost first -
\ rather than a count alone, and each seam names its own range where it stands
\ and says why.
: DO-FRAME-IS? ( n -- bool )
   CS-KIND @ HIR-CTRL:OPEN-DO HIR-CTRL:EQ ;

\ How many counted loops are open here - the length of the second list, halved.
: DO-OPEN-N ( -- n )
   0
   CS-N @ 0 ?do
      i DO-FRAME-IS? if 1+ then
   loop ;

variable DOK                         \ counted loops the search below has passed

\ The frame of the `k`-th open counted loop, OUTERMOST FIRST. Every carrier asks
\ for its frames one at a time through this word, so the order the lists are
\ written in is stated once and they cannot fall out of step. A carrier that
\ asked for a loop that is not open would be handing over values that are not
\ there, and it is refused rather than answered with a frame of some other kind.
: DO-NTH ( n -- n )
   {: k:n :}
   -1
   0 DOK !
   CS-N @ 0 ?do
      i DO-FRAME-IS? if
         DOK @ k = if drop i leave then
         DOK @ 1+ DOK !
      then
   loop
   dup 0 < if E-NELAB-CTRL throw then ;

\ Which counted loops' counters cross an ORDINARY edge here - one that stays
\ inside every loop the walk is in: all of them, outermost first. With no call in
\ the body nothing renames them, so none of them cross.
: CROSS-N ( -- n )
   CALL-NEED @ 0= if 0 exit then
   DO-OPEN-N ;

: CROSS-DO ( -- n n )
   0 CROSS-N ;

\ How many bound locals cross an edge: the ones a call can reach, which
\ CROSS-SCAN worked out for the whole definition. Before the group has closed
\ there are none to cross, whatever the declaration said.
: CROSS-L ( -- n )
   LBOUND @ 0= if 0 exit then
   0
   LN @ 0 ?do  i LCROSS? if 1+ then  loop ;

\ One open loop's two counters as operands of the branch or call being staged,
\ index first. Which order they go in matters only in that all the carriers
\ agree, and they agree because these words are the only places any of the lists
\ is written.
: LOOP-OPERAND+ ( n -- )
   {: t:n :}
   CTX BLD  t CS-IDX @  IR-BUILD:ADD-OPERAND
   CTX BLD  t CS-LIM @  IR-BUILD:ADD-OPERAND ;

: LOOP-OPERANDS+ ( n n -- )
   {: lo:n h:n :}
   h 0 ?do  lo i + DO-NTH LOOP-OPERAND+  loop ;

\ The same two as ARGUMENTS of the block being opened, taken straight back into
\ the frame they belong to. A block argument is a new value, so the frame has to
\ name the new one from here on: the loop's index inside the block is the one the
\ branch handed over, not the one some earlier block defined.
: LOOP-ARG+ ( n -- )
   {: t:n :}
   CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG  t CS-IDX !
   CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG  t CS-LIM ! ;

: LOOP-ARGS+ ( n n -- )
   {: lo:n h:n :}
   h 0 ?do  lo i + DO-NTH LOOP-ARG+  loop ;

\ The crossing locals, in declaration order, as operands and as arguments. A
\ local is a name for a value and nothing else, so carrying one is carrying the
\ value it names and rebinding the name to what arrived.
\
\ `l` IS EITHER ALL OF THEM OR NONE, and it is checked rather than trusted: the
\ seams that hand nothing over say so with a zero, and everything else takes
\ CROSS-L. A count between the two would be a list neither the branch nor the
\ block could name.
: LOCAL-CK ( n -- bool )
   {: l:n :}
   l 0= if false exit then
   l CROSS-L <> if E-NELAB-LOCAL throw then
   true ;

\ No crossing local holds a double, which DO-CLOSE-LOCALS made true when it bound
\ them: a local that travels goes through a data-stack slot at a call, and a slot
\ is a cell, so the cell is where a travelling local lives and the crossing is
\ done ONCE at the binding rather than at every seam it reaches. Nothing here is
\ meant to be reachable and it is asked anyway, for the reason NO-REAL-CK is:
\ reaching it means a local was rebound to a double after the binding, and the
\ block argument waiting for it holds a register of the other file.
: NO-REAL-LOCAL-CK ( n -- )
   LOCAL-CK 0= if exit then
   LN @ 0 ?do
      i LCROSS? if
         i LVAL @ REAL-VALUE? if E-NELAB-TYPE throw then
      then
   loop ;

: LOCAL-OPERANDS+ ( n -- )
   dup NO-REAL-LOCAL-CK
   LOCAL-CK 0= if exit then
   LN @ 0 ?do
      i LCROSS? if CTX BLD  i LVAL @  IR-BUILD:ADD-OPERAND then
   loop ;

: LOCAL-ARGS+ ( n -- )
   LOCAL-CK 0= if exit then
   LN @ 0 ?do
      i LCROSS? if
         CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG  i LVAL !
      then
   loop ;

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
\ habu-let-exit-leave-7e013b93 carries the general case.
variable IN-N                        \ values the definition takes
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

\ ---- what type each block argument has ---------------------------------------
\ THE SEAM THIS TABLE EXISTS FOR. A block argument's type has to be stated when
\ the block is OPENED, and the block after an `if` is opened once while the
\ values that will reach it come from two arms written at two different places in
\ the body - so the type cannot be read off "the value arriving", because there
\ are two of them and they arrive at two different moments. Stating CELL for
\ every position, which is what this file did before, is right for an integer
\ body and wrong the moment an arm hands over a double: the same eight bytes
\ would be read out of the wrong register file at the other end.
\
\ THE RULE, AND THE ARGUMENT FOR IT. The FIRST edge into a block states the type
\ of each of its argument positions, and every later edge into that block crosses
\ its value to the type already stated. The crossing is `hir.bits>real` or
\ `hir.real>bits`, neither of which computes anything - they are one FMOV between
\ the two register files, the same eight bytes read the other way - so an edge
\ that crosses hands over exactly the value it was given, whichever direction it
\ goes in. That is what makes "first edge wins" a COST question rather than a
\ correctness one: any assignment of one type per position is bit-exact, and the
\ one this walk can state in a single pass is the first arriving value's.
\
\ WHY NOT "A DOUBLE ANYWHERE MAKES THE SLOT A DOUBLE". It is the rule a reader
\ expects and it cannot be kept in one pass: `x f0< if x else 0.0 then` hands the
\ join a cell from the arm built FIRST and a double from the arm built second,
\ and a module's operations only grow at the end - the first arm's branch is
\ already built and cannot be reached back into. Keeping that rule would mean a
\ second walk of the body computing a REAL-ness per slot per join, which is a
\ second derivation of the value vector's whole stack effect - locals, calls,
\ loops and all - and the one thing this file does not do is keep two records
\ that could disagree about the same fact.
\
\ AND WHY NEITHER CHOICE CAN GO WRONG QUIETLY. Every operation of this dialect
\ declares the type of each operand it takes, and IR-OP measures a staged
\ operation against its schema; the register allocator refuses a class spanning
\ the two files by name. So a slot typed the way a later reader cannot use is a
\ REFUSAL - E-NELAB-TYPE where a double reaches an integer operation,
\ E-IR-VERIFY-OPTYPE where one reaches the module unmediated - and never a wrong
\ instruction. The one thing that would be wrong quietly is what this table
\ replaces: a position typed CELL that a double is handed to unchanged.
\
\ WHAT TIGHTENS WHEN THE CHECKER'S OWN TYPES REACH A RECORDED UNIT (dot
\ habu-bind-checker-env-ed4f9f87). The checker types RELU-F `( r -- r )` and
\ therefore types its join `r`; this file cannot see that yet, so it reads the
\ join's type off the first arm and crosses the other. With the checker's types
\ bound, the word's arguments would ARRIVE as doubles, both arms would hand a
\ double over, and the crossing this table places would not be there to place.
\ The rule does not change - the first edge still states the type - it is just
\ that both edges would agree.
NFROZEN:BMAX VMAX * constant ARG-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create ARG-N NFROZEN:BMAX cells allot   \ vector positions stated for this block, or -1
ARG-CAP TYPED-BUFFER ARG-T IR-ID:ir-type-id  \ the type each of those positions has
VMAX TYPED-BUFFER XV IR-ID:ir-value-id  \ what the edge being staged really hands over

: ARG-RESET ( -- )
   NFROZEN:BMAX 0 ?do  -1 i cells ARG-N + !  loop ;

: ARG-BLOCK-CK ( n -- n )
   dup 0 < over NFROZEN:BMAX >= or if E-NELAB-BLOCK throw then ;

: ARG-STATED? ( n -- bool )
   ARG-BLOCK-CK cells ARG-N + @ 0 >= ;

: ARG-WIDTH@ ( n -- n )
   ARG-BLOCK-CK cells ARG-N + @ ;

: ARG-T@ ( n n -- IR-ID:ir-type-id )
   {: t:n k:n :}
   k 0 < k t ARG-WIDTH@ >= or if E-NELAB-JOIN throw then
   t VMAX * k + ARG-T @ ;

\ The first edge into a block, stating one type per vector position it hands
\ over. A block whose types were already stated is never restated: the whole
\ point is that the second edge is held to the first one's answer.
: ARG-STATE ( n n -- )
   {: t:n n:n :}
   t ARG-STATED? if E-NELAB-JOIN throw then
   n 0 < n VMAX > or if E-NELAB-CAP throw then
   n 0 ?do
      i VAT VTYPE-OF  t VMAX * i +  ARG-T !
   loop
   n t ARG-BLOCK-CK cells ARG-N + ! ;

\ A block that takes its live values as arguments. Every value the vector held
\ is handed over by the branch that reached it, so the vector is replaced by the
\ arguments: a join is the one place where two different definitions of "the
\ value in this stack slot" meet, and a block argument is what SSA calls that.
\
\ The open loops' counters come next and the memory order LAST when the
\ definition has one. Neither is on the value vector - Forth's data stack holds
\ neither - but both are live across the edge for exactly the same reason the
\ vector's values are, so they cross the same way and by the same mechanism. The
\ three positions are this file's convention and the one TERM-BR hands the
\ operands in, so the two always line up; the verifier matches a terminator's
\ operands against the destination's arguments position by position and would
\ refuse them if they did not.
\
\ `lo`, `h` AND `l` ARE WHAT THE EDGE INTO THIS BLOCK CROSSES WITH: which range of
\ open loops' counters, and how many bound locals. They are CROSS-DO and CROSS-L
\ for every ordinary block; the seams that enter or leave a loop, and the one
\ that leaves the definition, name their own range and say why where they stand.
\
\ THE VECTOR POSITIONS TAKE THE TYPES THE FIRST EDGE INTO THIS BLOCK STATED, and
\ that is the whole of the join-type rule as far as this word is concerned: the
\ table above holds the answer, TERM-BR-H below put it there, and every edge
\ after the first was already held to it. A block reached with nothing stated is
\ refused rather than opened at a guess - every seam that opens a block with
\ arguments branches to it first, so no edge means the walk and its own carriers
\ disagree about what reaches this block.
\
\ THE OTHER THREE GROUPS NEED NO TABLE, and each for its own reason. A counted
\ loop's index and limit are integers: `?do` subtracts them with `hir.sub`, whose
\ operands are cells, so a double in either is E-NELAB-TYPE before the loop's
\ header is ever opened. A bound local that crosses anything is a CELL by
\ construction - DO-CLOSE-LOCALS puts it in one, because a call carries it
\ through a data-stack slot and a slot is a cell. And the memory order has its
\ own type and holds no register at all.
: OPEN-ARGS-H ( n n n n n -- )
   {: ix:n n:n lo:n h:n l:n :}
   NB @ ARG-STATED? 0= if E-NELAB-JOIN throw then
   NB @ ARG-WIDTH@ n <> if E-NELAB-JOIN throw then
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   LIT-RESET
   n 0 ?do
      CTX BLD  NB @ i ARG-T@  IR-BUILD:ADD-BLOCK-ARG VPUSH
   loop
   lo h LOOP-ARGS+
   l LOCAL-ARGS+
   TOK-LIVE @ 0<> if
      CTX BLD  CTX BLD HIR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG TOK!
   then ;

: OPEN-ARGS ( n n -- )
   CROSS-DO CROSS-L OPEN-ARGS-H ;

\ A block that takes no arguments and keeps the vector it inherits. Its only
\ predecessor is the two-way branch just above it, and a two-way branch hands
\ nothing over, so every value the vector holds was defined in a block that
\ dominates this one and may be read here by name. That is the dominance rule
\ the freeze verifier already enforces, not a licence this file takes.
\
\ THE LITERAL MEMO IS KEPT FOR THE SAME REASON AND BY THE SAME SENTENCE. Its
\ entries name values defined in blocks that dominate the one being left, and
\ this block is dominated by that one, so they dominate this block too. Nothing
\ is reset here; the memo's own header says where it is reset and why a stub is
\ the one boundary it must not cross.
: OPEN-PLAIN ( n -- )
   {: ix:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN ;

\ ---- what one edge really hands over -----------------------------------------
\ One vector position, as the type the destination's argument has. A value
\ already of that type is handed over as it is; a cell where the destination
\ holds a double, and a double where it holds a cell, are the two crossings, and
\ neither computes anything - the destination receives the same eight bytes
\ either way. A difference that is neither of those is a type this file has no
\ crossing for and is refused by name.
\
\ IT DOES NOT TOUCH THE VECTOR. The crossings are staged into the block being
\ closed and their results go into a list of their own, because the vector this
\ edge reads may still be live on ANOTHER path: the stub a two-way branch is
\ split with hands over the same values the arm below it goes on to read, and
\ rewriting the vector there would leave the arm reading a value defined in a
\ block that does not dominate it.
: EDGE-VALUE ( n n IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: ix:n k:n want:IR-ID:ir-type-id :}
   k VAT {: v:IR-ID:ir-value-id :}
   v VTYPE-OF want NFROZEN:SAME-TYPE? if v exit then
   want REAL-T?  v REAL-VALUE? 0=  and if
      ix v HIR-OPCODE:BITSREAL CROSS-VALUE exit
   then
   want CELL-T?  v REAL-VALUE?  and if
      ix v HIR-OPCODE:REALBITS CROSS-VALUE exit
   then
   E-NELAB-TYPE throw ;

\ The whole vector as this edge hands it over. The first edge into a block states
\ the types and hands its values on untouched; every later one is held to what
\ was stated and crosses what differs. The width is checked against the record
\ too, which is a second derivation of the agreement the closers already check -
\ two paths into one join carry the same number of values.
: EDGE-STAGE ( n n -- )
   {: ix:n t:n :}
   t ARG-STATED? 0= if
      t VN @ ARG-STATE
      VN @ 0 ?do  i VAT  i XV !  loop
      exit
   then
   t ARG-WIDTH@ VN @ <> if E-NELAB-JOIN throw then
   VN @ 0 ?do
      ix i  t i ARG-T@  EDGE-VALUE  i XV !
   loop ;

\ Hand every live value to one block and end this one. The operands are the
\ vector bottom first, then two per open loop the edge crosses with, then one per
\ local, then the memory order when the definition has one - the four positions
\ OPEN-ARGS-H gives them.
: TERM-BR-H ( n n n n n -- )
   {: ix:n t:n lo:n h:n l:n :}
   ix t EDGE-STAGE
   CTX BLD  CTX BLD HIR-OPCODE:BR HIR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  VW MKEY ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN
   VN @ 0 ?do
      CTX BLD  i XV @  IR-BUILD:ADD-OPERAND
   loop
   lo h LOOP-OPERANDS+
   l LOCAL-OPERANDS+
   TOK-LIVE @ 0<> if
      CTX BLD TOK IR-BUILD:ADD-OPERAND
   then
   CTX BLD  t BLOCK-ORD  IR-BUILD:ADD-SUCCESSOR
   CTX BLD IR-BUILD:END-OP drop
   CLOSE-BLOCK ;

: TERM-BR ( n n -- )
   CROSS-DO CROSS-L TERM-BR-H ;

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
\
\ THE LITERAL MEMO IS SCOPED TO THE STUB, WHICH IS WHERE CARRYING IT HAS TO STOP.
\ A stub is the ONE block a walk opens that does not dominate what the walk opens
\ next: the block after the branch is the stub's sibling, reached from the same
\ two-way branch, and a value defined in a stub reaches neither it nor anything
\ below it. The stub may READ the memo, because the branch above dominates the
\ stub as it dominates everything else here; what it may not do is add to the
\ memo the sibling then inherits. So the mark is taken before the stub opens and
\ released after it closes. Today's stubs stage no literal at all - they stage
\ only what one edge crosses with - and this scope is what makes that a fact
\ about stubs rather than the reason the rule holds.
: STUB-H ( n n n n n -- )
   {: ix:n t:n lo:n h:n l:n :}
   LIT-MARK {: m:n :}
   ix OPEN-PLAIN
   ix t lo h l TERM-BR-H
   m LIT-RELEASE ;

: STUB ( n n -- )
   CROSS-DO CROSS-L STUB-H ;

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

\ ---- the names the dialect does not model, before anything reads the model ----
\ THE DIALECT IS NOT THE WHOLE VOCABULARY AND NEVER WAS. It models the operations
\ this chain compiles into instructions; everything else a body names is some
\ OTHER word, and what a call site needs to know about another word - where its
\ code starts and how many cells it moves - is a fact of the running engine and
\ of the checker, not of the dialect. So before any pass reads the word model,
\ every name in the body that the model does not carry is put to the engine, and
\ the ones it and the checker can both answer for become callable rows.
\
\ THIS IS WHERE THE STAGING WENT. It used to be the CALLER of the migration that
\ named a body's callees and stated each one's entry address and arity by hand,
\ up to a fixed ceiling of four. Every one of those facts was already in the
\ running engine, and a caller that stated one wrongly compiled a routine that
\ branched somewhere nobody named, or moved the wrong number of cells, with
\ nothing downstream able to tell - the selector builds the save run, the restore
\ run and both byte counts from the one stated number, so its derivations always
\ agree with each other (dot habu-resolve-a-callee-0340dfde). Resolving here
\ removes the ceiling and the parameter together: a body names a word, and the
\ answer is taken from the only thing that has it.
\
\ AND IT IS A PASS OF ITS OWN RATHER THAN A QUESTION ASKED DURING THE WALK,
\ because the passes between here and the walk READ the model and decide from it:
\ MEM-SCAN decides whether an order has to be minted, CROSS-SCAN whether this
\ body calls at all and so whether its edges carry save runs. A row that appeared
\ later would make the walk emit a call the pre-scans had already concluded could
\ not happen, and CALL-LIVE and CALL-CROSS-CK refuse exactly that disagreement -
\ correctly, because a call with no order live and no crossing counted cannot be
\ lowered. One model, complete before the first reader, is the only arrangement
\ in which the scans and the walk are looking at the same program.
\
\ DECLINING IS AN ORDINARY ANSWER AND IS NOT AN ERROR. RESOLVE-CALLABLE declines
\ every spelling the engine and the checker cannot both answer for - a name that
\ denotes nothing here, one that runs at compile time, one the checker certified
\ no effect for, one whose effect has a term whose width cannot be stated - and
\ declining leaves the model exactly as it was, so the token is refused later as
\ unmodelled, by name, with the capability it waits for recorded. Nothing is
\ swallowed: the outcome a caller sees is still the admit's.
\
\ A NAME THE PROGRAM BOUND TO A LOCAL IS NOT A NAME OF ANYTHING ELSE, so the
\ declaration group and every mention of a bound local are passed over here for
\ the same reason the walk passes over them: the body chose that spelling, and a
\ dictionary word that happens to share it is not what the body means. The locals
\ pass runs first so that those bindings already exist to be passed over.
: RESOLVE-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? if exit then
   CTX BLD r sy HIR-WORD:RESOLVE-CALLABLE drop ;

: RESOLVE-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   n 1 ?do
      r i RESOLVE-STEP
   loop ;

\ ---- how far a body token may be from the definition's first ------------------
\ The ceiling every table keyed by a BODY TOKEN shares - the skeleton's forward
\ joins and the inline decision below - so the two cannot disagree about which
\ token indices exist. A body that wants more is a capability to raise here, not
\ a ceiling to widen silently.
256 constant TMAX                    \ body tokens one definition may have

: TOK-CK ( n -- n )
   dup 0 < over TMAX >= or if E-NELAB-BLOCK throw then ;

\ ---- which calls are COPIED instead of made ----------------------------------
\ THE DECISION, IN ONE SENTENCE. A call to a word whose body the chain recorded
\ when it published it (src/compiler/native/inline.f), and whose every recorded
\ token THIS definition's own word model admits with a meaning the copy has a
\ rule for, is not compiled as a call at all: the callee's body is elaborated
\ here, at the call site, out of the caller's own values.
\
\ WHY THE COPY IS MADE HERE AND NOT FURTHER DOWN THE CHAIN. Because here it needs
\ no new concept anywhere. The callee's arguments are values this walk is already
\ holding on the compile-time vector, so handing them over costs no instruction
\ and no data-stack slot; its results go back on that vector; and the operations
\ it stages are ordinary operations of this module, selected, allocated,
\ validated and emitted exactly as the caller's own. A copy made at any later
\ stage would be a copy of machine words into a module whose dialect has no form
\ for them, and the register allocator and its validator would have nothing to
\ re-derive them from.
\
\ WHAT A COPY REMOVES, WHICH IS WHY IT IS WORTH MAKING. The whole interface: the
\ site's stores of the arguments, its branch, its loads of the results, and the
\ callee's own pointer moves, loads, stores and return - and, because the site is
\ no longer a call, everything the CALLER was doing on account of one. A body
\ whose only calls are copied has nothing that destroys a register, so its loop
\ counters and its locals stop travelling across every edge (CROSS-SCAN below),
\ its routine stops declaring the direct-call trait, and its frame and link save
\ are not built at all.
\
\ THE DECISION IS MADE ONCE, BEFORE ANY OTHER WALK, AND EVERY LATER WALK READS
\ THAT ONE ANSWER. Three of them turn on it - whether the body needs a memory
\ order, whether anything in it renames a counter or a local, and what the walk
\ itself stages - and three walks each deciding it for themselves is three
\ answers that can disagree. So it is a table keyed by the body token, filled
\ here, and read everywhere else.
\
\ WHAT MAKES THE COPY TERMINATE. No recorded body holds a call
\ (src/compiler/native/inline.f), so nothing that is copied can contain a call to
\ copy in its turn, and a definition cannot reach itself: `RECURSE` is a control
\ word, which is not a meaning a recorded body may hold. The splice here is
\ therefore one level deep by construction rather than by a depth counter.
\
\ AND THAT IS TRUE OF A CALLEE WHOSE OWN CALLS WERE COPIED TOO, WITHOUT THIS FILE
\ DOING ANYTHING ABOUT IT. A definition whose call this walk copies is recorded
\ with the callee's ROW written where the call stood rather than with the call -
\ src/compiler/native/inline.f's STAGE-RECORD, driven by COPIED? below - so the
\ row that reaches a later caller is already flat. What that buys is the whole
\ point of it: `T-GET-N` writes `T-AT-N @` and publishes a routine with no call
\ in it, and a loop that reads an element per turn now copies `cells + @` into
\ its own body instead of branching. The recursion happens once, while the callee
\ is compiled, where the emitter's instruction count can be held against it.
\
\ AND WHOSE BODY IT IS, IS HELD BETWEEN TWO AUTHORITIES. The caller states what
\ effect it believes the word at an address has (src/compiler/native/migrate.f
\ stages it), and the callee's own migration recorded the effect that word really
\ declared. WHICH word lives at that address is no longer one of the caller's two
\ statements to get wrong: the staging refuses an address that is not where the
\ staged spelling's own word begins (migrate.f RESOLVES-TO-ENTRY), so the row
\ reached here is the row of the word the site named. The ARITY is what is left,
\ and a disagreement about it means the caller is compiling against an effect the
\ publication does not have - the call it would emit instead would be just as
\ wrong - so it is refused by name rather than resolved in either direction.
here CELL 1- and CELL swap - CELL 1- and allot
create INL-TAB TMAX cells allot      \ whether the call on this body token is copied

: INL-RESET ( -- )
   TMAX 0 ?do
      0 i cells INL-TAB + !
   loop ;

: INL-AT? ( n -- bool )
   TOK-CK cells INL-TAB + @ 0<> ;

: INL+ ( n -- )
   1 swap TOK-CK cells INL-TAB + ! ;

\ The symbol one recorded token's spelling is in THIS module. A recorded body
\ carries spellings as bytes, because the module its tokens were read into died
\ with the migration that compiled it; interning them here is what turns them
\ back into identities this module's word model can be asked about.
: INL-SYM ( n n -- IR-ID:ir-symbol-id )
   {: entry:n k:n :}
   CTX BLD  entry k NINL:SPELL$  IR-BUILD:INTERN-SYMBOL ;

public

\ WHAT A COPY STAGES FOR A TOKEN OF ONE MEANING. It is a value rather than a
\ branch because two different questions are asked about it and they have to be
\ ONE answer: whether a body holding such a token may be copied at all, which the
\ pre-scan below and src/compiler/native/migrate.f's recorder ask, and what the
\ splice stages when it gets there. A boolean answer and a staging ladder are two
\ tables over the same vocabulary, and two tables can drift - which is exactly
\ what had happened here: one of them said a meaning could be spliced and the
\ other threw on it, so a body ever holding that meaning would have aborted a
\ migration where every other refusal in this pass falls back quietly to a call.
\
\ `call` IS THE ANSWER FOR EVERY MEANING A COPY CANNOT HOLD, and it names the
\ behaviour rather than the refusal: the site calls the callee, which is what it
\ did before any of this existed and what every ceiling and every shape rule here
\ falls back to.
ENUM staging DERIVE eq
   call
   op
   const-op
   fixed
   rename
;ENUM

private

\ The one table. Every meaning of the dialect is answered here and nowhere else,
\ so a meaning added to it has to answer for itself once - and answering it
\ decides both questions at once, because both readers below are derived from
\ this and neither restates it.
\
\ WHY EACH `call` IS A `call`. A control word would build blocks this walk's
\ skeleton never counted; a callable would copy a call into a body that must hold
\ none; either half of a locals group would bind names in the caller's own scope.
\
\ AND THE TWO LITERAL MEANINGS ANSWER `call`, WHICH IS HONEST RATHER THAN ABSENT.
\ Both belong to a TOKEN and never to a word: src/compiler/native/hir-word.f's
\ N>MEAN refuses their stored codes outright, so MEANING@ - the only way a
\ meaning reaches this table - cannot answer either of them, and a token that
\ really is a literal is answered by its KIND long before this is asked. A row
\ claiming one would be a corrupt row, and what a corrupt row earns is not a copy.
: SPLICE-STAGING ( HIR:meaning -- staging )
   MATCH HIR:meaning
      literal      OF NELAB-STAGING:CALL ENDOF
      real-literal OF NELAB-STAGING:CALL ENDOF
      op           OF NELAB-STAGING:OP ENDOF
      const-op     OF NELAB-STAGING:CONST-OP ENDOF
      fixed        OF NELAB-STAGING:FIXED ENDOF
      rename       OF NELAB-STAGING:RENAME ENDOF
      callable     OF NELAB-STAGING:CALL ENDOF
      control      OF NELAB-STAGING:CALL ENDOF
      open-locals  OF NELAB-STAGING:CALL ENDOF
      close-locals OF NELAB-STAGING:CALL ENDOF
      unmodeled    OF NELAB-STAGING:CALL ENDOF
   ;MATCH ;

\ May a copied body hold a token of this meaning? Read off the table above rather
\ than listed again: a meaning is one a copy may hold exactly when the copy has
\ something to stage for it. Nothing here can say yes to a meaning the splice
\ cannot stage, because there is no second list to say it in.
: SPLICE-MEANING? ( HIR:meaning -- bool )
   SPLICE-STAGING NELAB-STAGING:CALL NELAB-STAGING:EQ 0= ;

: REC-NAME? ( n n -- bool )
   {: entry:n k:n :}
   entry k NINL:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ ;

\ One recorded token, against THIS definition's own word model. A literal is a
\ literal whatever any table says, because its kind is what makes it one; a name
\ has to be modeled here, and modeled as something the copy can stage.
: REC-TOKEN? ( IR-ARENA:arena n n -- bool )
   {: r:IR-ARENA:arena entry:n k:n :}
   entry k NINL:KIND@ {: kd:NTAPE:kind :}
   kd NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   entry k INL-SYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ SPLICE-MEANING? ;

: REC-BODY? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena entry:n :}
   true
   entry NINL:TOKENS 0 ?do
      r entry i REC-TOKEN? 0= if drop false leave then
   loop ;

\ Does this word stage an operation that takes the definition's memory order?
\ Asked of the SCHEMA TABLE and not of a list of memory words, so a form added to
\ the dialect is answered without this file being edited. A word of any other
\ meaning answers no: the two callers below ask their own question about a call.
: SYM-ORDER? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena sy:IR-ID:ir-symbol-id :}
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

: REC-TOKEN-ORDER? ( IR-ARENA:arena n n -- bool )
   {: r:IR-ARENA:arena entry:n k:n :}
   entry k REC-NAME? 0= if false exit then
   r  entry k INL-SYM  SYM-ORDER? ;

\ Does a copied body need the definition's memory order? A copy's loads and
\ stores thread the CALLER's order - there is one order per definition and the
\ copy is part of this one - so a caller that copies a body with a memory word in
\ it is a caller that touches memory, and the pre-scan below has to say so.
: REC-BODY-ORDER? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena entry:n :}
   false
   entry NINL:TOKENS 0 ?do
      r entry i REC-TOKEN-ORDER? or
   loop ;

\ The callee named on this token, and whether its body may be copied here.
\
\ THE ROW IS FOUND BY AN ADDRESS THE CALLER STATED, and what makes that address
\ the right routine's is settled where it was staged rather than here: a staged
\ address that is not where the staged spelling's own word begins is refused by
\ src/compiler/native/migrate.f's RESOLVES-TO-ENTRY, against the engine's own
\ lookup, which settles the name, the package and the address in one comparison.
\ So a row reached here belongs to the word this site named, and nothing about
\ WHICH routine it is remains to be asked.
\
\ WHAT IS STILL HELD AGAINST THE ROW IS THE ARITY, because that half is still the
\ caller's own statement: the migration takes the callee's declared effect from
\ the caller (dot habu-resolve-a-callee-0340dfde carries reading it off the
\ checker instead), and a row of the right routine and the wrong effect leaves
\ the caller's stack a value out. The caller's declaration and the publication
\ then disagree about the word at that address, and the CALL the site would emit
\ instead is wrong in exactly the same way, so it is refused by name rather than
\ resolved in either direction. An address with NO row is not a disagreement at
\ all: nothing was ever recorded there, and the site calls, which is what it
\ always did.
: CALLEE-COPY? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:ENTRY@ {: entry:n :}
   entry NINL:KNOWN? 0= if false exit then
   entry NINL:IN@  r sy HIR-WORD:CALLEE-IN@  <> if E-NELAB-INLINE throw then
   entry NINL:OUT@ r sy HIR-WORD:CALLEE-OUT@ <> if E-NELAB-INLINE throw then
   r entry REC-BODY? ;

: INL-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CALLABLE MODELED-AS? 0= if exit then
   r ix CALLEE-COPY? if ix INL+ then ;

: INLINE-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   INL-RESET
   n 1 ?do
      i IN-DECL? 0=  i LOCAL-OF 0 <  and if r i INL-STEP then
   loop ;

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
\ A CALL THAT IS COPIED ANSWERS FOR THE BODY THAT REPLACES IT. The call operation
\ takes an order and the copy does not stage one, so a caller whose only memory
\ word was inside a copied body still needs an order and a caller that copies a
\ body with none does not - which is the same question asked of the body that
\ will really be there.
: WORD-ORDER? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:CALLABLE HIR-MEANING:EQ if
      ix INL-AT? if r  r sy HIR-WORD:ENTRY@  REC-BODY-ORDER? exit then
      CTX BLD  CTX BLD  HIR-OPCODE:WORDCALL HIR:OPCODE  TOKEN-OPERANDS
      0<> exit
   then
   m HIR-MEANING:CONTROL HIR-MEANING:EQ if
      CTX BLD  CTX BLD  HIR-OPCODE:CALL HIR:OPCODE  TOKEN-OPERANDS 0= if
         false exit
      then
      r sy HIR-WORD:CTRL@ HIR-CTRL:SELF-CALL HIR-CTRL:EQ exit
   then
   r sy SYM-ORDER? ;

: MEM-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 TOK-NEED !
   n 1 ?do
      i IN-DECL? 0=  i LOCAL-OF 0 <  and if
         r i WORD-ORDER? if 1 TOK-NEED ! then
      then
   loop ;

\ ---- does this definition CALL at all? ---------------------------------------
\ The other question a pre-scan answers about the whole body, and the section
\ above CS-PENDING says what turns on it: a call is the only thing that renames a
\ loop's counter or a local, so a body with none needs neither on any edge and
\ compiles to exactly the module it compiled to before. Both call forms count -
\ `RECURSE` and a call to another word - because both destroy the same registers.
\
\ AND A CALL THE DECISION ABOVE MARKED FOR COPYING IS NOT ONE. What stands there
\ is the callee's body, staged out of the caller's own values, and no operation of
\ it destroys anything - so a body whose every call is copied renames no counter
\ and no local, and compiles to exactly the module it would have compiled to with
\ no call written in it at all.
\
\ It is as quiet as WORD-ORDER? about rows it cannot answer for, and for the same
\ reason: this pass decides what has to travel, not whether the body compiles.
: WORD-CALL? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:CALLABLE HIR-MEANING:EQ if ix INL-AT? 0= exit then
   m HIR-MEANING:CONTROL HIR-MEANING:EQ if
      r sy HIR-WORD:CTRL@ HIR-CTRL:SELF-CALL HIR-CTRL:EQ exit
   then
   false ;

\ ---- which locals a call can reach -------------------------------------------
\ A local's value travels only if a call can destroy the register it is in before
\ something reads it again. The answer is a walk of the tape, and it has two
\ parts because control has two directions.
\
\ FORWARD IS THE EASY HALF: once a call has been met, every later mention of a
\ local is a read that a call could have got in front of, so the local travels.
\
\ BACKWARD IS THE HALF A TEXTUAL READING WOULD GET WRONG. Inside a loop, a
\ mention BEFORE the call is read again on the next turn, after it. So each open
\ loop remembers which locals were mentioned inside it before any call was met,
\ and a call anywhere in that loop marks all of them. A loop nested in another
\ records into both, because a call in the outer one reaches the inner one's
\ mentions the same way.
\
\ IT IS DELIBERATELY CONSERVATIVE ACROSS THE ARMS OF AN `if`: a call in one arm
\ marks a local mentioned later in the other, which cannot really read a value
\ that arm destroyed. Overstating what travels costs a block argument;
\ understating it is the miscompile this whole section exists to stop.
32 constant LSMAX                    \ loops one definition may nest, as CMAX does

here CELL 1- and CELL swap - CELL 1- and allot
variable LSN                         \ loops the scan is inside
create LS-CALL LSMAX cells allot     \ whether a call has been met inside this loop
create LS-PEND LSMAX cells allot     \ locals mentioned in it before any call was met

: ROW-CTRL? ( IR-ARENA:arena n HIR:ctrl -- bool )
   {: r:IR-ARENA:arena ix:n want:HIR:ctrl :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CTRL@ want HIR-CTRL:EQ ;

: OPENS-LOOP? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:OPEN-BEGIN ROW-CTRL?
   r ix HIR-CTRL:OPEN-DO ROW-CTRL? or ;

: CLOSES-LOOP? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:CLOSE-UNTIL ROW-CTRL?
   r ix HIR-CTRL:CLOSE-REPEAT ROW-CTRL? or
   r ix HIR-CTRL:CLOSE-LOOP ROW-CTRL? or ;

: LS-PUSH ( -- )
   LSN @ LSMAX >= if E-NELAB-BLOCK throw then
   0 LSN @ cells LS-CALL + !
   0 LSN @ cells LS-PEND + !
   LSN @ 1+ LSN ! ;

\ Closing a loop that met a call: everything mentioned in it before that call is
\ read again on a turn after it, so all of it travels.
: LS-POP ( -- )
   LSN @ 1 < if E-NELAB-CTRL throw then
   LSN @ 1- LSN !
   LSN @ cells LS-CALL + @ 0= if exit then
   LSN @ cells LS-PEND + @ {: m:n :}
   LN @ 0 ?do
      m 1 i lshift and 0<> if i LCROSS+ then
   loop ;

: LS-CALL+ ( -- )
   LSN @ 0 ?do  1 i cells LS-CALL + !  loop ;

: LS-PEND+ ( n -- )
   {: k:n :}
   LSN @ 0 ?do
      i cells LS-PEND + @  1 k lshift or  i cells LS-PEND + !
   loop ;

\ One row of the walk. A call marks the whole definition and every loop it is
\ inside; a mention of a local either travels at once or is remembered against
\ the open loops.
: CROSS-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix IN-DECL? if exit then
   ix LOCAL-OF {: k:n :}
   k 0 >= if
      CALL-NEED @ 0<> if k LCROSS+ exit then
      k LS-PEND+ exit
   then
   r ix WORD-CALL? if 1 CALL-NEED ! LS-CALL+ exit then
   r ix OPENS-LOOP? if LS-PUSH exit then
   r ix CLOSES-LOOP? if LS-POP then ;

: CROSS-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 CALL-NEED !
   0 LSN !
   n 1 ?do
      r i CROSS-STEP
   loop
   LSN @ 0<> if E-NELAB-CTRL throw then ;

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
\ two blocks, `else` one, `then` one, `begin` one, `while` two, `until` two,
\ `repeat` one, `?do` three and `loop` three; everything else makes none. Getting
\ one of them wrong here would put a branch somewhere else, so every closer
\ compares the ordinal the build really reached against the one the opener
\ branched to, and a disagreement is refused by name. Two derivations of one
\ number, and they have to agree.
\
\ AND ONE OPENER MAY NEED ITS ANSWER WRITTEN BY A WORD THAT IS NOT ITS CLOSER.
\ The forward branch `if` makes goes to the block its false path lands in, and
\ that is the join only when there is no `else`: with one, it is the else arm,
\ which `else` opens. So the answer for the `if` is written by whichever of the
\ two the walk meets first, and `then` then writes the join against the `else`'s
\ own token. A loop is the same shape read the other way round: `while` branches
\ forward to the block after the loop, which `repeat` opens, so the answer is
\ written against the `begin`'s token and every `while` of that loop reads the
\ one row - which is what lets a loop have more than one of them.
\
\ THE CEILING AND ITS BOUND CHECK ARE TMAX AND TOK-CK ABOVE, shared with the
\ inline decision, because both tables are keyed by the same body token and two
\ ceilings over one key are two answers about which tokens exist.
here CELL 1- and CELL swap - CELL 1- and allot
create JOIN-TAB TMAX cells allot

\ Every row starts as "no answer", so a token the skeleton wrote nothing against
\ reads back as one rather than as whatever the definition before this one left
\ in the same row. `begin` reads its row without knowing yet whether a `repeat`
\ will write one, and that is exactly the question the sentinel answers.
: JOIN-RESET ( -- )
   TMAX 0 ?do
      -1 i cells JOIN-TAB + !
   loop ;

: JOIN-OF ( n -- n )
   TOK-CK cells JOIN-TAB + @ ;

\ The forward ordinal a branch is about to name. A token the skeleton recorded
\ no answer for is the two walks disagreeing about what the body contains, and
\ it is refused rather than branched to.
: JOIN-CK ( n -- n )
   dup 0 < if E-NELAB-CTRL throw then ;

: JOIN! ( n n -- )
   {: ix:n j:n :}
   j ix TOK-CK cells JOIN-TAB + ! ;

\ During the skeleton the control stack holds the opener's TOKEN index where a
\ built frame holds its join ordinal, because that is what the closer has to
\ write the answer against. The depth is unused: no value is staged here.
: SK-PUSH ( HIR:ctrl n -- )
   {: k:HIR:ctrl ix:n :}
   k 0 ix CS-PUSH ;

\ `else`: the true arm ends here and the else arm starts, so one block closes and
\ the block that opens is the one the `if`'s false path was branching to. Its
\ ordinal is therefore the answer for the `if`'s token, and the join's answer is
\ left for `then` to write against this token.
\
\ AN ARM THAT ALREADY LEFT THE WORD HAS NOTHING TO CLOSE HERE, AND IS REFUSED.
\ `exit` ends the block it is in, so `if … exit else` would have `else` close a
\ block that is not open. It is the same rule this file already refuses an `exit`
\ anywhere but the last position of an arm for, and dot
\ habu-let-exit-stand-d74f14ec carries the capability.
: SK-ELSE ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-ELSE? if E-NELAB-CTRL throw then
   EXIT-PENDING @ 0<> if E-NELAB-CTRL throw then
   NB @ 1+ NB !
   t CS-JOIN@ NB @ JOIN!
   ix t CS-ARM! ;

\ `then`: the arm the walk is in ends at the join, and the join opens. Which
\ token the answer is written against is the one whose forward branch is still
\ unanswered - the `else`'s when there is one, and the `if`'s when there is not.
: SK-CLOSE-IF ( -- )
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-PENDING {: key:n :}
   EXIT-PENDING @ 0= if NB @ 1+ NB ! then
   0 EXIT-PENDING !
   key NB @ JOIN!
   CS-POP ;

\ `while`: the test block ends here, its false edge leaves the loop through a
\ stub, and the body opens - the same two blocks `if` makes, for the same reason.
\ The loop it leaves is recorded, because that is what tells `until` it is the
\ wrong closer for this loop and `repeat` that it is the right one.
: SK-WHILE ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   EXIT-PENDING @ 0<> if E-NELAB-CTRL throw then
   t CS-WHILE+
   NB @ 2 + NB ! ;

\ `repeat`: the body ends with the branch back to the header, and the block that
\ opens after it is the one every `while` of this loop left to. Its ordinal is
\ the answer for the `begin`'s token. A loop no `while` ever left cannot be
\ closed this way: the block after it would have no path into it at all.
: SK-REPEAT ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0= if E-NELAB-CTRL throw then
   NB @ 1+ NB !
   t CS-JOIN@ NB @ JOIN!
   CS-POP ;

\ `until`: the latch and the exit, as before - and only for a loop that no
\ `while` has left, because the block a `while` branched to is opened by
\ `repeat` and `until` opens no such block.
: SK-UNTIL ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0<> if E-NELAB-CTRL throw then
   NB @ 2 + NB !
   CS-POP ;

: SK-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   r ix ADMIT-AT
   HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if exit then
   r  VW MKEY ix NTAPE:SPELL@  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if      OF HIR-CTRL:OPEN-IF ix SK-PUSH  NB @ 2 + NB ! ENDOF
      mid-else     OF ix SK-ELSE ENDOF
      close-if     OF SK-CLOSE-IF ENDOF
      open-begin   OF HIR-CTRL:OPEN-BEGIN ix SK-PUSH  NB @ 1+ NB ! ENDOF
      mid-while    OF SK-WHILE ENDOF
      close-until  OF SK-UNTIL ENDOF
      close-repeat OF SK-REPEAT ENDOF
      open-do      OF HIR-CTRL:OPEN-DO ix SK-PUSH  NB @ 3 + NB ! ENDOF
      close-loop   OF HIR-CTRL:OPEN-DO CS-OPENER-CK CS-JOIN@
                      NB @ 3 + NB !  NB @ JOIN!  CS-POP ENDOF
      index        OF ENDOF
      drop-loop    OF ENDOF
      early-exit   OF NB @ 1+ NB !  1 EXIT-USED !  1 EXIT-PENDING ! ENDOF
      self-call    OF ENDOF
   ;MATCH ;

\ Walk the body once, counting. A structure left open at the end of the body is
\ refused here rather than at the return, because the walk that follows would
\ otherwise build blocks against a join nobody ever named.
: SKELETON ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   n TMAX > if E-NELAB-BLOCK throw then
   0 NB !
   JOIN-RESET
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
\ Each of the nine below is one block construction, written out once. They share
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
\ no values and the block it lands in needs them. Where it lands is the join when
\ the structure has one arm and the second arm when it has two; the skeleton
\ decided which, so this reads one ordinal either way.
: DO-OPEN-IF ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   NB @ {: c:n :}
   ix JOIN-OF JOIN-CK {: j:n :}
   HIR-CTRL:OPEN-IF  VN @ 1-  j  CS-PUSH
   ix  c 1+  c 2 +  TERM-BRZ
   ix j STUB
   ix OPEN-PLAIN ;

\ `else`: the first arm is over and the second one starts. The block that opens
\ is the one the `if`'s false stub already branched to, and it takes the values
\ the stub handed it - the vector as the `if` left it, which is what the frame's
\ depth records. The first arm ends by branching to the join, so from here on the
\ frame's join is that block and not the second arm.
\
\ WHAT THE JOIN'S WIDTH IS ONCE THERE ARE TWO REAL ARMS. With one arm the join is
\ also reached by the `if`'s own false stub, so the arm has to leave the vector
\ exactly as the `if` found it. With two, no edge into the join comes from the
\ `if` at all: both come from arms, so they only have to agree with EACH OTHER,
\ and `a b > if a else b then` - which leaves one value where the `if` found none
\ - is an ordinary structure rather than a refusal. The width is therefore what
\ the first arm left, recorded here for `then` to hold the second arm to.
: DO-ELSE ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-ELSE? if E-NELAB-CTRL throw then
   t CS-DEPTH@ {: d:n :}
   t CS-JOIN@ {: e:n :}
   ix JOIN-OF JOIN-CK {: j:n :}
   VN @ t CS-ARM!
   ix j TERM-BR
   NB @ e <> if E-NELAB-CTRL throw then
   j t CS-JOIN!
   ix d OPEN-ARGS ;

\ `then`: the arm the walk is in reaches the join too, and the join takes as many
\ arguments as every edge into it carries. An arm that left the stack a different
\ depth is refused here: the two paths would be handing the same block different
\ numbers of values.
: DO-JOIN-WIDTH ( n -- n )
   {: t:n :}
   t CS-ELSE? if t CS-ARM@ exit then
   t CS-DEPTH@ ;

: DO-CLOSE-IF ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t DO-JOIN-WIDTH {: w:n :}
   t CS-JOIN@ {: j:n :}
   EXIT-PENDING @ 0<> if
      0 EXIT-PENDING !
   else
      VN @ w <> if E-NELAB-JOIN throw then
      ix j TERM-BR
   then
   NB @ j <> if E-NELAB-CTRL throw then
   ix w OPEN-ARGS
   CS-POP ;

\ `begin`: the loop header is a block of its own, because control reaches it
\ twice - once from here and once from the latch - and the values it holds are
\ different each time. That is exactly what a block argument is for.
\
\ THE BLOCK AFTER THE LOOP IS READ HERE BECAUSE A `while` INSIDE IT HAS TO BRANCH
\ TO ONE. The skeleton wrote its ordinal against this token when a `repeat`
\ closed the loop, and nothing at all when an `until` did - so the frame carries
\ either a real ordinal or the sentinel that says this loop has no such block,
\ and a `while` that met the sentinel is refused rather than pointed anywhere.
: DO-OPEN-BEGIN ( n -- )
   {: ix:n :}
   NB @ 1+ {: h:n :}
   VN @ {: d:n :}
   HIR-CTRL:OPEN-BEGIN d h CS-PUSH
   ix JOIN-OF CS-TOP CS-EXIT!
   ix h TERM-BR
   ix d OPEN-ARGS ;

\ `while` ( flag -- ): stay in the loop while the flag is true and leave it when
\ it is false. That is one two-way branch and the two blocks `if` builds for the
\ same shape: the false edge goes through a stub to the block after the loop,
\ because a two-way branch carries no values and that block takes them, and the
\ true edge falls into the rest of the body.
\
\ THE POLARITY IS THE ONE THING THIS WORD HAS TO GET RIGHT, AND IT IS THE
\ OPPOSITE OF `until`'s. TERM-BRZ goes to its FIRST successor when the flag is
\ ZERO. `while` LEAVES on zero, so the first successor is the stub out of the
\ loop and the second is the body; `until` leaves on true, so its first successor
\ is the latch back to the header. Turning these two round compiles a loop that
\ runs exactly when it should not.
\
\ EVERY `while` OF ONE LOOP LEAVES THROUGH THE SAME BLOCK, so they all have to
\ hand it the same number of values. The first one to run states the width and
\ the rest are held to it - the same rule two arms of an `if` meet at their join,
\ for the same reason.
: DO-WHILE ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-EXIT@ JOIN-CK {: j:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   t CS-NW@ 0<> if
      VN @ 1- t CS-XD@ <> if E-NELAB-JOIN throw then
   then
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   VN @ t CS-XD!
   t CS-WHILE+
   ix j STUB
   ix OPEN-PLAIN ;

\ `repeat`: the body ends by branching back to the header, and the block that
\ opens after it is the one every `while` of this loop left to - so it takes the
\ values they handed it. The back edge carries the loop's own live values, which
\ have to be the ones the header takes, and a body that left the vector some
\ other depth is refused here.
: DO-CLOSE-REPEAT ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0= if E-NELAB-CTRL throw then
   t CS-DEPTH@ {: d:n :}
   t CS-JOIN@ {: h:n :}
   t CS-EXIT@ JOIN-CK {: j:n :}
   t CS-XD@ {: xd:n :}
   VN @ d <> if E-NELAB-JOIN throw then
   ix h TERM-BR
   NB @ j <> if E-NELAB-CTRL throw then
   ix xd OPEN-ARGS
   CS-POP ;

\ `until` ( flag -- ): leave when the flag is true, go round when it is false.
\ The latch is a stub, for the same reason the false arm of `if` is one. It
\ closes a loop no `while` ever left: the block a `while` branches out to is one
\ `repeat` opens, and this word opens no such block, so the values that `while`
\ handed over would arrive nowhere.
: DO-CLOSE-UNTIL ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0<> if E-NELAB-CTRL throw then
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
\ index and the limit then travel as live values of the loop, because they change
\ on every turn and the header is reached more than once.
\
\ THE FRAME OPENS AFTER THE EXIT STUB AND BEFORE THE LATCH, WHICH IS THE WHOLE
\ POINT OF WHERE CS-PUSH STANDS. The stub is the edge taken when the loop runs no
\ turns at all: it goes to the block AFTER the loop, where this loop's counters
\ are not live, so it is built while the frame is still closed. The branch into
\ the header goes INSIDE the loop, where they are, so the frame is open by then
\ and carries the starting index and limit; the header takes them back as
\ arguments and the frame names those from the first turn on.
\
\ AND THAT ONE EDGE CARRIES THIS LOOP'S COUNTERS WHETHER OR NOT ANYTHING RENAMES
\ THEM, which is why it names its own range instead of taking CROSS-DO. The
\ header is reached twice and holds a different index each time - that is what a
\ block argument is for, and it is true of a loop in a body with no call in it at
\ all. With no call that is the whole range: the loop being opened is the
\ innermost, and nothing renames the ones around it.
: HEAD-CROSS-DO ( -- n n )
   CALL-NEED @ 0<> if 0 DO-OPEN-N exit then
   DO-OPEN-N 1- 1 ;

: DO-OPEN-DO ( n -- )
   {: ix:n :}
   VN @ 2 < if E-NELAB-UNDER throw then
   VN @ 1- VAT {: st:IR-ID:ir-value-id :}
   VN @ 2 - VAT {: lm:IR-ID:ir-value-id :}
   ix HIR-OPCODE:SUB EMIT-OPCODE
   VN @ 1- {: d:n :}
   NB @ {: c:n :}
   ix JOIN-OF JOIN-CK {: j:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix j STUB
   ix OPEN-PLAIN
   HIR-CTRL:OPEN-DO d j CS-PUSH
   c 3 + CS-TOP cells CS-HEAD + !
   st CS-TOP CS-IDX !
   lm CS-TOP CS-LIM !
   ix  c 3 +  HEAD-CROSS-DO CROSS-L  TERM-BR-H
   ix d  HEAD-CROSS-DO CROSS-L  OPEN-ARGS-H ;

\ `loop`: the index goes up by one, and the body runs again while it is still
\ below the limit - the engine's own signed test. The exit is a stub because the
\ join takes the live values, and the latch is a stub because the header does.
\
\ TWO EDGES LEAVE HERE AND THEY CARRY DIFFERENT LISTS. The latch goes back into
\ the loop, so it hands the header the NEXT index - written into the frame first,
\ so that the one carrier states it - along with every enclosing loop's counters.
\ The exit goes to the block after the loop, which is outside it, so that edge
\ crosses with one loop fewer and the frame is closed before its join is opened.
: EXIT-CROSS-DO ( -- n n )
   CALL-NEED @ 0= if 0 0 exit then
   0 DO-OPEN-N 1- ;

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
   ix j  EXIT-CROSS-DO CROSS-L  STUB-H
   ix OPEN-PLAIN
   nx t CS-IDX !
   ix h  HEAD-CROSS-DO CROSS-L  TERM-BR-H
   CS-POP
   ix d OPEN-ARGS
   NB @ j <> if E-NELAB-CTRL throw then ;

\ `i`: the index of the innermost counted loop the walk is inside. A `begin`
\ between it and the `?do` changes nothing - Forth's `i` names the innermost
\ COUNTED loop - so the frame is searched for rather than assumed to be on top.
\ DO-FRAME-IS? is up with the live-value carriers, which ask the same question.
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

\ `RECURSE`: call the word being compiled. What the operation is handed is the
\ WHOLE compile-time value vector, bottom first, and what it answers is the
\ vector as it stands afterwards - the values below the arguments again, then the
\ word's outputs. The arguments are the top `in` of the vector, exactly as they
\ would be for any other word taking `in` values.
\
\ WHY EVERYTHING LIVE CROSSES THE OPERATION. src/compiler/native/hir.f gives the
\ reason in full: the callee is this same routine, so no register the caller holds
\ a value in survives the call, and the honest statement of that is that the call
\ consumes each live value and answers a new one. It costs nothing when nothing is
\ live - the operation then takes just the arguments and the order - and it is
\ what lets the machine stage put the survivors somewhere the callee cannot reach.
\
\ AND "EVERYTHING LIVE" MEANS ALL THREE LISTS. What this walk holds live is the
\ value vector, the counters of every counted loop it is inside, and the memory
\ order - the section above CS-PENDING states it once and names the three
\ carriers. The machine stage knows only what the operation names: it writes each
\ operand into a slot of the caller's own data stack, branches, and reads each
\ result back out. A value left off the operand list is a value that stays in a
\ register the callee's contract declares destroyed, and it comes back wrong. The
\ loop counters were left off, and a chain-compiled callee in a `?do` body came
\ back having overwritten the loop's index or limit - so the loop ran the wrong
\ number of turns and answered the wrong number.
\
\ THE ORDER IS TAKEN AND ANSWERED, so a call cannot be moved across a memory word
\ and a memory word cannot be moved across a call. It is live here because
\ MEM-SCAN counts a self-call as a word that needs one, so COLON minted it in the
\ entry block; reaching this with none is the same disagreement between the
\ pre-scan and the walk that a memory word with no order is, and it is refused
\ rather than patched up by minting one in whatever block the walk has reached.
\ How many values are live ACROSS the call, given what the callee takes and
\ leaves. Both call forms ask this and neither carries its own arithmetic: the
\ vector has to hold at least the arguments, an order has to be live for the
\ operation to take, and what is left over plus what the callee returns has to
\ still fit the vector afterwards.
: CALL-LIVE ( n n -- n )
   {: a:n r:n :}
   VN @ a < if E-NELAB-CALL throw then
   TOK-LIVE @ 0= if E-NELAB-CALL throw then
   VN @ a - {: k:n :}
   k r + VMAX > if E-NELAB-CALL throw then
   k ;

\ The operands of either call form: the order, then two per open counted loop,
\ then one per bound local, then the WHOLE vector bottom first, whose top `a`
\ values are the arguments.
\
\ THE ARGUMENTS ARE LAST AND NOTHING MAY COME AFTER THEM. The machine stage
\ writes operand i+1 into data-stack slot i and enters the callee with the
\ pointer one past the LAST of them, so the callee reads its argument j out of
\ slot k+j where k is everything the site published before them. Put a live value
\ after the arguments and the callee reads that value as an argument. That is why
\ the counters and the locals go in front of the vector and not behind it.
\
\ THE COUNTS ARE THE EDGE'S OWN, and a call is why an edge has any: reaching here
\ with the pre-scan saying the body calls nothing is the same disagreement
\ between the pre-scan and the walk that a memory word with no order is, and it
\ is refused rather than lowered into a save run the joins do not match.
: CALL-CROSS-CK ( -- )
   CALL-NEED @ 0= if E-NELAB-CALL throw then ;

\ Every value this call site hands over goes into a DATA-STACK SLOT: the machine
\ stage writes each operand into a slot of the caller's own stack below the
\ callee's argument base and reads it back out of that slot afterwards
\ (src/compiler/native/select.f, CALL-SAVE and CALL-RESTORE). A slot is
\ sixty-four bits and nothing else - it has no register file - so a double
\ crossing a call crosses as the cell it is, exactly as one leaving through
\ `hir.return` does, and comes back a cell that the next float word crosses
\ again. That is why CALL-RESULTS+ states CELL for every answer without knowing
\ anything about what arrives: after this crossing, every one of them IS a cell.
\
\ THE CROSSING RUNS BEFORE THE OPERATION IS OPENED, because a staged operation
\ cannot be opened inside another one - so both call forms cross first and stage
\ second, and NO-REAL-CK below states that they did.
: CALL-CROSS ( n -- )
   VN @ CELL-CROSS ;

: CALL-OPERANDS+ ( -- )
   CALL-CROSS-CK
   NO-REAL-CK
   CTX BLD TOK IR-BUILD:ADD-OPERAND
   CROSS-DO LOOP-OPERANDS+
   CROSS-L LOCAL-OPERANDS+
   VN @ 0 ?do
      CTX BLD  i VAT  IR-BUILD:ADD-OPERAND
   loop ;

\ Its results, one for one against those operands: the order again, then the
\ counters, then the locals, then one value per survivor and one per output.
\ `n` is the last group - what goes back on the vector - because that is the only
\ count either caller works out for itself.
: CROSS-RESULTS ( -- n )
   CROSS-N 2 *  CROSS-L + ;

: CALL-RESULTS+ ( n -- )
   {: n:n :}
   CTX BLD  CTX BLD HIR:MEM-TYPE  IR-BUILD:ADD-RESULT
   n CROSS-RESULTS +  0 ?do
      CTX BLD  CTX BLD CELL-TYPE  IR-BUILD:ADD-RESULT
   loop ;

\ The call's answer for one open loop, back into the frame it belongs to. Result
\ 2k+1 is the k-th loop's index and 2k+2 its limit, which is the order
\ LOOP-OPERANDS+ handed them over in.
: LOOP-RESULT@ ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id k:n t:n :}
   CTX BLD id  k 2 * 1+   IR-BUILD:OP-RESULT@  t CS-IDX !
   CTX BLD id  k 2 * 2 +  IR-BUILD:OP-RESULT@  t CS-LIM ! ;

: LOOP-RESULTS@ ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id lo:n h:n :}
   h 0 ?do  id i  lo i + DO-NTH  LOOP-RESULT@  loop ;

\ And its answer for the locals, which stand behind the counters.
variable LRK                         \ crossing locals the walk below has taken back

: LOCAL-RESULTS@ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id l:n :}
   l LOCAL-CK 0= if exit then
   0 LRK !
   LN @ 0 ?do
      i LCROSS? if
         CTX BLD id  CROSS-N 2 * LRK @ + 1+  IR-BUILD:OP-RESULT@  i LVAL !
         LRK @ 1+ LRK !
      then
   loop ;

\ Closing either call form: everything it consumed goes and what it answered
\ takes its place - the order into its slot, each loop's counters back into their
\ frame, each local's value back under its name, and the survivors and outputs
\ onto the vector.
: CALL-CLOSE ( n -- )
   {: n:n :}
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   VN @ VDROP
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK!
   id CROSS-DO LOOP-RESULTS@
   id CROSS-L LOCAL-RESULTS@
   n 0 ?do
      CTX BLD id  i 1+ CROSS-RESULTS +  IR-BUILD:OP-RESULT@ VPUSH
   loop ;

: DO-SELF-CALL ( n -- )
   {: ix:n :}
   IN-N @ OUT-N @ CALL-LIVE  OUT-N @ + {: back:n :}
   ix CALL-CROSS
   CTX BLD HIR-OPCODE:CALL HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CALL-OPERANDS+
   back CALL-RESULTS+
   back CALL-CLOSE ;

\ The three fields a call to another word carries: where the callee starts, and
\ what its declared effect is. Nothing here decides any of them - they are the
\ word model's row, read once by the caller below and written onto the operation
\ so that every later stage answers about one callee.
: WCALL-ATTRS+ ( n n n -- )
   {: entry:n in:n out:n :}
   CTX BLD  CTX BLD HIR:KEY-ENTRY
   CTX BLD entry IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD HIR:KEY-IN
   CTX BLD in IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD HIR:KEY-OUT
   CTX BLD out IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR ;

\ A word this definition CALLS. It is the same staging as `RECURSE` with one
\ difference, and the difference is where the arity comes from: a self-call takes
\ and leaves what the DEFINITION declares, and this takes and leaves what the
\ CALLEE declares. Everything the caller still holds crosses the operation either
\ way, for the reason src/compiler/native/hir.f gives - no register survives a
\ call whatever the callee destroys, so the honest statement is that the call
\ consumes each live value and answers it again.
: DO-WORD-CALL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:CALLEE-IN@ {: a:n :}
   r sy HIR-WORD:CALLEE-OUT@ {: o:n :}
   a o CALL-LIVE o + {: back:n :}
   ix CALL-CROSS
   CTX BLD HIR-OPCODE:WORDCALL HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CALL-OPERANDS+
   back CALL-RESULTS+
   r sy HIR-WORD:ENTRY@ a o WCALL-ATTRS+
   back CALL-CLOSE ;

\ ---- copying a callee's body in instead of calling it ------------------------
\ WHAT A COPIED CALL IS. The callee's recorded tokens, staged one at a time by
\ the very words that stage this definition's own tokens. Nothing else happens:
\ there is no operation for the call, no store run, no branch, no load run, and
\ the callee's own entry and exit are not there to be paid either.
\
\ THE ARGUMENTS ARE ALREADY WHERE THE BODY WANTS THEM. A Habu word's arguments
\ are the top of the data stack, and the top of the compile-time value vector is
\ exactly that - so the copied body reads its arguments off the vector by
\ standing where it stands, and leaves its results there. That is why a copy
\ costs no data-stack traffic at all where the call cost one slot per argument
\ and one per result.
\
\ EVERY OPERATION ANSWERS FOR THE CALL SITE'S SPAN, which is the token this
\ definition really wrote. The callee's own source belongs to a module that died
\ with the migration that compiled it, and a span of this module is what every
\ later stage can carry; a diagnostic about a copied instruction therefore points
\ at the call the programmer wrote, which is the line they can do something
\ about.
\
\ THE ARGUMENTS ARRIVE AS CELLS, BECAUSE THAT IS WHAT THE RECORD WAS COMPILED
\ AGAINST. A routine takes its arguments out of data-stack slots, so its entry
\ block's arguments are cells (OPEN-BLOCK says so), and the recorded tokens were
\ elaborated with cells in those positions - a body that stores its first
\ argument staged a cell store, because a cell is what it had. A caller holding a
\ DOUBLE there is holding the same eight bytes read the other way, so handing it
\ over unchanged would splice the row against types it was never compiled for:
\ `: T-SET-N ( r ptr a n -- ) T-AT-N ! ;` copied into a body that has just
\ computed a double would reach a cell store with a double in it and be refused,
\ though the call it replaces compiles and runs. So the argument positions are
\ crossed to cells first, which is EXACTLY what the call did - CALL-CROSS above
\ crosses everything live for the same reason - and the crossing computes
\ nothing, so the copy still answers to the bit what the call answered. What the
\ copy no longer pays is the crossing of everything ELSE that is live: only the
\ arguments go, because only the arguments are what the body reads.
\
\ AND THE RESULTS LEAVE AS CELLS, FOR THE MIRROR REASON. The row holds the
\ routine's straight-line OPERATIONS, and a routine is more than its operations at
\ both ends: it takes its arguments out of data-stack cells and it puts its
\ results back into them. The second half has no token either - the callee's own
\ compilation ended in EMIT-RETURN, whose RETURN-CROSS puts every double it leaves
\ back into the cell the caller's slot is - so a splice that reproduced only the
\ tokens would leave a DOUBLE where the call it replaces left a CELL, and a caller
\ that stored the result would be refused though the same source calling the same
\ word compiles and runs. Acceptance would then depend on whether the optimisation
\ fired, which is the one thing an optimisation may never decide. So the outputs
\ are crossed after the tokens, by the same run that crossed the inputs before
\ them, and for the reason RETURN-CROSS states: a Habu word leaves result j in
\ slot j of the caller's stack and a slot is a cell.
\
\ WHAT IS CHECKED WHILE THE COPY RUNS, AND NEITHER CHECK IS DECORATION. The body
\ may not reach BELOW the values its caller was holding - a checked body cannot,
\ because the checker proved it against its own declared effect, but the vector
\ is this file's and a floor it can state is a floor it should state - and it has
\ to leave the vector exactly as the callee's declared effect says. The second is
\ what holds the recorded body and the recorded arity together: a body that
\ consumed or left a different number of values than the row says is refused by
\ name rather than compiled into a definition whose stack is one value out.
\
\ AND WHICH MEANINGS MAY BE HERE AT ALL IS NOT DECIDED HERE. This dispatches on
\ SPLICE-STAGING - the one table - and not on the meaning, so it cannot hold an
\ opinion about which meanings are copyable that the pre-scan could contradict.
\ It used to: this was a second list over the same vocabulary, and it threw on
\ two meanings the other list said were fine. What is written here now is only
\ how each staging is staged, and the table decides which stagings exist.
\
\ THE `call` ARM IS UNREACHABLE AND IS STILL A THROW. It is unreachable because
\ the pre-scan read the same table over the same token and would not have marked
\ the call for copying; and it is a throw rather than a fall back to a call
\ because there is no way back from the middle of a splice - the vector already
\ holds the callee's arguments crossed to cells and part of its body staged. A
\ quiet answer there would leave a definition compiled out of half a body.
: INLINE-NAME ( IR-ARENA:arena IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:ADMIT SPLICE-STAGING
   MATCH staging
      call     OF E-NELAB-INLINE throw ENDOF
      op       OF r ix sy EMIT-OP-SYM ENDOF
      const-op OF r ix sy EMIT-CONST-OP-SYM ENDOF
      fixed    OF r ix sy EMIT-FIXED-SYM ENDOF
      rename   OF p r sy RENAME ENDOF
   ;MATCH ;

: INLINE-TOKEN ( IR-ARENA:arena IR-ARENA:arena n n n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n entry:n k:n :}
   entry k NINL:KIND@
   MATCH NTAPE:kind
      name           OF p r ix  entry k INL-SYM  INLINE-NAME ENDOF
      int-literal    OF ix  entry k NINL:LIT@  EMIT-LIT ENDOF
      real-literal   OF ix  entry k NINL:LIT@  EMIT-FLIT ENDOF
      char-literal   OF E-NELAB-INLINE throw ENDOF
      string-literal OF E-NELAB-INLINE throw ENDOF
   ;MATCH ;

: DO-INLINE ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:ENTRY@ {: entry:n :}
   entry NINL:IN@ {: a:n :}
   entry NINL:OUT@ {: o:n :}
   VN @ a < if E-NELAB-UNDER throw then
   VN @ a - {: base:n :}
   base o + VMAX > if E-NELAB-CAP throw then
   ix base a CELL-CROSS-RUN
   entry NINL:TOKENS 0 ?do
      p r ix entry i INLINE-TOKEN
      VN @ base < if E-NELAB-INLINE throw then
   loop
   VN @ base o + <> if E-NELAB-INLINE throw then
   ix base o CELL-CROSS-RUN ;

\ Either way of reaching another word's body. Which one this token is was decided
\ once, before any walk started, and is read here rather than asked again.
: DO-CALL ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   ix INL-AT? if p r ix DO-INLINE exit then
   r ix DO-WORD-CALL ;

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
\
\ AND IT CROSSES WITH NO COUNTED LOOP AND NO LOCAL, which is why both counts are
\ written here rather than taken from the frames. An `exit` leaves the word for
\ good: the return block is outside every structure and every name - COLON opens
\ it with nothing on the control stack and nothing but the outputs is read there
\ - so a branch that handed it a counter or a local would be handing a block
\ arguments it does not take.
: DO-EXIT ( n -- )
   {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP CS-KIND @ HIR-CTRL:OPEN-IF HIR-CTRL:EQ 0= if E-NELAB-CTRL throw then
   VN @ OUT-N @ <> if E-NELAB-ARITY throw then
   EXIT-ORD @ 0 < if E-NELAB-CTRL throw then
   ix EXIT-ORD @ 0 0 0 TERM-BR-H
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
\ A local that a call can reach is put into a CELL here, once, and stays one. The
\ reason is where it goes: the machine stage writes every value a call site hands
\ over into a data-stack slot and reads it back out of that slot, and a slot is
\ sixty-four bits with no register file attached - so a double naming a
\ travelling local is a double that has to be in a cell by the time the first
\ call is reached anyway. Doing it at the binding rather than at each seam means
\ the name has ONE type for the whole body, which is what lets the block argument
\ every edge hands it to be minted without asking where the walk has got to.
\
\ A local that no call can reach is not crossed and not touched. Its value is
\ defined in the block the group closed in, which dominates every mention of it,
\ so it is read where it stands and in whichever file it stands in - a double
\ stays in the floating file and costs nothing.
: LOCAL-BIND-CROSS ( n -- )
   {: ix:n :}
   LN @ 0 ?do
      i LCROSS?  i LVAL @ REAL-VALUE?  and if
         ix  i LVAL @  HIR-OPCODE:REALBITS CROSS-VALUE  i LVAL !
      then
   loop ;

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
   ix LOCAL-BIND-CROSS
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
      open-if      OF ix DO-OPEN-IF ENDOF
      mid-else     OF ix DO-ELSE ENDOF
      close-if     OF ix DO-CLOSE-IF ENDOF
      open-begin   OF ix DO-OPEN-BEGIN ENDOF
      mid-while    OF ix DO-WHILE ENDOF
      close-until  OF ix DO-CLOSE-UNTIL ENDOF
      close-repeat OF ix DO-CLOSE-REPEAT ENDOF
      open-do      OF ix DO-OPEN-DO ENDOF
      close-loop   OF ix DO-CLOSE-LOOP ENDOF
      index        OF DO-INDEX ENDOF
      drop-loop    OF DO-UNLOOP ENDOF
      early-exit   OF ix DO-EXIT ENDOF
      self-call    OF ix DO-SELF-CALL ENDOF
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
   r ix ADMIT-AT
   MATCH HIR:meaning
      literal      OF ix EMIT-CONST ENDOF
      real-literal OF ix EMIT-FCONST ENDOF
      op           OF r ix EMIT-OP ENDOF
      const-op     OF r ix EMIT-CONST-OP ENDOF
      fixed        OF r ix EMIT-FIXED ENDOF
      callable     OF p r ix DO-CALL ENDOF
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

\ ---- the two walks of the body, with the record around them ------------------
\ Both of them ask the word model about every token through ADMIT-AT, so both can
\ be left by a refusal that ADMIT-AT was standing in the middle of, and these are
\ where such a refusal is written down. Each hands its arguments back because a
\ checked catch takes a stack-preserving quotation and a quotation cannot read
\ the enclosing word's locals (docs/forth.md § Errors); each rethrows the code
\ unchanged, because this seam decides nothing about the refusal and only names
\ the token it was about.
\
\ THE BUILDER IS LEFT EXACTLY AS THE REFUSAL LEFT IT. Unwinding its open stages
\ would need handles a stack-preserving quotation cannot carry, which is the
\ paragraph at the top of this file. Naming a token needs nothing but what this
\ package has already parked, which is why one of the two is possible here and
\ the other is not.
: SK-KEEP ( IR-ARENA:arena n -- IR-ARENA:arena n )
   {: r:IR-ARENA:arena n:n :}
   r n SKELETON
   r n ;

: SKELETON-TRY ( IR-ARENA:arena n -- )
   [: SK-KEEP ;] catch {: rc:n :}
   2drop
   rc 0= if exit then
   RF-RECORD
   rc throw ;

: WALK-KEEP ( IR-ARENA:arena IR-ARENA:arena n -- IR-ARENA:arena IR-ARENA:arena n )
   {: p:IR-ARENA:arena r:IR-ARENA:arena n:n :}
   p r n WALK
   p r n ;

: WALK-TRY ( IR-ARENA:arena IR-ARENA:arena n -- )
   [: WALK-KEEP ;] catch {: rc:n :}
   2drop drop
   rc 0= if exit then
   RF-RECORD
   rc throw ;

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
\
\ THIS IS THE ONE BLOCK WHOSE ARGUMENT TYPES ARE NOT READ OFF AN EDGE, and it is
\ not an exception: no edge reaches the entry block. Its arguments are the
\ CALLER's values, and a Habu word's caller leaves each of them in a data-stack
\ slot, so a cell is what really arrives - a double among them arrives as the
\ cell it is, and the first float word that reads one crosses it. When the
\ checker's own types reach a recorded unit (dot habu-bind-checker-env-ed4f9f87)
\ an argument declared `r` could be stated as a double here and the crossing
\ would be gone rather than placed.
: OPEN-BLOCK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      in:n :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  v key 0 NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   LIT-RESET
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

\ The one table, and the question derived from it. Every reader of the rule - the
\ pre-scan that decides which calls are copied, the splice that copies them, and
\ SPLICEABLE? below - reads these and keeps no second copy of either, so they
\ cannot answer one question two ways. Both are published because that is what
\ makes the claim checkable from outside: a suite can hold the table's own answer
\ for each meaning of the dialect against what the chain then does with it.
EXPORT SPLICE-STAGING
EXPORT SPLICE-MEANING?

\ Could a body copied into a caller hold this tape token? It is the rule the
\ decision above applies to a RECORDED token, asked of a token still on a tape -
\ which is where src/compiler/native/migrate.f asks it, of a definition it has
\ just compiled, to decide whether that definition's body is one worth recording
\ at all. One rule, asked at both ends: the definition that is recorded has to
\ pass it against its own word model, and every caller that copies it has to pass
\ it again against the caller's.
: SPLICEABLE? ( IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena n -- bool )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key r:IR-ARENA:arena ix:n :}
   v ix NTAPE:KIND@ {: kd:NTAPE:kind :}
   kd NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   r  v key ix NTAPE:SPELL@  HIR-WORD:MODELS? 0= if false exit then
   r  v key ix NTAPE:SPELL@  HIR-WORD:MEANING@ SPLICE-MEANING? ;

\ ---- what a recorder has to be told about a call -----------------------------
\ SPLICEABLE? above answers no for every call, and that is right for a token that
\ will really BE a call. It is wrong for one this pass decided to copy: the
\ routine being compiled has the callee's operations in it and no branch, so a
\ recorder that judged the source token would throw away a straight-line routine.
\ These two publish the decision instead, so src/compiler/native/migrate.f writes
\ down what the routine IS rather than what its source said.
\
\ THE DECISION IS NOT RE-DERIVED, IT IS READ. INLINE-SCAN made it once, before
\ any other walk, precisely so that the walks cannot disagree about which sites
\ are calls; a recorder deciding it again would be a fourth reader of a question
\ with one answer. So COPIED? is the table, and it answers about the definition
\ this pass elaborated LAST - which is the one the migration is recording, since
\ the recording stands between COLON and the emission.

\ Was the call written on this body token COPIED into the definition rather than
\ made?
: COPIED? ( n -- bool )
   INL-AT? ;

\ Whose row it was copied from, as the address that row is keyed by. Asked of a
\ token that is not a copied call it is refused by name: an address answered for
\ a token that will really branch would be a body staged where a call belongs.
: COPIED-ENTRY ( IR-ARENA:arena n -- n )
   {: r:IR-ARENA:arena ix:n :}
   ix INL-AT? 0= if E-NELAB-INLINE throw then
   r  VW MKEY ix NTAPE:SPELL@  HIR-WORD:ENTRY@ ;

\ Does the definition this pass last elaborated CALL anything? It is the pre-scan
\ answer above, published because the routine contract the later stages are told
\ turns on it: a definition whose every call was copied into it destroys nothing,
\ needs no frame and saves no return address, and the module the selector reads
\ has to be described by the contract it is selected under.
: CALLED? ( -- bool )
   CALL-NEED @ 0<> ;

\ Does the definition this pass last elaborated make a call control comes BACK
\ from? That is the question the FRAME turns on, and it is not the same question
\ as CALLED? once a body can leave through its final call: the branch that leaves
\ writes no return address, so a definition whose only call is that one destroys
\ nothing of its caller's and needs no frame to keep anything in. A definition
\ that calls somebody else first does, and declares it.
: CALLS-BACK? ( -- bool )
   CALL-BACK @ 0<> ;

\ Does the definition this pass last elaborated LEAVE through its last call? It
\ is published for the same reason CALLED? is: the routine contract the later
\ stages are told turns on it, and the module the selector reads has to be
\ described by the contract it is selected under. The selector re-derives the
\ same answer from the module it built and refuses a disagreement by name, so
\ this is a claim held against a derivation rather than a decision taken twice.
: TAIL-CALLED? ( -- bool )
   TAIL-NEED @ 0<> ;

\ And where the callee it would leave through starts. The migration entry asks,
\ because whether a branch to that address can be PUBLISHED is the publication
\ seam's answer and not this pass's - and the shape has to be decided before the
\ routine contract is built, not refused after the code is emitted. Asked of a
\ definition that leaves through nothing it answers zero, which no code occupies.
: TAIL-ENTRY@ ( -- n )
   TAIL-ENTRY @ ;

\ ---- when the last call need not be come back from ---------------------------
\ A routine can leave THROUGH its final callee - branching instead of calling, so
\ that the callee's own return goes to this routine's caller - exactly when four
\ things are true of the body, and each of them is about what the machine would
\ have to do between the branch and the callee's return, which is nothing.
\
\   THE CALL IS THE LAST THING THE BODY DOES. The tape's final token is a call
\   this pass did not copy into the definition. A copied call is not a call at
\   all - the callee's operations are in this routine and there is no branch to
\   redirect - and WORD-CALL? is the same reading INLINE-SCAN's decision is
\   published through everywhere else, so the two cannot come to disagree.
\
\   NOTHING BRANCHES. A definition with an `exit` or with control flow has more
\   than one block, and the call would then be the last operation of ONE path
\   rather than of the routine. That is a real shape and a real further
\   optimisation, and it is not this one: dot habu-leave-through-a-43dd5bdd.
\
\   THE CALLEE'S RESULTS ARE THIS ROUTINE'S RESULTS. The callee leaves the
\   data-stack pointer one past ITS results and this routine's caller reads them
\   one past OURS, and no instruction can run in between - so the two counts have
\   to be the same number.
\
\   AND THE POINTER IS ALREADY WHERE THE CALLEE IS ENTERED. A call site moves it
\   up over what it hands over; a tail branch cannot, because the move would have
\   to be undone after the branch. The three counts being equal - what this
\   routine takes, what the callee takes, and what either leaves - is what makes
\   every one of the placement's requirements the same place, so the pointer
\   stands there already and the branch is the whole of the site.
\
\ A SELF-CALL IS NOT ONE OF THESE, and not because it could not be: a definition
\ whose only block ends in a call to itself is a loop with no way out, so the
\ shape a tail-recursive routine really has is a guarded one - which is the
\ branching case above and waits on the same dot.
: TAIL-CALLEE? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MEANING@ HIR-MEANING:CALLABLE HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CALLEE-IN@ IN-N @ <> if false exit then
   r sy HIR-WORD:CALLEE-OUT@ OUT-N @ = ;

: TAIL-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 TAIL-NEED !
   0 TAIL-ENTRY !
   CALL-NEED @ CALL-BACK !
   IN-N @ OUT-N @ <> if exit then
   IN-N @ 0= if exit then
   EXIT-USED @ 0<> if exit then
   NB @ 0<> if exit then
   n 2 < if exit then
   r n 1- WORD-CALL? 0= if exit then
   r n 1- TAIL-CALLEE? 0= if exit then
   1 TAIL-NEED !
   r  VW MKEY n 1- NTAPE:SPELL@  HIR-WORD:ENTRY@ TAIL-ENTRY !
   0 CALL-BACK !
   n 1- 1 ?do
      r i WORD-CALL? if 1 CALL-BACK ! leave then
   loop ;

\ Elaborate the one colon definition this sealed tape holds, and answer the
\ function it became. The arenas are, in order, the tape's sealed view, the word
\ model's pick pool and the word model's rows; the two counts are the values the
\ word takes and the values it leaves. Every identity read off the tape is
\ checked against this builder's module by the table that owns it, so a tape of
\ another module cannot be elaborated into this one.
: COLON ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena n n -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena in:n out:n :}
   RF-RESET
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
   ARG-RESET
   in IN-N !
   out OUT-N !
   r n LOCALS-SCAN
   r n RESOLVE-SCAN
   r n INLINE-SCAN
   r n MEM-SCAN
   r n CROSS-SCAN
   r n SKELETON-TRY
   c b v key in out OPEN-FUN
   c b v key in OPEN-BLOCK
   TOK-NEED @ 0<> if 0 EMIT-MEM then
   0 EXIT-PENDING !
   p r n WALK-TRY
   CS-N @ 0<> if E-NELAB-CTRL throw then
   EXIT-PENDING @ 0<> if E-NELAB-CTRL throw then
   EXIT-USED @ 0<> if
      VN @ out <> if E-NELAB-ARITY throw then
      0 EXIT-ORD @ 0 0 0 TERM-BR-H
      NB @ EXIT-ORD @ <> if E-NELAB-CTRL throw then
      0 out 0 0 0 OPEN-ARGS-H
   then
   c b v key out EMIT-RETURN
   r n TAIL-SCAN
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN ;

\ ---- what the last elaboration refused ---------------------------------------
\ The readers of the record above, for a caller that has just caught a refusal
\ out of COLON and needs to know which token it was about, and the clear that
\ makes the answer honest for a caller driving more than one definition. They
\ answer about the last elaboration that reached COLON, which the row decides:
\ COLON clears the row before it reads anything, so a definition that compiled
\ and a definition refused somewhere other than a body token both answer "no
\ record". An attempt refused BEFORE COLON is reached never runs that clear -
\ see the section above - so a driver clears the record itself.
\
\ WHY THE KIND IS A QUESTION AND NOT AN ANSWER. There is no token kind that
\ means "no token", so a reader that handed one back would have to invent a
\ sentinel and every caller would have to know it. Asking whether the refused
\ token was of a given kind has a true answer in every state: with no record,
\ no token was refused, so it was not of that kind either.

\ Which body row the last elaboration's refusal stood on, or -1 when it refused
\ nothing, or refused something that is not a body token - a declared arity, the
\ shape of the tape, or a control structure still open when the body ended.
: REFUSED-ROW ( -- n )
   RF-ROW @ ;

\ Was that token of this tape kind? False whenever there is no record.
: REFUSED-KIND? ( NTAPE:kind -- bool )
   {: k:NTAPE:kind :}
   RF-ROW @ 0 < if false exit then
   0 RF-KIND @ k NTAPE-KIND:EQ ;

\ Its spelling, as the tape interned it. Empty when there is no record, and
\ empty when the spelling was longer than the record holds - REFUSED-ROW
\ separates the two, and truncating rather than answering nothing would name a
\ word other than the one that was refused.
: REFUSED$ ( -- ptr u8 n )
   RF-ROW @ 0 < if RF-BUF 0 exit then
   RF-BUF RF-U @ ;

\ The longest spelling REFUSED$ can answer, so a caller that copies the answer
\ away can size its own buffer from this rather than from a number of its own.
: REFUSED-CAP ( -- n )
   RF-CAP ;

\ Throw the record away. A driver that compiles many definitions in one process
\ calls this before EACH attempt, and then what it reads afterwards describes
\ that attempt or nothing: an attempt refused before elaboration begins - the
\ engine rejecting the source, a reader refusing it, anything that never reaches
\ COLON - leaves the readers above answering "no record" instead of answering
\ the last definition that did reach it. Reasoning that such a refusal carries a
\ code the driver would not have asked about is not the same guarantee: it
\ depends on which codes can come out of where, and this does not.
: REFUSED-RESET ( -- )
   RF-RESET ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
