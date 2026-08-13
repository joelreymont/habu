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
\ structured control words, the three tag-dispatch forms - `MATCH … ;MATCH`,
\ `case … endcase` and `construct` - the cell and byte memory words, `RECURSE`,
\ a call to a declared callee or a declared data word, one `{: … :}` group
\ of typed locals read by name, and the return-stack transfers. Nothing else.
\
\ WHAT IT DOES NOT TRANSLATE, because a reader deciding whether a program can
\ compile here should not have to infer it from silence: character literals,
\ `does>`, `+loop`, `leave` and `j`. The modeled vocabulary is the table in
\ src/compiler/native/hir-word.f and it is the authority; anything absent from
\ it is E-HIR-UNMODELED.
\
\ THIS PARAGRAPH WAS WRONG BEFORE IT WAS THIS, and the correction is worth
\ recording because the list is a contract a reader is entitled to trust. It
\ named plain `do`, quotations and `execute` as untranslated long after all
\ three had entered the table - test/compiler/native-do.f runs plain `do`
\ against the engine on pinned inputs and has since it was written. A list kept
\ by hand beside a table drifts from it; this one is now checked against the
\ table by test/compiler/native-hir.f's declaration-order walk, which names the
\ vocabulary's ends.
\
\ AND THE RETURN-STACK TRANSFERS EMIT NOTHING, WHICH IS THE WHOLE OF THEM. `>r`,
\ `r>`, `r@` and their pair forms move value ids between the two compile-time
\ vectors below and stage no operation at all, so the machine never learns that
\ any of them happened. A parked value crosses a join, a loop edge and a call
\ with the values on the data vector and by the same machinery; what that
\ machinery cannot re-derive is the SPLIT between the two vectors, so the control
\ frame carries it and the join openers state it. The seams are named where they
\ stand: R-SPILL and R-FILL below, the frame's `rd`, `armr` and `xr` fields, and
\ the FRONT operand group at a call.
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
require src/compiler/native/clobber.f
require src/compiler/native/hir.f
require src/compiler/native/hir-word.f
require src/compiler/native/string.f
require src/compiler/native/inline.f
require src/compiler/native/frozen.f
require src/compiler/native/trap.f
require src/compiler/native/family.f

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

\ ---- the word a body token names ---------------------------------------------
\ Which word this tape row is about, as the word model's own key: the fold of the
\ spelling the tape recorded, which is the fold the ENGINE applied when it decided
\ what the token meant (src/compiler/native/hir-word.f KEY-SYM carries the
\ argument). Every pass below that asks the model anything asks it about this, so
\ a body may spell a word of the dialect in any case the engine accepts and all of
\ the passes agree about which word it wrote.
\
\ WHAT DOES NOT COME THROUGH HERE, AND WHY EACH ONE IS RAW. The refusal record and
\ a string literal's body are the BYTES the source wrote - a refusal that renamed
\ the word it is about would name a word nobody wrote, and a folded string literal
\ would be different text. A `{: … :}` local's name is raw because the engine's
\ own local lookup compares those bytes raw, so folding here would bind a name the
\ engine keeps apart. And the definition's own name is the name the engine
\ published. Those four read NTAPE:SPELL@ directly and say so by doing it.
: WSYM ( n -- IR-ID:ir-symbol-id )
   {: ix:n :}
   CTX BLD  VW MKEY ix NTAPE:SPELL@  HIR-WORD:KEY-SYM ;

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
   VW r ix  ix WSYM  HIR-WORD:ADMIT-TOKEN
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

\ A REFUSAL THIS FILE MAKES ITSELF STILL HAS TO NAME ITS TOKEN. ADMIT-AT marks
\ the token only while the word model is answering, because that is the only
\ throw it is standing in front of; a refusal raised after the model has answered
\ - which is every refusal about a word the model DOES know - leaves the mark
\ clear and the record empty.
\
\ AND IT TAKES THE RECORD ITSELF RATHER THAN LEAVING IT TO A HANDLER, because the
\ handlers that take one wrap the two WALKS and a pre-scan runs before either.
\ Marking without recording would name the token for a refusal made during the
\ skeleton and leave a pre-scan's refusal nameless, which is the same diagnostic
\ arriving in two qualities depending on which pass happened to notice. Taking it
\ here makes the answer the same from anywhere; a walk handler that then takes it
\ again reads the same token and copies the same bytes.
: QUOT-REFUSE ( n -- )
   RF-AT !
   RF-RECORD
   E-NELAB-QUOT throw ;

\ ---- the compile-time value vector -------------------------------------------
\ How deep the data stack may get inside one straight-line body. Sixty-four is
\ far past anything hand-written Forth reaches; a body that wants more is a
\ capability to raise here, not a ceiling to widen silently.
64 constant VMAX

\ ---- which entries of the vector are cells of ONE value ----------------------
\ THE VECTOR COUNTS CELLS AND THE LANGUAGE COUNTS VALUES, and for almost every
\ program those are the same number. A value of a layout family is where they
\ part: it occupies several stack cells, so it takes several entries here, and
\ those entries are not independent - reordering them, or moving one without the
\ others, destroys the value. A rename is exactly a permutation of this vector,
\ so a rename reaching such an entry compiles a program that takes a value apart
\ while every count still adds up and nothing refuses. That was measured as four
\ working definitions the chain turned into wrong ones (dot
\ habu-rename-over-rows-982167af).
\
\ SO THE VECTOR CARRIES A SECOND FACT PER ENTRY: whether this cell is part of a
\ multi-cell value. It is a bitmask rather than an array because the vector's own
\ ceiling is sixty-four entries, so one cell holds one bit for each of them, and
\ the two can never disagree about how many entries exist.
\
\ WHERE THE BITS COME FROM, WHICH IS THE WHOLE OF IT. Nothing this file computes
\ makes a value wider than a cell: every literal, every operation result and
\ every block argument is one cell. Only two things put a wider value on the
\ vector - the definition's own arguments, and a call's results - and both arrive
\ from the checker's declared effect through src/compiler/native/dict.f. So the
\ mask is written in exactly two places and is otherwise carried along.
\
\ AND WHAT IT IS FOR IS A REFUSAL, NOT A REPAIR. Moving whole values would mean
\ this vector knowing where each value begins and ends across joins, loop edges
\ and returns as well; that is dot habu-rename-rows-row-143c0331. Until it lands
\ a rename that reaches a bundled cell is refused by name, which turns a silently
\ wrong program into one that does not compile.

\ ---- and which entries name a quotation body ---------------------------------
\ THE THIRD FACT PER ENTRY, AND IT RIDES ON THE ENTRY FOR THE SAME REASON THE
\ GLUE DOES. `[:` leaves one value - the address of another function of this
\ emission - and what that function TAKES AND LEAVES is stated nowhere near it:
\ the numbers belong to the term that consumes the value, which the walk reaches
\ later. So something has to carry "this cell is body k" from the one place to
\ the other.
\
\ IT IS THE CELL AND NOT THE VALUE THAT CARRIES IT, WHICH WAS MEASURED. The first
\ form of this asked whether the value a consumer holds IS the value `hir.quot`
\ defined. It is not, past a call: CALL-OPERANDS+ hands the WHOLE vector over and
\ CALL-CLOSE takes it back as the operation's results, so every value that merely
\ SURVIVES a call comes back a new one - while a call the inliner copied leaves
\ them untouched. A body's arity would then be known or lost depending on whether
\ a call stood between the `[:` and its consumer and whether that call was
\ copied, which is the one thing an optimisation may never decide. The vector
\ entry survives both: a call puts its survivors back in the positions they came
\ from, and a rename permutes positions and says which went where.
\
\ AND EVERY OTHER MOTION CLEARS IT, WHICH IS THE FAIL-CLOSED DIRECTION. A block
\ argument, an operation's result, a literal, a local read - none of them is a
\ quotation this definition wrote, so each starts unmarked, exactly as it starts
\ unglued. A quotation carried across a branch is therefore a cell nobody can
\ name a body for, and it is refused as one nothing consumed rather than
\ compiled under whichever arm was walked first.
-1 constant VQ-NONE                  \ this entry names no quotation body

here CELL 1- and CELL swap - CELL 1- and allot
variable VN                          \ how many values the vector holds
variable VGLUE                       \ bit i set: vector entry i is a cell of a multi-cell value
VMAX TYPED-BUFFER VSTK IR-ID:ir-value-id
VMAX TYPED-BUFFER VWIN IR-ID:ir-value-id
create VQ    VMAX cells allot        \ the quotation body entry i names, or VQ-NONE
create VQWIN VMAX cells allot        \ the same for the window a rename consumed
create VQSAV VMAX cells allot        \ and for the entries a call hands over and takes back

\ The RETURN vector's storage. Its operations and the whole argument for why it
\ needs none of the three facts per entry that the data vector carries are below,
\ under "the compile-time RETURN vector"; only the declarations are here, so that
\ VRESET can empty both in one place and no caller can empty one and forget the
\ other.
16 constant RMAX                     \ measured: the tree's deepest nest is ten
variable RN                          \ how many values the return vector holds
RMAX TYPED-BUFFER RSTK IR-ID:ir-value-id

: VRESET ( -- )
   0 VN !
   0 RN !
   0 VGLUE !
   VMAX 0 ?do  VQ-NONE i cells VQ + !  loop ;

\ Ordinary pushes are one whole value that names no body, so they clear their own
\ bit and their own mark rather than inheriting whatever the entry held when the
\ vector last reached this depth.
: VPUSH ( IR-ID:ir-value-id -- )
   {: val:IR-ID:ir-value-id :}
   VN @ VMAX >= if E-NELAB-CAP throw then
   val VN @ VSTK !
   VGLUE @  1 VN @ lshift invert and  VGLUE !
   VQ-NONE VN @ cells VQ + !
   VN @ 1+ VN ! ;

\ A run of `n` set bits at the bottom of a cell. It is written once because three
\ readers want it and one of them wants it at the ceiling: shifting by the word
\ size is reduced modulo that size on this machine, so a run as long as the
\ vector would come back as a run of ONE if the arithmetic were repeated at each
\ site.
: VRUN-MASK ( n -- n ) {: n:n :}
   n 0 <= if 0 exit then
   n VMAX >= if -1 exit then
   1 n lshift 1 - ;

\ Just the low `n` bits of a mask: what a record of the vector's own glue holds
\ when the vector is n entries deep, so a stored mask never carries bits for
\ entries that were not there.
: VGLUE-LOW ( n n -- n ) {: mask:n n:n :}
   mask  n VRUN-MASK  and ;

\ Whether ONE entry is a cell of a multi-cell value. The run tests above answer
\ about a span; this answers about a position, which is what a reader checking
\ that a bundle ENDS where a declaration says it does has to ask.
: VGLUE-AT? ( n -- bool ) {: i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   VGLUE @  1 i lshift  and 0<> ;

\ Every entry of a run stops being a cell of a multi-cell value. It is what a
\ dispatch arm does to the cells it keeps: they were part of the scrutinee's
\ bundle and the bundle is gone, so what they are now is the arm's own question.
: VGLUE-CLEAR ( n n -- ) {: base:n n:n :}
   VGLUE @  n VRUN-MASK base lshift invert and  VGLUE ! ;

\ Mark a run of entries already on the vector from a row's own mask, whose bit i
\ is the i-th cell from the bottom of that row. The run starts at `base`, so the
\ row's bit i is this vector's bit base+i.
: VGLUE-RUN ( n n -- ) {: base:n mask:n :}
   mask 0= if exit then
   VGLUE @  mask base lshift or  VGLUE ! ;

\ Whether any entry from `base` to the top is a cell of a multi-cell value.
\ It masks off the entries BELOW base rather than building a window of the ones
\ above it, because the window's width would be VN-base and a vector filled to its
\ ceiling makes that the whole word - a shift the machine reduces modulo the word
\ size, which would answer "nothing bundled" for the one case that holds the most.
\ Masking downwards only ever shifts by base, which is strictly less than VN.
: VGLUE-ABOVE? ( n -- bool ) {: base:n :}
   base VN @ >= if false exit then
   VGLUE @ VN @ VGLUE-LOW
   1 base lshift 1 - invert and 0<> ;

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

\ The body one entry names, and the one writer of it: `[:` marks the entry it
\ just pushed. The mark survives a crossing through VAT! above, because a
\ crossing puts back the same eight bytes read as the other type and an address
\ read as an address is what it already was.
: VQ@ ( n -- n )
   {: i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   i cells VQ + @ ;

: VQ! ( n n -- )
   {: k:n i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   k i cells VQ + ! ;

\ What a call hands over and takes back. The whole vector goes out as operands
\ and the survivors come back as results in the positions they left from, so the
\ marks are put aside before the vector is emptied and laid back over the
\ survivors afterwards - which is what CALL-CLOSE does with the glue, one line
\ further down, and for the same reason.
: VQ-SAVE ( -- )
   VN @ 0 ?do  i cells VQ + @  i cells VQSAV + !  loop ;

: VQ-KEEP ( n -- )
   {: n:n :}
   n 0 ?do  i cells VQSAV + @  i cells VQ + !  loop ;

\ ---- the compile-time RETURN vector ------------------------------------------
\ `>r` does not compile to anything. The engine's own `>r` writes a cell into a
\ data-region stack and bumps a depth counter (src/habu/habu2.f J-TOR); this pass
\ moves a value id from one compile-time vector to another and emits no
\ instruction at all, exactly as a rename moves one within a single vector. The
\ parked value goes on living wherever the register allocator already keeps live
\ values, so the return-stack region is never read, never written, and its depth
\ counter never moves in code this chain emits.
\
\ WHAT MAKES THAT SOUND IS A PROOF THE CHECKER HAS ALREADY DONE, and it is worth
\ stating exactly because everything here rests on it. The return stack's DEPTH
\ AT EVERY POINT OF A CERTIFIED BODY IS A COMPILE-TIME NUMBER. src/core/checker.f
\ carries a typed return row beside the data row (RCUR/RBROW), unifies it at
\ every branch join and every loop edge, and refuses a definition that does not
\ leave it exactly as it found it. So a body that reaches this pass cannot pop
\ what it did not push, cannot leave a cell behind, cannot grow the row a turn at
\ a time, and cannot arrive at a join with two arms disagreeing about the depth -
\ every one of those is refused before the tape is built. This vector therefore
\ never has to ask how deep it is at run time, because nothing can make the
\ answer vary.
\
\ AND IT RIDES THE DATA VECTOR ACROSS EVERY SEAM RATHER THAN BESIDE IT. A join, a
\ call and a return each already have machinery that hands the data vector over,
\ agrees its width, agrees which cells are bundled, and crosses a value whose
\ type differs from the one the destination stated. The parked values need all
\ four of those and nothing else, so at each seam they are pushed onto the data
\ vector, carried by that machinery unchanged, and taken back off at the far end.
\
\ WHAT RIDING ALONG CANNOT CARRY IS THE SPLIT, and that is the one thing this
\ costs. Once the two vectors are one list, no arithmetic at the far end recovers
\ how many of its entries were parked - which vector a value belongs to is a fact
\ about the value, not about its position - so the split is carried explicitly
\ everywhere the width is: ARG-R@ records it per block beside the width and the
\ glue, and the control frame's `rd`, `armr` and `xr` are the parked halves of
\ `depth`, `arm` and `xd` so that a join opener can state it. Two arms carrying
\ the same TOTAL and disagreeing about how it splits is then a named refusal
\ rather than a guess.
\
\ THE ORDER IS BOTTOM FIRST, which is what makes a pair form work: `2>r` moves the
\ top two cells keeping the lower one lower, so the vector's own order is the
\ order the return stack has them in, and `2r>` puts them back by reading the
\ same two positions in the same direction.
\
\ SIXTEEN IS THE CEILING AND IT IS A MEASUREMENT. The deepest return-stack nest
\ anywhere in the tree is ten, in lib/process-pty-handle.f COMMIT; every other
\ definition in src, lib, tools, test and maki uses four or fewer. A body that
\ wants more is a capability to raise here, not a ceiling to widen silently.
\
\ AND NOTHING CHECKED CAN REACH IT TODAY, which is worth writing down rather than
\ leaving for the next reader to discover. Seventeen `>r`s in one body is refused
\ by the CHECKER before this file sees the tape, so the E-NELAB-CAP below is an
\ assertion about this vector's own storage and not a refusal a program can
\ provoke. It stays because the storage is real and a ceiling nobody checks is a
\ silent overwrite the day a second producer of tapes appears.
\
\ The i-th parked value from the bottom. One reader, so an index outside what the
\ vector holds is one refusal rather than several - the rule VAT keeps.
: RAT ( n -- IR-ID:ir-value-id )
   {: i:n :}
   i 0 < i RN @ >= or if E-NELAB-UNDER throw then
   i RSTK @ ;

: RPUSH ( IR-ID:ir-value-id -- )
   {: val:IR-ID:ir-value-id :}
   RN @ RMAX >= if E-NELAB-CAP throw then
   val RN @ RSTK !
   RN @ 1+ RN ! ;

: RDROP ( n -- )
   {: k:n :}
   k 0 < k RN @ > or if E-NELAB-UNDER throw then
   RN @ k - RN ! ;

\ How the parked values get across a seam. SPILL COPIES them onto the top of the
\ data vector, bottom first, and leaves the return vector alone; the seam then
\ runs the machinery that was already there, over a vector that is data-then-
\ parked; and the copies come off again with `RN @ VDROP`. FILL is the far side:
\ the destination takes the top k arrivals into its own return vector.
\
\ SPILL COPIES RATHER THAN MOVES, AND THAT IS WHAT MAKES A STUB SAFE. The block a
\ two-way branch splits off hands over the same values the arm below it goes on
\ to read, so a seam that EMPTIED the return vector would leave the sibling path
\ with nothing parked and no way to know it. Copying means the source side's own
\ state is exactly what it was before the seam, which is the property STUB-H's
\ whole existence depends on.
\
\ NEITHER CARRIES GLUE OR A QUOTATION MARK, and that is not an omission:
\ RSTACK-STEP below refuses to park a cell holding either, so a parked value can
\ only ever be a plain cell. VPUSH clears both facts for exactly the entries this
\ pushes, which is the answer those entries need rather than a fact being lost.
: R-SPILL ( -- )
   RN @ 0 ?do  i RAT VPUSH  loop ;

: R-FILL ( n -- )
   {: k:n :}
   VN @ k < if E-NELAB-UNDER throw then
   0 RN !
   k 0 ?do  VN @ k - i + VAT RPUSH  loop
   k VDROP ;

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
\ THE WINDOW MAY NOT REACH A CELL OF A MULTI-CELL VALUE. A rename names its
\ inputs by depth and puts back whichever it likes, so a window holding one cell
\ of a value can drop it, duplicate it, or move it away from the cells it belongs
\ with - and the picks are counted in cells, so the arity still balances and
\ nothing further down would object. The test is on the whole window rather than
\ only on its lower edge: a value lying entirely inside the window is just as
\ easily permuted as one straddling it, and `swap` over two adjacent two-cell
\ values is exactly that case.
\
\ ONE PICK IS PUT BACK WITH THE MARK THE WINDOW ENTRY IT NAMES CARRIES. A rename
\ is the one motion that MOVES a quotation rather than making or consuming one,
\ so it is the one place a mark travels sideways: `swap` over a quotation and a
\ number leaves the quotation still named, one entry down.
: RENAME-PICK ( n -- )
   {: w:n :}
   w VWIN @ VPUSH
   w cells VQWIN + @  VN @ 1-  VQ! ;

: RENAME ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena sym:IR-ID:ir-symbol-id :}
   r sym HIR-WORD:INPUTS@ {: in:n :}
   r sym HIR-WORD:PICKS {: picks:n :}
   in VN @ > if E-NELAB-UNDER throw then
   VN @ in - {: base:n :}
   base VGLUE-ABOVE? if E-NELAB-BUNDLE throw then
   in 0 ?do
      base i + VAT  i VWIN !
      base i + VQ@  i cells VQWIN + !
   loop
   in VDROP
   VN @ picks + VMAX > if E-NELAB-CAP throw then
   picks 0 ?do
      in 1- p r sym i HIR-WORD:PICK@ -  RENAME-PICK
   loop ;

\ ---- the return-stack transfers ----------------------------------------------
\ `>r`, `r>`, `r@` and their pair forms. Each moves whole cells between the two
\ compile-time vectors and stages nothing, so the whole of the lowering is here.
\
\ THE TWO REFUSALS ARE ABOUT WHAT A CELL IS, not about how many there are. A cell
\ of a MULTI-CELL value may not be parked, for the reason a rename may not permute
\ one: the value's cells would be separated, every count would still add up, and
\ nothing further down would object - the same silent wrongness dot
\ habu-rename-over-rows-982167af measured, reached by the other door. And a cell
\ carrying a QUOTATION MARK may not be parked, because the mark says "this entry
\ is body k of this emission" and only the data vector's own motions carry it;
\ parking one would hand its consumer a cell nobody can name a body for. Both are
\ refused by name rather than repaired, which turns a wrong program into one that
\ does not compile. The whole window is tested, not only its lower edge, because a
\ value lying entirely inside a `2>r` pair is as easily separated as one
\ straddling it.
\
\ ONE OF THE TWO IS REACHED BY REAL SOURCE AND THE OTHER IS NOT, and the
\ difference is worth writing down because they share a code. The MULTI-CELL
\ clause is unreachable from checked source today: `2>r` over a two-cell layout
\ value is refused by the CHECKER, which reads that value as ONE term of two
\ cells while the transfer's axiom takes two terms, so the tape never carries the
\ shape. The QUOTATION clause fires on the tree as it stands - src/core/combinators.f
\ BI and TRI park a quotation parameter with `>r`, and the census measures both
\ arriving here. They are the two definitions this refusal is FOR, and their real
\ blocker is the multishot-quotation capability (dot
\ habu-multishot-quotations-typed-8832cace) rather than anything about parking.
\ Sharing E-NELAB-BUNDLE between the two clauses is imprecise - a quotation mark
\ is not a bundle - and a code of its own is worth minting the next time this
\ file's error vocabulary is opened.
: RSTACK-CK ( n -- )
   {: base:n :}
   base VGLUE-ABOVE? if E-NELAB-BUNDLE throw then
   VN @ base ?do  i VQ@ VQ-NONE <> if E-NELAB-BUNDLE throw then  loop ;

: TO-R ( n -- )
   {: cells:n :}
   cells VN @ > if E-NELAB-UNDER throw then
   VN @ cells - {: base:n :}
   base RSTACK-CK
   RN @ cells + RMAX > if E-NELAB-CAP throw then
   cells 0 ?do  base i + VAT RPUSH  loop
   cells VDROP ;

: FROM-R ( n -- )
   {: cells:n :}
   cells RN @ > if E-NELAB-UNDER throw then
   VN @ cells + VMAX > if E-NELAB-CAP throw then
   RN @ cells - {: base:n :}
   cells 0 ?do  base i + RAT VPUSH  loop
   cells RDROP ;

\ A peek is a pop that does not take: the same cells arrive on the data vector
\ and the return vector keeps them, so a body may read a parked value as often as
\ it likes and still owes exactly one `r>`.
: FETCH-R ( n -- )
   {: cells:n :}
   cells RN @ > if E-NELAB-UNDER throw then
   VN @ cells + VMAX > if E-NELAB-CAP throw then
   RN @ cells - {: base:n :}
   cells 0 ?do  base i + RAT VPUSH  loop ;

: RSTACK-STEP ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena sym:IR-ID:ir-symbol-id :}
   r sym HIR-WORD:RSTACK-CELLS@ {: cells:n :}
   r sym HIR-WORD:RSTACK@
   MATCH HIR:rmove
      to-r    OF cells TO-R ENDOF
      from-r  OF cells FROM-R ENDOF
      fetch-r OF cells FETCH-R ENDOF
   ;MATCH ;

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
\ WHY THE GROUPS ARE FOUND BEFORE THE WALK. Two walks read the body - the
\ skeleton that counts blocks and the build that makes them - and both of them
\ meet tokens that are neither dialect words nor literals: the declared names,
\ and every later mention of one. Asking the word model about either is a
\ refusal, so both walks have to know which rows are a declaration and which
\ names are locals before they start. The pre-pass answers both by recording
\ each group's two ENDS as tape indices, so the two walks share one derivation
\ of where the groups are instead of each keeping a state machine that could
\ drift; and the build checks its own arrival at a closer against the index the
\ pre-pass recorded for the group it is binding next, which is the same
\ two-derivations discipline SKELETON keeps.
\
\ AS MANY GROUPS AS THE BODY WRITES, EACH AT THE TOP LEVEL, READ-ONLY. A
\ definition may open a group wherever it stands, and the tree writes them that
\ way constantly - bind the arguments, compute, name the results, compute again.
\ The names one group declares come into scope at its OWN closer and stay in
\ scope for the rest of the body, so the locals a given row can see are always a
\ PREFIX of the declared names, and LBN is the length of that prefix. Every
\ reader that hands locals across a seam reads that prefix rather than the whole
\ table.
\
\ AND THE PREFIX IS STABLE ACROSS A CONTROL STRUCTURE, WHICH IS WHAT LETS THE
\ SEAMS KEEP READING ONE COUNT. A group whose closer stands inside an open
\ structure is refused (dot habu-scope-a-locals-2faa3d7a), so the prefix can only
\ grow where no structure is open - and every edge of a structure therefore sees
\ the same prefix its opener saw. That is a fact about the rules rather than
\ about the corpus: it is what makes CROSS-L one number at both ends of an edge.
\
\ A GROUP INSIDE A CONTROL STRUCTURE AND AN UNCLOSED GROUP ARE STILL REFUSED BY
\ NAME, and so is a name the dialect already models (dot
\ habu-decide-what-a-9f38a8f6). Rebinding a local and taking its address need no
\ refusal here at all - no such word is in the dialect's vocabulary, so `to` and
\ `^` are already refused as words this dialect cannot compile. Dots
\ habu-rebind-a-typed-b2a3e369 and habu-take-the-addr-18a38b4f carry the two
\ capabilities.
16 constant LMAX                     \ locals, and groups, one definition may declare
64 constant LNAME-CAP                \ bytes one declaration spelling may hold

here CELL 1- and CELL swap - CELL 1- and allot
variable LN                          \ how many locals were declared
variable LG-N                        \ how many groups the pre-pass found
variable LG-OPEN                     \ the group the pre-pass is reading, or -1
variable LG-K0                       \ the first name index of that open group
variable LGB                         \ how many groups the walk has bound
variable LBN                         \ how many declared locals are bound
LMAX TYPED-BUFFER LNAME IR-ID:ir-symbol-id
LMAX TYPED-BUFFER LVAL IR-ID:ir-value-id
create LCROSS LMAX cells allot       \ whether a call can reach a mention of this local
\ THE QUOTATION ROW A LOCAL'S VALUE NAMES, carried on the NAME and not on the
\ value. Binding a name and reading it are both pushes onto the compile-time
\ vector, and a push clears the vector entry's mark - correctly, because most
\ pushes are not quotations - so a quotation bound to a local would lose its row
\ at the binding and every read of the name would be a cell nobody can name an
\ arity for. The local is what the walk really moves here, so the fact rides on
\ the local, exactly as it rides on the vector entry everywhere else.
create LQ LMAX cells allot           \ the quotation row this local's value names, or VQ-NONE
create LROW LMAX cells allot         \ the row the group declaring this local closes on
create LG-A LMAX cells allot         \ the row each group's `{:` is on
create LG-B LMAX cells allot         \ the row its `:}` is on, or -1 while it is open
create LG-K LMAX cells allot         \ how many names it declares
create LBUF LNAME-CAP allot

: LRESET ( -- )
   0 LN !
   0 LG-N !
   -1 LG-OPEN !
   0 LG-K0 !
   0 LGB !
   0 LBN !
   LMAX 0 ?do
      0 i cells LCROSS + !
      VQ-NONE i cells LQ + !
   loop ;

: LAT ( n -- n )
   dup 0 < over LN @ >= or if E-NELAB-LOCAL throw then ;

: LQ@ ( n -- n )     LAT cells LQ + @ ;
: LQ! ( n n -- )     {: k:n i:n :}  k i LAT cells LQ + ! ;

\ The row at which this local comes into scope: the one its group's `:}` is on,
\ or -1 while that group is still being read.
: LROW@ ( n -- n )
   LAT cells LROW + @ ;

\ The write is not bounded here, exactly as the name's own store is not: the
\ declaration that fills both is the one that raises LN, and its ceiling is
\ DECLARE-LOCAL's cap check. Reads are bounded, which is where an index that
\ came from somewhere else arrives.
: LROW! ( n n -- )
   {: row:n k:n :}
   row k cells LROW + ! ;

\ Whether this local is a name that row may use: its group has closed, and
\ closed before the row.
: LIVE-AT? ( n n -- bool )
   {: k:n ix:n :}
   k LROW@ {: r:n :}
   r 0 < if false exit then
   r ix < ;

\ One group's three facts, each read through the same bound. A group index the
\ pre-pass has not filled is refused rather than answered.
: LGAT ( n -- n )
   dup 0 < over LG-N @ >= or if E-NELAB-LOCAL throw then ;

: LG-A@ ( n -- n )
   LGAT cells LG-A + @ ;

: LG-B@ ( n -- n )
   LGAT cells LG-B + @ ;

: LG-K@ ( n -- n )
   LGAT cells LG-K + @ ;

\ Whether ONE of this body's calls keeps no register for the caller, which is the
\ half of "does this local travel" that is about the CALLEES rather than about
\ where the name is written. CROSS-SCAN writes it, once, before the walk starts;
\ it stands here because this is where it is read.
variable CALL-BARE

\ Whether this local's value has to TRAVEL - be handed over at every call and
\ taken back from it, and carried across every block edge in between. CROSS-SCAN
\ below decides both halves of the answer for the whole definition before the
\ walk starts, and the section above CS-PENDING says what turns on it.
\
\ TWO FACTS MEET HERE AND NEITHER IS ENOUGH ALONE. The mark says a call can reach
\ a mention of this name, which is a property of where the name is written.
\ CALL-BARE says one of this body's calls keeps no register for the caller, which
\ is a property of the CALLEES. A local only has to travel when both are true: a
\ value a call can reach still needs somewhere to be, and a register the callee
\ leaves alone is somewhere - the allocator keeps it out of the destroyed ones
\ (src/compiler/native/regalloc.f MB-FORBID) and the validator refuses the
\ allocation if it did not (regalloc-verify.f CLOB-AT). Travelling is what buys a
\ DATA-STACK SLOT instead, and that is only worth its price when there is no
\ register to be had.
\
\ AND THE PRICE IS WHY THIS IS A QUESTION AT ALL. A travelling local is an
\ operand and a result of every call it survives and a block argument of every
\ block on the path, and those are what put its class beyond MB-SPILLABLE? -
\ measured in tools/codegen-spill-probe.f, which straddles both walls.
\
\ THE SECOND FACT IS READ HERE AND NOT WRITTEN AT THE MARK, and that is not a
\ style choice. The scan meets the calls in tape order, so a body whose LAST call
\ is the bare one would have marked nothing by the time it reached it. What the
\ scan records is reachability, which is final the moment the row is walked; what
\ this word does is price it, which is only answerable once the whole body has
\ been seen. One reader, one place, and every consumer of the mark - CROSS-L, the
\ operand and argument lists, the results taken back, and the binding that puts a
\ travelling double into a cell - inherits the same answer.
: LCROSS? ( n -- bool )
   LAT cells LCROSS + @ 0<>
   CALL-BARE @ 0<> and ;

: LCROSS+ ( n -- )
   LAT cells LCROSS +  1 swap ! ;

\ Is this tape row part of a declaration - a group's opener, or one of the names
\ after it? A closer is not: it is the row that does the binding.
: IN-DECL? ( n -- bool )
   {: ix:n :}
   false
   LG-N @ 0 ?do
      ix i LG-A@ >=  ix i LG-B@ <  and or
   loop ;

\ Which declared local this row names, or a negative answer. The comparison is
\ between interned identities of one module, so it is an identity question and
\ not a search for text.
\
\ IT IS ALSO A QUESTION ABOUT WHERE THE ROW STANDS. A name is that local only
\ from its group's closer onwards; before it, the same spelling is whatever else
\ the body means by it - a call to a word of that name, most obviously - and
\ reading it as the local would refuse a program the engine compiles.
: LOCAL-OF ( n -- n )
   {: ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if -1 exit then
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   -1
   LN @ 0 ?do
      sy i LNAME @ NFROZEN:SAME-SYM?  i ix LIVE-AT?  and if drop i leave then
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
\ BOTH VECTORS ARE ASKED, because both of them go into data-stack slots at a
\ call and a slot is a cell. A parked value is as able to be a double as a value
\ on the data vector is - `f+ >r` parks one - so a crossing skipped there is the
\ same wrongness skipped here, and one refusal covers the two.
: NO-REAL-CK ( -- )
   VN @ 0 ?do
      i VAT REAL-VALUE? if E-NELAB-TYPE throw then
   loop
   RN @ 0 ?do
      i RAT REAL-VALUE? if E-NELAB-TYPE throw then
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

\ THE PARKED VALUES AS CELLS, for the same sentence and the same reason. A call
\ site hands every live value through a data-stack slot, a parked value is live
\ across the call, and a slot is sixty-four bits with no register file - so a
\ parked double crosses as the cell it is and comes back a cell that the next
\ float word crosses again. It replaces the value in the return vector in place,
\ exactly as CROSS1 does in the data vector, because the crossing consumes one
\ value and answers one and the parked value's position is not what changes.
: R-CROSS ( n -- )
   {: ix:n :}
   RN @ 0 ?do
      i RAT REAL-VALUE? if
         ix  i RAT  HIR-OPCODE:REALBITS CROSS-VALUE  i RSTK !
      then
   loop ;

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
create LIT-KIND LITMAX cells allot    \ and what the number IS
LITMAX TYPED-BUFFER LIT-ID IR-ID:ir-value-id
variable LIT-N

: LIT-RESET ( -- )
   0 LIT-N ! ;

\ The memo as a scope, which is what a stub needs. Rows are only ever appended,
\ so the count IS the mark: releasing to a mark drops exactly the rows added
\ since it was taken and leaves every earlier row the value it already held.
: LIT-MARK ( -- n )
   LIT-N @ ;

\ A release only ever DROPS rows. It is written as the smaller of the two counts
\ rather than as a store because the memo has a second way of shrinking - the
\ barrier below empties it at a call nothing survives - and a mark taken before
\ such a call would otherwise put the emptied rows back, still holding the value
\ ids the call consumed. Dropping too much costs a fold; putting a row back costs
\ a value that has to live where no register may hold it.
: LIT-RELEASE ( n -- )
   {: m:n :}
   m LIT-N @ > if exit then
   m LIT-N ! ;

\ ---- and what a call does to it ----------------------------------------------
\ A CALL WHOSE CALLEE PUBLISHED NOTHING DESTROYS EVERY REGISTER, so a value
\ defined before it and read after it has nowhere to be: the allocator bars the
\ whole pool for a class that crosses such a call (regalloc.f MB-FORBID over a
\ callee with no row) and refuses the definition with E-A64RA-POOL. A literal is
\ the one value that never has to cross anything - it has no inputs, so the far
\ side of the call can simply stage it again for the price of the move that
\ materialises it. So the memo is emptied there, and the second mention of a
\ number becomes a second literal instead of a live range no register may hold.
\
\ THE RULE IS THE ONE CALL-KEEPS? ALREADY STATES ABOUT LOCALS, asked the same way
\ of the same fact: a callee with a clobber record leaves registers a crossing
\ value can sit in, and the fold across it is the corpus win the memo was
\ measured for, so it is kept. Only the call that keeps nothing empties the memo.
\
\ IT IS ASKED OF THE ENTRY ADDRESS rather than passed in by the three callers
\ that stage a call, because the address is what the record is keyed on and one
\ of those callers - `execute` - has no name to ask about at all.
: LIT-CALL-BARRIER ( n -- )
   {: entry:n :}
   entry NCLOB:KNOWN? if exit then
   LIT-RESET ;

\ Which memo row holds this literal, or -1.
\
\ THE KEY IS THE KIND AND THE NUMBER, NOT THE NUMBER. A `create`d word's data
\ field and an ordinary integer that happens to equal it are the same sixty-four
\ bits and two different literals: one is an address the relocation pass must
\ rewrite when its region moves, the other is a number that must survive
\ untouched. Keyed on the number alone this memo folds them into ONE operation,
\ and whichever kind that operation ended up carrying is then wrong for one of
\ the two references - silently, because the fold is invisible by design.
: LIT-FIND ( n n -- n )
   {: kind:n val:n :}
   -1
   LIT-N @ 0 ?do
      i cells LIT-VAL + @ val =
      i cells LIT-KIND + @ kind = and if drop i leave then
   loop ;

: LIT-REMEMBER ( n n IR-ID:ir-value-id -- )
   {: kind:n val:n id:IR-ID:ir-value-id :}
   LIT-N @ LITMAX >= if exit then
   val LIT-N @ cells LIT-VAL + !
   kind LIT-N @ cells LIT-KIND + !
   id LIT-N @ LIT-ID !
   LIT-N @ 1+ LIT-N ! ;

\ ---- the things a body token becomes -----------------------------------------
\ One integer literal, staged at the span of the token named. The value is the
\ whole content of a constant, so it rides as the attribute the opcode's schema
\ requires. It takes the value rather than reading it off the token, because a
\ constant-and-operation word's constant is the word model's and not the tape's.
\ A number this block has already staged is not staged again: the memo above
\ answers with the value the first one defined.
\
\ AND IT TAKES THE KIND FOR THE SAME REASON IT TAKES THE VALUE. Whether the
\ number is an address is the WORD MODEL's answer too, and this is the last pass
\ that can ask - below here it is sixty-four bits like any other. The schema
\ requires the kind, so a staging that forgot it is refused by IR-OP rather than
\ producing a literal nobody can classify later.
: STAGE-LIT ( n n n -- )
   {: ix:n val:n kind:n :}
   CTX BLD HIR-OPCODE:CONST HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op OPERANDS+
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-VALUE  CTX BLD val IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD HIR:KEY-ADDR  CTX BLD kind HIR:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE ;

: EMIT-KIND-LIT ( n n n -- )
   {: ix:n val:n kind:n :}
   kind val LIT-FIND {: j:n :}
   j 0 >= if j LIT-ID @ VPUSH exit then
   ix val kind STAGE-LIT
   kind val  VN @ 1- VAT  LIT-REMEMBER ;

\ An ordinary number: what the source wrote, or what a word model carries as a
\ plain constant. Every caller that is not staging an address goes through here,
\ so "not an address" is stated once instead of at each of them.
: EMIT-LIT ( n n -- )
   {: ix:n val:n :}
   ix val HIR:ADDR-NONE EMIT-KIND-LIT ;

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
   r ix  ix WSYM  EMIT-OP-SYM ;

\ A word that pushes one fixed value - the address a `create`d data word names,
\ or the number a `constant` names. The value is the word model's, so this stages
\ the same operation an integer literal in the source would, and WHAT the number
\ is comes from the same row: a data word's is an address of DATA, and this is
\ the point at which that is still known, while a constant's is an ordinary
\ number and is staged exactly as the digits would have been. Below here both are
\ numbers, which is why the kind cannot be recovered further down and is not
\ re-derived there.
: EMIT-FIXED-SYM ( IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:FIXED-VALUE@  r sy HIR-WORD:FIXED-KIND@  EMIT-KIND-LIT ;

: EMIT-FIXED ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix  ix WSYM  EMIT-FIXED-SYM ;

\ ---- a string literal ----------------------------------------------------------
\ WHAT ONE COMPILES TO, AND WHY IT IS NOTHING NEW. The checker certifies `s"` as
\ ( -- ptr u8 n ): an address and a length. The length is a number. The address
\ is the address of bytes that outlive the routine, which is exactly what a
\ `create`d data word's address is - and EMIT-FIXED above already stages one of
\ those as an ordinary integer literal. So a literal stages two constants and
\ needs no operation, no machine form and no encoder this dialect did not already
\ have.
\
\ THE BYTES ARE INTERNED HERE AND NOT AT PUBLICATION, because the address is an
\ OPERAND: instruction selection has to materialise it, so it has to be a number
\ before the module is built, and there is no later point at which it could be
\ filled in - src/compiler/native/emit.f keeps no relocation list because there is
\ nothing to patch afterwards. What makes that safe against a refusal further
\ down is that the store is keyed by CONTENT, so interning is idempotent: a
\ definition the allocator refuses and the pipeline re-elaborates allocates
\ nothing the second time. src/compiler/native/string.f carries that argument.
\
\ THE BODY IS COPIED OUT OF THE MODULE'S INTERNER, which is where the tape put
\ it: a string row's spelling IS its body. A body longer than this buffer is
\ refused by the interner's own copier with its own name, so there is no second
\ ceiling here to disagree with it.
$1000 constant SB-CAP
create SB-BUF SB-CAP allot

: STRING-BODY ( n -- ptr u8 n ) {: ix:n :}
   VW MKEY ix NTAPE:SPELL@ {: sy:IR-ID:ir-symbol-id :}
   SB-BUF  CTX BLD sy SB-BUF SB-CAP IR-BUILD:SYMBOL-COPY ;

: EMIT-STRING ( n -- ) {: ix:n :}
   ix STRING-BODY {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   ix  a u NSTR:INTERN  HIR:ADDR-DATA  EMIT-KIND-LIT
   ix  u  EMIT-LIT ;

\ A word that is one constant and one operation - `1-` is `1` then `-`. Both
\ halves come off the word model's row, so a second opcode meaning the same
\ thing is not needed and the source stays one token.
: EMIT-CONST-OP-SYM ( IR-ARENA:arena n IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:CONST-VALUE@  EMIT-LIT
   ix  r sy HIR-WORD:CONST-OPCODE@  EMIT-OPCODE ;

: EMIT-CONST-OP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix  ix WSYM  EMIT-CONST-OP-SYM ;

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

\ THE RETURN VECTOR IS EMPTY HERE, AND THAT IS ASKED RATHER THAN ASSUMED. A
\ routine leaves through this operation, and a parked value at that moment is a
\ cell the caller's own return stack would have to hold - which this chain never
\ writes, because the whole lowering is that a parked value stays wherever the
\ register allocator keeps live values. The checker has already refused every body
\ that leaves the return row anything but as it found it, so nothing is meant to
\ reach this refusal; it is asked for the reason NO-REAL-LOCAL-CK is asked, since
\ reaching it means the one fact the lowering rests on stopped being true and the
\ alternative is a routine that returns with a cell nobody can name.
: EMIT-RETURN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      out:n :}
   VN @ out <> if E-NELAB-ARITY throw then
   RN @ 0<> if E-NELAB-JOIN throw then
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
\
\ AND THREE MORE, WHICH ARE `depth`, `arm` AND `xd` ASKED ABOUT THE OTHER VECTOR.
\ A seam hands over the data values AND the parked ones, so each of the three
\ numbers a join opener needs is really a PAIR - how many data values and how many
\ parked - and the second half has to be carried the same way the first is. It
\ cannot be re-derived at the opener and it cannot be taken from the walk's live
\ `RN`: an arm may pop what the `if` parked (`1 >r if r> drop 2 else r> drop 3
\ then` leaves nothing parked at its join and one at its `else`), so the parked
\ depth AT A JOIN is no more the frame's opening depth than the data depth is.
\ `rd` is what the structure opened holding, which is what its own stub carries;
\ `armr` is what the first arm of an `if` left, beside `arm`; and `xr` is what the
\ first `while` of a loop left, beside `xd`.
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
create CS-END CMAX cells allot       \ whether the arm before this frame's `else` ended
create CS-W CMAX cells allot         \ cells one value of a tag-dispatch frame's subject occupies
create CS-TRAP CMAX cells allot      \ the ordinal a `MATCH` mismatch traps with
create CS-OFIX CMAX cells allot      \ the row of the `of` that opened the arm being read, or -1
create CS-JOINED CMAX cells allot    \ whether any arm of this frame reached its join
create CS-RD CMAX cells allot        \ parked values the return vector held when this structure opened
create CS-ARMR CMAX cells allot      \ parked values the first arm of an `if` left, beside CS-ARM
create CS-XR CMAX cells allot        \ parked values the first `while` of a loop left, beside CS-XD
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
\ DO-ENTER writes all three before its own frame is read, and clearing them
\ would need a value id this file has no way to mint.
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
   0 t cells CS-END + !
   0 t cells CS-W + !
   0 t cells CS-TRAP + !
   -1 t cells CS-OFIX + !
   0 t cells CS-JOINED + !
   RN @ t cells CS-RD + !
   -1 t cells CS-ARMR + !
   -1 t cells CS-XR + !
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
: CS-END@ ( n -- n )      cells CS-END + @ ;
: CS-W@ ( n -- n )        cells CS-W + @ ;
: CS-TRAP@ ( n -- n )     cells CS-TRAP + @ ;
: CS-OFIX@ ( n -- n )     cells CS-OFIX + @ ;
: CS-JOINED? ( n -- bool ) cells CS-JOINED + @ 0<> ;
: CS-RD@ ( n -- n )       cells CS-RD + @ ;
: CS-ARMR@ ( n -- n )     cells CS-ARMR + @ ;
: CS-XR@ ( n -- n )       cells CS-XR + @ ;

: CS-JOIN! ( n n -- )     cells CS-JOIN + ! ;
: CS-ARM! ( n n -- )      cells CS-ARM + ! ;
: CS-ARMR! ( n n -- )     cells CS-ARMR + ! ;
: CS-XD! ( n n -- )       cells CS-XD + ! ;
: CS-XR! ( n n -- )       cells CS-XR + ! ;
: CS-EXIT! ( n n -- )     cells CS-EXIT + ! ;
: CS-END! ( n n -- )      cells CS-END + ! ;
: CS-W! ( n n -- )        cells CS-W + ! ;
: CS-TRAP! ( n n -- )     cells CS-TRAP + ! ;
: CS-OFIX! ( n n -- )     cells CS-OFIX + ! ;
: CS-JOINED+ ( n -- )     cells CS-JOINED +  1 swap ! ;

\ One more `while` has been met by the loop this frame is.
: CS-WHILE+ ( n -- )
   {: t:n :}
   t CS-NW@ 1+  t cells CS-NW + ! ;

\ Whether an `else` has been met, which is the one question both walks ask of
\ `arm` whatever each of them stores in it.
: CS-ELSE? ( n -- bool )
   CS-ARM@ 0 >= ;

\ Is this frame one of the two tag-dispatch forms, and is it the one over a
\ family? `of` and `endof` belong to whichever of them is open - the source
\ language gives them one spelling each and the engine and the checker both
\ decide by the open structure - so every reader of an arm asks the frame.
: CS-ADT? ( n -- bool ) {: t:n :}
   t CS-KIND @ HIR-CTRL:OPEN-MATCH HIR-CTRL:EQ
   t CS-KIND @ HIR-CTRL:OPEN-CASE HIR-CTRL:EQ or ;

: CS-MATCH? ( n -- bool )
   CS-KIND @ HIR-CTRL:OPEN-MATCH HIR-CTRL:EQ ;

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
\ the second is what `do` or `?do` took off it and the body may no longer see;
\ the third is what a `{: … :}` group named; the fourth is what a load and a
\ store pass along. Everything in this file that CARRIES live values - a branch's operands,
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
\
\ ONE KIND ANSWERS FOR BOTH OPENERS OF A COUNTED LOOP, because a frame records
\ the STRUCTURE and `do` and `?do` open one - an index, a limit, and `loop` for a
\ closer. DO-ENTER is the only word in this file that pushes such a frame, so
\ there is no second spelling for this question to miss.
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
\ CROSS-SCAN worked out for the whole definition. Only the bound PREFIX counts -
\ a name whose group has not closed yet holds no value to carry, whatever the
\ declaration said - and before the first group has closed the prefix is empty.
: CROSS-L ( -- n )
   0
   LBN @ 0 ?do  i LCROSS? if 1+ then  loop ;

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
   LBN @ 0 ?do
      i LCROSS? if
         i LVAL @ REAL-VALUE? if E-NELAB-TYPE throw then
      then
   loop ;

: LOCAL-OPERANDS+ ( n -- )
   dup NO-REAL-LOCAL-CK
   LOCAL-CK 0= if exit then
   LBN @ 0 ?do
      i LCROSS? if CTX BLD  i LVAL @  IR-BUILD:ADD-OPERAND then
   loop ;

: LOCAL-ARGS+ ( n -- )
   LOCAL-CK 0= if exit then
   LBN @ 0 ?do
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
variable IN-GLUE                     \ which of those it takes are cells of a multi-cell value
variable OUT-GLUE                    \ and which of those it leaves are
variable FR-GIN                      \ what the caller staged for the definition about to be compiled
variable FR-GOUT
variable EXIT-USED                   \ whether the body has an `exit` at all
variable EXIT-ORD                    \ the block every `exit` and the fall-through reach

\ ---- a path that has already ended -------------------------------------------
\ TWO WORDS END A PATH AND THEY END IT DIFFERENTLY, so what is remembered is
\ WHICH, not merely THAT. An `exit` branches to the return block: the arm's block
\ is closed and one edge into the return exists. A call to a word the checker
\ certified as never returning closes the arm's block too, but there is no edge
\ anywhere - control is gone. The two agree on everything the join has to do
\ (this arm hands the join nothing and contributes no width) and disagree about
\ everything else: whether the definition needs a return block, whether `else`
\ may follow, and whether the body may simply stop.
\
\ IT IS ONE RECORD BECAUSE THERE IS ONE FACT. Two flags could say a path both
\ exited and died, and every reader would then have to decide which it believed.
\ A single cell holding one of three answers cannot.
0 constant PATH-LIVE                 \ the walk is on a path that goes on
1 constant PATH-EXIT                 \ an `exit` closed the arm; only its `then` may follow
2 constant PATH-DEAD                 \ a call that does not come back closed it
variable PATH-END

: PATH-ENDED? ( -- bool )
   PATH-END @ PATH-LIVE <> ;

: PATH-DEAD? ( -- bool )
   PATH-END @ PATH-DEAD = ;

: EXIT-RESET ( -- )
   0 EXIT-USED !
   -1 EXIT-ORD !
   PATH-LIVE PATH-END ! ;

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
create ARG-G NFROZEN:BMAX cells allot   \ which of those positions are cells of a multi-cell value
create ARG-R NFROZEN:BMAX cells allot   \ how many of the TOP positions are parked return values
ARG-CAP TYPED-BUFFER ARG-T IR-ID:ir-type-id  \ the type each of those positions has
VMAX TYPED-BUFFER XV IR-ID:ir-value-id  \ what the edge being staged really hands over

: ARG-RESET ( -- )
   NFROZEN:BMAX 0 ?do
      -1 i cells ARG-N + !  0 i cells ARG-G + !  0 i cells ARG-R + !
   loop ;

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

\ Which of a block's argument positions are cells of a multi-cell value. A value
\ that is one value on one side of an edge is one value on the other, so this
\ crosses with the width and the types rather than being rederived after the
\ join - without it every bundled value would arrive at a join looking like
\ unrelated cells and a rename below it would no longer refuse.
: ARG-G@ ( n -- n )
   ARG-BLOCK-CK cells ARG-G + @ ;

\ How many of a block's argument positions - counted from the TOP, which is where
\ R-SPILL put them - are parked return values rather than data-stack values. It
\ crosses with the width for the reason the glue does: which vector a value
\ belongs to is a fact about the value, and re-deriving it after the join is not
\ possible at all. The WIDTH agreement already refuses two arms that carry
\ different totals; this refuses two that agree on the total and disagree about
\ how it splits, which is a program the checker cannot produce and this file must
\ still not compile into a guess.
: ARG-R@ ( n -- n )
   ARG-BLOCK-CK cells ARG-R + @ ;

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
   VGLUE @  n VGLUE-LOW  t ARG-BLOCK-CK cells ARG-G + !
   RN @ t ARG-BLOCK-CK cells ARG-R + !
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
\ loop's index and limit are integers, and DO-PAIR refuses a double at either
\ opener, so a double in either is E-NELAB-TYPE before the loop's header is ever
\ opened. A bound local that crosses anything is a CELL by
\ construction - DO-CLOSE-LOCALS puts it in one, because a call carries it
\ through a data-stack slot and a slot is a cell. And the memory order has its
\ own type and holds no register at all.
\
\ `n` IS THE WHOLE WIDTH: the data values AND the parked ones above them, which
\ is what the edge into this block handed over and what ARG-WIDTH@ recorded. The
\ caller states it as two numbers added, and every one of them takes the second
\ from its own control frame, because which vector a value belongs to is a fact
\ about the value and no arithmetic here could recover the split. R-FILL below
\ then takes the top ARG-R@ of them off into the return vector, which restores the
\ one invariant this file keeps everywhere else: between seams the DATA vector
\ holds data values only and the RETURN vector holds parked values only.
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
   0 NB @ ARG-G@ VGLUE-RUN
   NB @ ARG-R@ R-FILL
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
   t ARG-R@ RN @ <> if E-NELAB-JOIN throw then
   t ARG-G@  VGLUE @ VN @ VGLUE-LOW  <> if E-NELAB-JOIN throw then
   VN @ 0 ?do
      ix i  t i ARG-T@  EDGE-VALUE  i XV !
   loop ;

\ Hand every live value to one block and end this one. The operands are the
\ vector bottom first, then two per open loop the edge crosses with, then one per
\ local, then the memory order when the definition has one - the four positions
\ OPEN-ARGS-H gives them.
: TERM-BR-H ( n n n n n -- )
   {: ix:n t:n lo:n h:n l:n :}
   R-SPILL
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
   CLOSE-BLOCK
   RN @ VDROP ;

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

\ ---- finding the locals groups -----------------------------------------------
\ One walk of the body before either of the other two, recording where each
\ group is and which names it declares. It asks the word model only about rows
\ the model could answer for: MODELS? is the one reader here that treats an
\ undeclared word as an ordinary answer rather than a refusal, which is exactly
\ what a name the program chose is.
: MODELED-AS? ( IR-ARENA:arena n HIR:meaning -- bool )
   {: r:IR-ARENA:arena ix:n m:HIR:meaning :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ m HIR-MEANING:EQ ;

: DUP-LOCAL? ( IR-ID:ir-symbol-id -- bool )
   {: sy:IR-ID:ir-symbol-id :}
   false
   LN @ 0 ?do
      sy i LNAME @ NFROZEN:SAME-SYM? or
   loop ;

\ ---- the one name a local may not take ----------------------------------------
\ A NAME THE DIALECT ALREADY MEANS IS STILL THE PROGRAM'S, AND THAT IS THE
\ ENGINE'S ANSWER RATHER THAN THIS FILE'S. It was measured, not reasoned about
\ (dot habu-decide-what-a-9f38a8f6):
\
\   : T ( n -- n ) {: i:n :} 0 3 0 ?do i + loop ;               5 T -> 15
\   : U ( n -- n ) drop 0 3 0 ?do i + loop ;                    5 U -> 3
\   : V ( n -- n ) {: j:n :} 0 2 0 ?do 3 0 ?do j + loop loop ;  7 V -> 42
\   : W ( n -- n ) {: dup:n :} dup dup + ;                      5 W -> 10
\   : X ( n -- n ) {: if:n :} if if + ;                         5 X -> 10
\
\ The declared name WINS - over a loop index, over a primitive and over a
\ keyword alike - from its group's closer onwards, and only from there:
\ `0 3 0 ?do i + loop {: s:n i:n :} s i +` answers 8 for 5, the index three
\ times and then the local once. That is LIVE-AT? exactly, and it is what
\ docs/forth.md has always said: locals are lexical and local-first. So the
\ collision needs no rule of this file's own: every pass that reads a body token
\ for what it MEANS asks LOCAL-OF first - RESOLVE-STEP, INLINE-SCAN, MSCAN-STEP,
\ DSCAN-STEP, MEM-SCAN, CROSS-STEP, SK-STEP and STEP. BACK-SCAN is the one
\ reader that does not, and it is the one whose answer cannot be wrong either
\ way: what it decides is whether this body makes a call it comes BACK from, and
\ a bound name it counted as one would keep a frame that is not needed rather
\ than drop one that is.
\
\ ONE PASS CANNOT ASK, AND THAT IS THE WHOLE REFUSAL LEFT. QUOT-SCAN runs before
\ this one, because the spans it finds are what every later walk steps over - so
\ at the moment it reads `[:` and `;]` there is no locals frame to consult, and a
\ body that bound one of those names would have its spans taken from a token that
\ means something else.
\
\ AND OF THE FOUR DELIMITERS ONLY ONE CAN REACH HERE AS A NAME, which is why one
\ test is the whole of it. A local's NAME is the bytes before the annotation's
\ `:` (HIR-WORD:LOCAL-NAME-LEN), so no name contains a colon at all - a group
\ writing `[:` declares `[`, which is nobody's word. `{:` and `:}` never arrive:
\ SCAN-STEP above reads both as what they are before a declaration is reached.
\ And a group that writes `[:` has put its own closer inside the span that token
\ opened, which QLOCALS-CK refuses as the QUOTATION's - test/compiler/native-
\ elaborate.f holds that refusal against its owner rather than letting this one
\ claim it. `;]` carries no colon and opens nothing, so it arrives here, and here
\ is where it is refused.
\
\ THE QUESTION IS ASKED UNDER THE FOLD, and it is the only question here that is.
\ What it decides is how a MENTION of this name will be read by that pre-scan,
\ and a mention reaches the word model through WSYM, which is the spelling's
\ fold. The NAME itself is the bytes the body wrote (below).
: PRE-FRAME? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CTRL@ HIR-CTRL:CLOSE-QUOT HIR-CTRL:EQ ;

\ One declared local: its bare name, interned into this module so that every
\ later mention of it in the body is the same identity. The annotation is cut
\ off by the word model, which owns how a source word of this dialect is
\ spelled.
\
\ THE NAME IS THE BYTES THE BODY WROTE, AND IS NOT FOLDED. Everywhere else this
\ file asks the word model under the spelling's FOLD, because that is the
\ identity the engine's keyword and dictionary compares give a name. A local is
\ the one thing the engine does NOT fold: src/habu/habu2.f EMIT-LOC-FIND compares
\ a local's name byte for byte, and the engine was asked rather than assumed -
\ `: TUP ( n -- n ) {: i:n :} 0 3 0 ?do i + loop ;` answers 15 for 5, the LOCAL,
\ while the same definition written `?do I + loop` answers 3, the loop INDEX. So
\ the name is interned as written and matched as written (LOCAL-OF), and a
\ mention in the OTHER case is not a local here for the same reason it is not one
\ in the engine - it goes to the word model under its key and is the dialect's
\ word. Folding LOCAL-OF would bind a mention the engine gives to something else.
: DECLARE-LOCAL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   LN @ LMAX >= if E-NELAB-LOCAL-CAP throw then
   CTX BLD  VW MKEY ix NTAPE:SPELL@  LBUF LNAME-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   LBUF u HIR-WORD:LOCAL-NAME-LEN {: nu:n :}
   nu 1 < if E-NELAB-LOCAL throw then
   CTX BLD LBUF nu IR-BUILD:INTERN-SYMBOL {: sy:IR-ID:ir-symbol-id :}
   r  CTX BLD sy HIR-WORD:KEY-SYM  PRE-FRAME? if E-NELAB-LOCAL throw then
   sy DUP-LOCAL? if E-NELAB-LOCAL throw then
   sy LN @ LNAME !
   -1 LN @ LROW!
   LN @ 1+ LN ! ;

\ A group opens. Its row is recorded, its names start where the ones declared so
\ far end, and it is the group every row until the closer belongs to. The
\ ceiling is the name table's, because a group that declares nothing still costs
\ a row here.
: GROUP-OPEN ( n -- )
   {: ix:n :}
   LG-N @ LMAX >= if E-NELAB-LOCAL-CAP throw then
   LG-N @ {: g:n :}
   g 1+ LG-N !
   ix g cells LG-A + !
   -1 g cells LG-B + !
   0 g cells LG-K + !
   LN @ LG-K0 !
   g LG-OPEN ! ;

\ And it closes. The row it closes on is where its names come into scope, which
\ is what every later reader asks LIVE-AT? about, so it is written into each of
\ them here rather than derived twice.
: GROUP-CLOSE ( n -- )
   {: ix:n :}
   LG-OPEN @ {: g:n :}
   ix g cells LG-B + !
   LN @ LG-K0 @ -  g cells LG-K + !
   LN @ LG-K0 @ ?do
      ix i LROW!
   loop
   -1 LG-OPEN ! ;

\ One row of the pre-pass. Outside a group the only row that matters is an
\ opener; inside one, every row is a declared name until the closer. A second
\ group is an ordinary group and not a refusal: the names it declares join the
\ ones already declared, and come into scope at its own closer.
: SCAN-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   LG-OPEN @ 0 < if
      r ix HIR-MEANING:OPEN-LOCALS MODELED-AS? if ix GROUP-OPEN then exit
   then
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-LOCAL throw then
   r ix HIR-MEANING:CLOSE-LOCALS MODELED-AS? if ix GROUP-CLOSE exit then
   r ix HIR-MEANING:OPEN-LOCALS MODELED-AS? if E-NELAB-LOCAL throw then
   r ix DECLARE-LOCAL ;

: LOCALS-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   LRESET
   n 1 ?do
      r i SCAN-STEP
   loop
   LG-OPEN @ 0 >= if E-NELAB-LOCAL throw then ;

\ ---- is this row a particular control word? ----------------------------------
\ Asked by three passes below - the tag-dispatch pre-pass, the scan that decides
\ which locals travel, and the one that decides where a body leaves through - so
\ the question about a control word's identity is written once. It answers about
\ the WORD MODEL's row and not about the spelling, exactly as everything else in
\ this file does, so a body that writes `MATCH` and one that writes `match` are
\ the same row here for the same reason they are one keyword in the engine.
: ROW-CTRL? ( IR-ARENA:arena n HIR:ctrl -- bool )
   {: r:IR-ARENA:arena ix:n want:HIR:ctrl :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CTRL@ want HIR-CTRL:EQ ;

\ ---- how far a body token may be from the definition's first ------------------
\ The ceiling every table keyed by a BODY TOKEN shares - the skeleton's forward
\ joins, the tag-dispatch pre-pass below and the inline decision after it - so no
\ two of them can disagree about which token indices exist. A body that wants
\ more is a capability to raise here, not a ceiling to widen silently.
256 constant TMAX                    \ body tokens one definition may have

: TOK-CK ( n -- n )
   dup 0 < over TMAX >= or if E-NELAB-BLOCK throw then ;

\ ---- the bodies this definition defers ---------------------------------------
\ WHAT A QUOTATION IS TO THIS PASS. `[: … ;]` writes a body that is not part of
\ the body around it: it has its own entry, its own return, and it is reached by
\ an address somebody executes. So the tokens between the pair are ANOTHER
\ FUNCTION of this emission, and what the enclosing body holds where `[:` stands
\ is one value - that function's address, staged as `hir.quot`.
\
\ AND THE BODIES ARE BUILT AFTER THE ENCLOSING FUNCTION IS CLOSED, which is not a
\ preference. src/compiler/ir/build.f holds ONE function stage at a time and
\ refuses a second `BEGIN-FUN` while one is open - E-IR-BUILD-STAGE, measured -
\ so a body cannot be built where its `[:` stands. This table is what carries
\ each body from the token that opened it to the loop at the end of COLON that
\ builds it: its span on the tape, and what it takes and leaves. Which vector
\ entry names which body is the vector's own fact and is kept there (VQ, above).
\
\ THE SPANS ARE FOUND BEFORE ANY WALK, for the reason the tag-dispatch pre-pass
\ above gives: three walks read this body and every one of them has to know which
\ tokens are not its own. One derivation, read everywhere.
\
\ THE PAIRING REFUSALS ARE ALL THIS PASS'S NOW. They used to be shared with the
\ walks - a walk that DECLINED every `[:` stood on the very token at fault for an
\ unopened `;]` and for an unclosed `[:`, so this pass made only the refusal a
\ walk could not name, which is a `[:` inside another. A walk that SKIPS a body
\ never meets either token, so both come back here, and each is still named by
\ the token really at fault: the opener that was never closed, and the closer that
\ opened nothing.
\
\ WHY THE NESTED CASE IS NAMED AT THE INNER OPENER. `[: … [: … ;] … ;]` is not a
\ nesting the engine compiles - it ends the process at the inner opener - so no
\ certified body can reach here holding one, and the refusal cannot fire on source
\ the engine accepted. It is still made, because this pass reads a TAPE and a tape
\ is filled by a lexer: a caller that builds one by hand, which is what a fixture
\ does, can present a shape the engine never would.
\
\ HOW MANY BODIES ONE DEFINITION MAY HOLD. Every body is a function of the same
\ module, so the ceiling is the module's own - src/compiler/native/frozen.f FMAX -
\ less the one the definition itself occupies. The measured tree needs nineteen.
\ A definition that wants more is a capability to raise at FMAX, where every pass
\ reads it, and not a ceiling to widen here.
NFROZEN:FMAX 1- constant QMAX        \ quotation bodies one definition may hold

-1 constant QNONE                    \ no consumer has said what this body takes and leaves

here CELL 1- and CELL swap - CELL 1- and allot
variable QN                          \ how many bodies this definition holds
variable QD                          \ the row of the body the pre-scan has open, or -1
variable QBASE                       \ the ordinal of the function the definition itself is
create QAT   QMAX cells allot        \ the `[:` each body was opened at
create QLO   QMAX cells allot        \ its first body token
create QHI   QMAX cells allot        \ its `;]`, which is one past its last body token
create QIN   QMAX cells allot        \ what the body takes, in cells, or QNONE
create QOUT  QMAX cells allot        \ and what it leaves
create QOPENED TMAX cells allot      \ this token opens body k, or -1
\ WHICH FUNCTION OF THIS EMISSION A ROW'S BODY IS, and QPARAM for a row that has
\ no body at all. It used to be derived - the row's index plus the definition's
\ own ordinal plus one - which was true only while every row was a body written
\ here. A quotation that ARRIVES as an argument is a quotation this definition
\ holds and has to know the arity of, and it is no function of this emission, so
\ the two facts came apart and the ordinal is now stated where the row is opened.
\ The loop that builds the bodies asserts it against the builder rather than
\ recomputing it, exactly as it did before.
-1 constant QPARAM                   \ this row's quotation is no body of this emission
create QFUN  QMAX cells allot        \ the function of this emission a row's body is

\ ---- which body each token belongs to, and which body is being walked ---------
\ ONE TABLE SAYING WHOSE TOKEN THIS IS, because the two walks below run once for
\ the definition and once for every body, over ranges of the SAME tape, and each
\ of them has to pass over every token that is not its own. A flag saying only
\ "this token is inside some body" cannot answer that: it is true of exactly the
\ tokens a body's own walk exists to read, so a walk asking it would step over
\ its whole body and build a function out of nothing - which is a routine that
\ returns its arguments, and for a body taking as many cells as it leaves that is
\ an emission no arity check can tell from the real one.
\
\ SO THE QUESTION IS "IS THIS TOKEN MINE", asked against the body a walk is
\ building. QCUR is that body's row, and QOWNER-DEF while the definition's own
\ function is being built - a value no row can take, so the definition is one
\ more owner rather than a special case at every reader.
\
\ THE CLOSER BELONGS TO ITS BODY and the opener does not. `;]` is the token the
\ body's return is staged at, and it is one past the body's walk, so marking it
\ the body's keeps the ENCLOSING walk off it without the body's walk ever
\ reaching it. `[:` is the enclosing body's own token: it is where the value
\ naming the function stands.
-1 constant QOWNER-DEF               \ the definition's own function, which is no body
create QOWN TMAX cells allot         \ the body each token belongs to, or QOWNER-DEF
variable QCUR                        \ the body being walked, or QOWNER-DEF

: QUOT-RESET ( -- )
   0 QN !
   -1 QD !
   QOWNER-DEF QCUR !
   TMAX 0 ?do
      -1 i cells QOPENED + !
      QOWNER-DEF i cells QOWN + !
   loop ;

: QROW-CK ( n -- n )
   dup 0 < over QN @ >= or if E-NELAB-QUOT-CAP throw then ;

: QAT@ ( n -- n )    QROW-CK cells QAT + @ ;
: QLO@ ( n -- n )    QROW-CK cells QLO + @ ;
: QHI@ ( n -- n )    QROW-CK cells QHI + @ ;
: QFUN@ ( n -- n )   QROW-CK cells QFUN + @ ;
: QIN@ ( n -- n )    QROW-CK cells QIN + @ ;
: QOUT@ ( n -- n )   QROW-CK cells QOUT + @ ;

: QOPENED@ ( n -- n )
   TOK-CK cells QOPENED + @ ;

: QOWN@ ( n -- n )
   TOK-CK cells QOWN + @ ;

\ Whether the walk in flight has to pass over this token, which is every token
\ some OTHER function of this emission is made of.
: QSKIP? ( n -- bool )
   QOWN@ QCUR @ <> ;

\ Whether this token is inside a body at all, which is a different question and
\ has one asker: a group closer the enclosing walk will never advance past.
: QINSIDE? ( n -- bool )
   QOWN@ QOWNER-DEF <> ;

\ What a consumer says one body takes and leaves.
\
\ A ROW TOLD TWICE HAS TO BE TOLD THE SAME THING, and the second telling is
\ accepted rather than refused because it carries no new information. A body
\ reaches two consumers when its value was duplicated and handed over twice, and
\ the checker has already proved the two agree: a quotation is ONE term with ONE
\ effect, and a second consumer declaring a different one is refused where the
\ definition is certified. Measured, on the engine's own checker: `[: 1 + ;] dup
\ 2 QA swap 3 QB` with `QA ( [ n -- n ] n -- n )` and `QB ( [ n n -- n ] n -- n )`
\ is rejected at `QB` - "expected n [ n n -- n ] n, actual n [ n -- n ] n" -
\ while the same source with `QA` twice compiles. So refusing the second telling
\ would refuse a program nothing is wrong with, and it would refuse it only
\ SOMETIMES: whether two consumers of one body are recognised as two depends on
\ what stands between them, which is not a thing acceptance may depend on.
\
\ A SECOND TELLING THAT DISAGREES IS STILL REFUSED, because then this file and
\ the checker disagree about a row the checker proved consistent - which is a
\ fault in the descent above, not a program to compile under whichever answer
\ arrived first.
: QFILL ( n n n -- )
   {: k:n in:n out:n :}
   k QIN@ QNONE <> if
      k QIN@ in <>  k QOUT@ out <>  or if k QAT@ QUOT-REFUSE then
      exit
   then
   in k cells QIN + !
   out k cells QOUT + ! ;

\ The bytes a token's name is spelled with. The tape holds an interned symbol and
\ the checker's effect store is asked by NAME, so the spelling is copied out
\ where it is asked for. It is the RAW spelling and not the word model's fold,
\ for the reason WSYM's section gives: this is a question put to the engine and
\ the checker about the word the source really wrote.
128 constant QSPELL-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create QSPELL-BUF QSPELL-CAP allot

: QSPELL ( n -- ptr u8 n )
   {: ix:n :}
   CTX BLD  VW MKEY ix NTAPE:SPELL@  QSPELL-BUF QSPELL-CAP IR-BUILD:SYMBOL-COPY
   QSPELL-BUF swap ;

\ ---- where a body's arity comes from -----------------------------------------
\ NOT FROM THE BODY, AND NOT FROM THE TOKEN THAT OPENED IT. A routine's arity is
\ how many cells its caller hands it and how many it takes back, and nothing where
\ `[:` stands says either number. What says them is the TERM that consumes the
\ value: an operand a callee declares, or a result the enclosing definition
\ declares - and the checker is the only authority on either. So the two readers
\ below are asked at the consumption site and answer through
\ src/compiler/native/dict.f, which descends into the term's own effect.
\
\ THE INDEX IS THE TERM'S, COUNTED FROM THE TOP. The checker numbers a row's
\ terms from the top; the vector holds cells and this walk knows which CELL it is
\ handing over; and the two are the same number only while every term of the row
\ is one cell wide. dict.f refuses the descent when they are not, so a row
\ carrying a term several cells wide answers "no quotation there" and this refuses
\ by name rather than descending into whichever term the arithmetic landed on.
\
\ AND A CELL THAT IS NOT A QUOTATION'S IS PASSED OVER, which is what makes these
\ two loops rather than one test. A call may take a quotation and three ordinary
\ numbers, and only the cells that really are quotation values ask anything of the
\ checker at all.
: QARG-FILL ( n n -- )
   {: ix:n j:n :}
   VN @ 1- j -  VQ@ {: k:n :}
   k 0 < if exit then
   ix QSPELL j NDICT:SPELL-QUOT-DIN {: qi:n qo:n :}
   qi NDICT:QUOT-NONE = if k QAT@ QUOT-REFUSE then
   k qi qo QFILL ;

\ Every cell this call is about to consume. The token is the call's, because that
\ is the row whose declared inputs are being read.
\
\ A SITE THAT DOES NOT HOLD THEM IS LEFT TO ITS OWNER, and that is not politeness
\ but the same rule the rest of this file keeps: both call forms refuse a vector
\ shallower than the callee's arity a moment later and each names it - CALL-LIVE
\ answers E-NELAB-CALL and the splice E-NELAB-UNDER - so a refusal here would
\ replace whichever of those a reader was going to get with a third code for the
\ same fault. Measured: it did, on the migration case that states a callee takes
\ two values where the caller holds one. Nothing is lost by saying nothing,
\ because no body is built out of the rows this fills unless the call is.
: QCALL-FILL ( n n -- )
   {: ix:n a:n :}
   a VN @ > if exit then
   a 0 ?do
      ix i QARG-FILL
   loop ;

: QRET1-FILL ( n -- )
   {: j:n :}
   VN @ 1- j -  VQ@ {: k:n :}
   k 0 < if exit then
   0 QSPELL j NDICT:SPELL-QUOT-DOUT {: qi:n qo:n :}
   qi NDICT:QUOT-NONE = if k QAT@ QUOT-REFUSE then
   k qi qo QFILL ;

\ Every cell this definition is about to leave. The name is the definition's own,
\ read off the one tape row that holds it, because the row being read is the
\ effect the checker accepted for THIS word.
\
\ A VECTOR THAT IS NOT THE DECLARED DEPTH IS LEFT ALONE, because whose refusal
\ that is, is already settled: EMIT-RETURN below holds the body against the
\ declared outputs and names the disagreement E-NELAB-ARITY. Refusing here would
\ answer the same body a different code for the same fault, one step earlier.
: QRET-FILL ( n -- )
   {: out:n :}
   out VN @ <> if exit then
   out 0 ?do
      i QRET1-FILL
   loop ;

\ Every body this definition holds has to have been told. A body nothing consumed
\ has no arity, so there is no function to build and no caller that could ever
\ enter it; compiling it under a guess would put a routine in the emission whose
\ contract nobody stated. It is named by its own `[:`, which is the token a reader
\ can do something about.
\
\ THIS IS ALSO WHERE `is` LANDS, and it lands here by construction rather than by
\ a spelling this file would otherwise have to know. `[: … ;] is FOO` binds the
\ quotation to a deferred word, which is a word that runs while the source is
\ being compiled - so the walk meets it as a token the dialect does not model and
\ refuses THERE, naming `is`. Binding one is dot habu-bind-a-quotation-1ea5f813.
: QCONSUMED-CK ( -- )
   QN @ 0 ?do
      i QFUN@ QPARAM <>  i QIN@ QNONE =  and if i QAT@ QUOT-REFUSE then
   loop ;

: QOPEN-ROW ( n -- )
   {: ix:n :}
   QN @ QMAX >= if E-NELAB-QUOT-CAP throw then
   QN @ {: k:n :}
   ix k cells QAT + !
   ix 1+ k cells QLO + !
   QNONE k cells QIN + !
   QNONE k cells QOUT + !
   QBASE @ k + 1+  k cells QFUN + !
   k ix TOK-CK cells QOPENED + !
   k QD !
   k 1+ QN ! ;

\ ---- a quotation this definition was HANDED ----------------------------------
\ WHY IT NEEDS A ROW AT ALL. `execute` has to know what the routine it enters
\ takes and leaves, and for a body written here that is the row a consumer
\ filled. A quotation that arrives as an ARGUMENT has no body here and no
\ consumer to fill anything - and it is by far the commoner shape: every
\ combinator in src/core/combinators.f, every `A-MAPI!` and `VEC-EACH` and
\ `MAP-EACH` in the library, takes its quotation and executes it.
\
\ AND ITS ARITY HAS THE SAME AUTHOR AS EVERY OTHER ARITY HERE. The enclosing
\ definition DECLARED it - `( ptr a len [ idx a -- a ] -- )` says exactly what
\ that quotation takes and leaves - and the checker is the authority on the
\ declaration, asked through the same descent a callee's operand is asked
\ through (src/compiler/native/dict.f SPELL-QUOT-DIN), over this definition's
\ own name. So one table holds every quotation the definition has, one column
\ holds every arity, and `execute` reads the row whatever put it there.
\
\ IT IS NO FUNCTION OF THIS EMISSION, which is what QPARAM says: the body is
\ somewhere else entirely, compiled by whoever wrote it, and the loop that
\ builds this definition's bodies passes over the row.
\
\ TERMS ARE COUNTED FROM THE TOP AND CELLS FROM THE BOTTOM, so term j is cell
\ in-1-j - the same correspondence QARG-FILL uses over a callee's row, and one
\ dict.f refuses the descent for when the two counts disagree.
: QOPEN-PARAM ( n n -- )
   {: j:n cellix:n :}
   0 QSPELL j NDICT:SPELL-QUOT-DIN {: qi:n qo:n :}
   qi NDICT:QUOT-NONE = if exit then
   QN @ QMAX >= if E-NELAB-QUOT-CAP throw then
   QN @ {: k:n :}
   0 k cells QAT + !
   0 k cells QLO + !
   0 k cells QHI + !
   qi k cells QIN + !
   qo k cells QOUT + !
   QPARAM k cells QFUN + !
   k 1+ QN !
   k cellix VQ! ;

\ Every declared input of this definition, marked where it is a quotation. The
\ cells are already on the vector, so this only says which of them the body may
\ execute and what happens when it does.
: QPARAMS-OPEN ( n -- )
   {: in:n :}
   in 0 ?do
      i  in 1- i -  QOPEN-PARAM
   loop ;

\ The open row's span, closed. The row is checked rather than trusted because the
\ ONE store below is the only unchecked write into these tables: `QD` is -1 when
\ no body is open, and -1 cells past QHI is somebody else's memory. The pre-scan
\ refuses a closer with nothing open before it gets here, so this can only fire
\ if that refusal is gone - which makes it the wall that keeps a deleted
\ diagnostic a loud failure instead of a quiet corruption.
: QCLOSE-ROW ( n -- )
   {: ix:n :}
   QD @ QROW-CK {: k:n :}
   ix k cells QHI + !
   ix  k QLO@  ?do
      k i TOK-CK cells QOWN + !
   loop
   k ix TOK-CK cells QOWN + !
   -1 QD ! ;

: QSCAN-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:OPEN-QUOT ROW-CTRL? if
      QD @ 0 >= if ix QUOT-REFUSE then
      ix QOPEN-ROW
      exit
   then
   r ix HIR-CTRL:CLOSE-QUOT ROW-CTRL? if
      QD @ 0 < if ix QUOT-REFUSE then
      ix QCLOSE-ROW
   then ;

: QUOT-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   QUOT-RESET
   n 1 ?do
      r i QSCAN-STEP
   loop
   QD @ 0 >= if QD @ QAT@ QUOT-REFUSE then ;

\ ---- the three tag-dispatch forms, read before anything else reads a word -----
\ WHAT MAKES THEM DIFFERENT FROM EVERY OTHER FORM IN THIS FILE. `MATCH option
\ some OF … ENDOF … ;MATCH` and `construct option some` write two tokens that
\ denote nothing a dictionary holds: `option` is a type family and `some` is one
\ of its variants, and both are rows of the checker's type-family registry. So
\ the walk cannot ask what it asks about every other token, and a pass that
\ guessed from the spelling would be a second opinion about which token is an
\ operand. What decides it is POSITION, exactly as it decides it in the two
\ authorities that already read these forms: the engine's compile-time token
\ machine (src/habu/habu2.f, the CMM cell) and the checker's (src/core/checker.f,
\ the MM cell) both consume the operand tokens by counting from the keyword, and
\ this pass is the third reader of the same grammar.
\
\ AND IT IS THE SAME GRAMMAR RATHER THAN A COPY OF IT, which is what the modes
\ below say: `construct` wants a family and then a variant; `MATCH` wants a
\ family, then a variant or `;MATCH`, then `of`. The numbers are the engine's own
\ mode numbers, so a reader holding the two files side by side is comparing one
\ machine with itself.
\
\ WHAT THE PASS WRITES DOWN, AND WHY IT IS A TABLE KEYED BY THE TOKEN. Three
\ walks read this body afterwards - the pre-scans, the skeleton and the build -
\ and every one of them meets the operand tokens and would ask the word model
\ about a name the model cannot answer for. They all read the role recorded here
\ instead, and the two walks that BUILD read the numbers beside it: a variant's
\ tag, the zero pads its bundle carries, and how many cells its payload is. One
\ derivation, four readers, exactly as the locals groups and the inline decision
\ are done.
\
\ A NAME IN A COMMENT OR IN A STRING IS NOT A TOKEN OF THIS GRAMMAR, and that is
\ structural rather than careful. The tape holds what the checker's reader
\ CONSUMED: a parenthesised comment is not a token at all, and a string literal
\ is one token whose KIND is string-literal, so neither can stand in an operand
\ position - the pass requires the row after a keyword to be a NAME and refuses
\ anything else by name.
0 constant MR-NONE                   \ an ordinary body token
1 constant MR-FAMILY                 \ the family operand of a `MATCH` or a `construct`
2 constant MR-VARIANT                \ the variant operand of one
\ The deferred word `is` binds to. It is the same KIND of row as the two above -
\ a name the body wrote that denotes nothing this dialect models, standing in a
\ position a keyword decides - so it is one more role rather than a table of its
\ own, and every walk passes over it through the one question they all already
\ ask. What separates it from an ordinary name is only that: read as a body
\ word it would resolve to the deferred word and compile into a CALL to it,
\ which is not what `[: … ;] is FOO` does to FOO.
3 constant MR-DEFER                  \ the deferred word `is` binds to

\ The modes, which are the engine's CMM numbers (src/habu/habu2.f
\ EM-COMPILE-ADT-MODE) under this file's names.
0 constant MM-OFF                    \ no operand token is expected
1 constant MM-CON-FAM                \ `construct` has been read; its family is next
2 constant MM-CON-VAR                \ and then its variant
3 constant MM-FAM                    \ `MATCH` has been read; its family is next
4 constant MM-VARIANT                \ a variant token, or the `;MATCH` that ends the form
5 constant MM-OF                     \ the `of` that opens the arm of the variant just read

0 constant MK-MATCH                  \ the open form is a `MATCH`
1 constant MK-CASE                   \ the open form is a `case`

128 constant MTOK-CAP                \ bytes of one operand token this pass can read

here CELL 1- and CELL swap - CELL 1- and allot
create MROLE TMAX cells allot        \ which operand, if any, this row is
create MTAG TMAX cells allot         \ an arm's variant tag, a `MATCH` row's trap ordinal, a `construct` row's tag
create MPAD TMAX cells allot         \ the zero pads an arm drops, or a `construct` pushes
create MPAY TMAX cells allot         \ the payload cells an arm keeps, or a `construct` consumes
create MWID TMAX cells allot         \ a `MATCH` row's bundle width in cells
create MONE TMAX cells allot         \ whether an arm's payload cells are ONE value
create MEND TMAX cells allot         \ whether this `of` opens the LAST arm of its `MATCH`
create MTOK MTOK-CAP allot

CMAX constant MSMAX                  \ open tag-dispatch forms, as the control stack's own ceiling
create MS-KIND MSMAX cells allot     \ MK-MATCH or MK-CASE
create MS-FAM MSMAX cells allot      \ the family a `MATCH` frame is over
create MS-ARM MSMAX cells allot      \ whether the pass is inside one of its arms
create MS-OF MSMAX cells allot       \ the row of the `of` that opened the arm read last
variable MSN                         \ how many forms are open
variable MM                          \ which operand token the pass is expecting
variable CB-ROW                      \ the `MATCH` or `construct` row being read
variable CB-FAM                      \ and the family its first operand named
variable MV-ROW                      \ the variant row read last, whose `of` is next

\ Only the two columns a later reader consults for a row this pass may not have
\ written are cleared: every other column is written by the step that later reads
\ it. A role of MR-NONE is what an ordinary token has, and a `MATCH` arm is the
\ last one only if the `;MATCH` that ended the form said so.
: MATCH-RESET ( -- )
   MM-OFF MM !
   0 MSN !
   -1 CB-ROW !
   -1 MV-ROW !
   TMAX 0 ?do
      MR-NONE i cells MROLE + !
      0 i cells MEND + !
   loop ;

: MROLE@ ( n -- n )   TOK-CK cells MROLE + @ ;
: MTAG@ ( n -- n )    TOK-CK cells MTAG + @ ;
: MPAD@ ( n -- n )    TOK-CK cells MPAD + @ ;
: MPAY@ ( n -- n )    TOK-CK cells MPAY + @ ;
: MWID@ ( n -- n )    TOK-CK cells MWID + @ ;
: MONE@ ( n -- bool ) TOK-CK cells MONE + @ 0<> ;
: MEND@ ( n -- bool ) TOK-CK cells MEND + @ 0<> ;

\ Is this row an operand of a tag-dispatch form? Every walk after this pass asks
\ it, and a row it answers for is passed over exactly as a locals declaration is:
\ the body chose that spelling and no dictionary word is what it means.
: MOPERAND? ( n -- bool )
   MROLE@ MR-NONE <> ;

: MROLE! ( n n -- ) {: ix:n r:n :}
   r ix TOK-CK cells MROLE + ! ;

: MARM! ( n n n n -- ) {: ix:n tag:n pads:n pay:n :}
   tag ix TOK-CK cells MTAG + !
   pads ix TOK-CK cells MPAD + !
   pay ix TOK-CK cells MPAY + ! ;

\ What a `MATCH` row carries: how many cells one value of its family is, and the
\ ordinal a mismatch over it traps with.
: MMATCH! ( n n n -- ) {: ix:n w:n ord:n :}
   w ix TOK-CK cells MWID + !
   ord ix TOK-CK cells MTAG + ! ;

: MONE! ( n bool -- ) {: ix:n one:bool :}
   one if 1 else 0 then  ix TOK-CK cells MONE + ! ;

: MEND! ( n -- ) {: ix:n :}
   1 ix TOK-CK cells MEND + ! ;

\ ---- the forms this pass has open --------------------------------------------
: MS-AT ( n -- n )
   dup 0 < over MSN @ >= or if E-NELAB-MATCH throw then ;

: MS-TOP ( -- n )
   MSN @ 1- MS-AT ;

: MS-PUSH ( n n -- ) {: kind:n fam:n :}
   MSN @ MSMAX >= if E-NELAB-BLOCK throw then
   kind MSN @ cells MS-KIND + !
   fam MSN @ cells MS-FAM + !
   0 MSN @ cells MS-ARM + !
   -1 MSN @ cells MS-OF + !
   MSN @ 1+ MSN ! ;

: MS-POP ( -- )
   MSN @ 1 < if E-NELAB-MATCH throw then
   MSN @ 1- MSN ! ;

: MS-MATCH? ( n -- bool )   MS-AT cells MS-KIND + @ MK-MATCH = ;
: MS-FAM@ ( n -- n )        MS-AT cells MS-FAM + @ ;
: MS-ARM? ( n -- bool )     MS-AT cells MS-ARM + @ 0<> ;
: MS-OF@ ( n -- n )         MS-AT cells MS-OF + @ ;

: MS-ARM! ( n n -- ) {: t:n ofix:n :}
   1 t MS-AT cells MS-ARM + !
   ofix t MS-AT cells MS-OF + ! ;

: MS-ARM-END! ( n -- ) {: t:n :}
   0 t MS-AT cells MS-ARM + ! ;

\ ---- one operand token -------------------------------------------------------
\ Its bytes, which is what the registry compares. A token longer than this buffer
\ is refused by the interner's own copier with its own name, and no family the
\ registry holds can be spelled that long - src/compiler/native/trap.f sizes its
\ arena from the tree's longest family tail, 31 bytes, and a package qualifier in
\ front of one leaves this buffer four times the room it needs.
: MTOK$ ( n -- ptr u8 n ) {: ix:n :}
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-MATCH throw then
   MTOK  CTX BLD  VW MKEY ix NTAPE:SPELL@  MTOK MTOK-CAP IR-BUILD:SYMBOL-COPY ;

\ ---- the operand steps, one per mode -----------------------------------------
\ The family of a `MATCH`, in signature scope. Its width is written against the
\ `MATCH` row because that is where the build needs it, and so is the ordinal a
\ mismatch traps with: registering a family with src/compiler/native/trap.f is
\ idempotent, so asking here costs one row per family per process and the build
\ reads a number.
: MSCAN-MATCH-FAM ( n -- ) {: ix:n :}
   ix MTOK$ NFAM:MATCH-FAM {: fam:n ok:bool :}
   ok 0= if E-NELAB-MATCH throw then
   ix MR-FAMILY MROLE!
   CB-ROW @  fam NFAM:WIDTH  fam NFAM:NAME$ NTRAP:FAMILY  MMATCH!
   MK-MATCH fam MS-PUSH
   MM-VARIANT MM ! ;

\ A variant of the open `MATCH`. Its numbers go onto its own row and the row of
\ the `of` that follows it copies them, because the build reaches the arm at the
\ `of`: the variant token itself stages nothing at all.
: MSCAN-VARIANT ( n -- ) {: ix:n :}
   MS-TOP {: t:n :}
   ix MTOK$ t MS-FAM@ NFAM:VARIANT {: vid:n ok:bool :}
   ok 0= if E-NELAB-MATCH throw then
   ix MR-VARIANT MROLE!
   ix  vid NFAM:TAG  t MS-FAM@ vid NFAM:PADS  vid NFAM:PAY-CELLS  MARM!
   ix  vid NFAM:PAY-TERMS  vid NFAM:PAY-CELLS  <>  MONE!
   ix MV-ROW !
   MM-OF MM ! ;

\ The `of` that opens the arm the variant token named. The variant's row is the
\ one the step above recorded rather than the row before this one, so a row this
\ pass passed over cannot shift which token the numbers came from.
: MSCAN-OF ( n -- ) {: ix:n :}
   MV-ROW @ {: vix:n :}
   vix 0 < if E-NELAB-MATCH throw then
   ix  vix MTAG@ vix MPAD@ vix MPAY@  MARM!
   ix  vix MONE@  MONE!
   MS-TOP ix MS-ARM!
   -1 MV-ROW !
   MM-OFF MM ! ;

\ The family of a `construct`, in OWNER scope: minting a value of a family
\ belongs to the package that declared it, and that is the registry's rule rather
\ than this pass's.
: MSCAN-CON-FAM ( n -- ) {: ix:n :}
   ix MTOK$ NFAM:CON-FAM {: fam:n ok:bool :}
   ok 0= if E-NELAB-MATCH throw then
   ix MR-FAMILY MROLE!
   fam CB-FAM !
   MM-CON-VAR MM ! ;

\ And its variant, whose numbers go onto the `construct` row: the pads it pushes
\ and the tag it pushes after them are what turn a payload already on the stack
\ into a value of the family.
: MSCAN-CON-VAR ( n -- ) {: ix:n :}
   ix MTOK$ CB-FAM @ NFAM:VARIANT {: vid:n ok:bool :}
   ok 0= if E-NELAB-MATCH throw then
   ix MR-VARIANT MROLE!
   CB-ROW @  vid NFAM:TAG  CB-FAM @ vid NFAM:PADS  vid NFAM:PAY-CELLS  MARM!
   MM-OFF MM ! ;

\ ---- the keywords the pass reacts to -----------------------------------------
\ `endof` closes the arm of whichever form is open, which is the same rule the
\ engine keeps with its branch-kind bit and the checker with its frame kind. A
\ `MATCH` goes back to wanting a variant; a `case` goes back to ordinary tokens,
\ because its next arm's key is an expression.
: MSCAN-ENDOF ( -- )
   MS-TOP {: t:n :}
   t MS-ARM? 0= if E-NELAB-MATCH throw then
   t MS-ARM-END!
   t MS-MATCH? if MM-VARIANT MM ! then ;

\ `;MATCH` ends the form, and it is the moment the LAST arm becomes known: no
\ arm's `of` can say it is the last one while another variant may still follow.
\ The build needs the answer at the `of` itself - a last arm's mismatch edge goes
\ to the trap block and every other one goes to the next arm's test - so it is
\ written back onto that row here.
: MSCAN-SEMI ( -- )
   MS-TOP {: t:n :}
   t MS-MATCH? 0= if E-NELAB-MATCH throw then
   t MS-ARM? if E-NELAB-MATCH throw then
   t MS-OF@ {: ofix:n :}
   ofix 0 < if E-NELAB-MATCH throw then
   ofix MEND!
   MS-POP
   MM-OFF MM ! ;

: MSCAN-ENDCASE ( -- )
   MS-TOP {: t:n :}
   t MS-MATCH? if E-NELAB-MATCH throw then
   t MS-ARM? if E-NELAB-MATCH throw then
   MS-POP ;

\ An `of` met with no operand pending belongs to a `case`: its key is the
\ expression just read and there is no variant token in front of it.
: MSCAN-CASE-OF ( n -- ) {: ix:n :}
   MS-TOP {: t:n :}
   t MS-MATCH? if E-NELAB-MATCH throw then
   t MS-ARM? if E-NELAB-MATCH throw then
   t ix MS-ARM! ;

: MSCAN-OPEN ( IR-ARENA:arena n -- bool ) {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:OPEN-MATCH ROW-CTRL? if
      ix CB-ROW !  MM-FAM MM !  true exit
   then
   r ix HIR-CTRL:MAKE-BUNDLE ROW-CTRL? if
      ix CB-ROW !  MM-CON-FAM MM !  true exit
   then
   r ix HIR-CTRL:OPEN-CASE ROW-CTRL? if
      MK-CASE 0 MS-PUSH  true exit
   then
   false ;

: MSCAN-CLOSE ( IR-ARENA:arena n -- bool ) {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:CLOSE-ARM ROW-CTRL? if MSCAN-ENDOF true exit then
   r ix HIR-CTRL:CLOSE-MATCH ROW-CTRL? if MSCAN-SEMI true exit then
   r ix HIR-CTRL:CLOSE-CASE ROW-CTRL? if MSCAN-ENDCASE true exit then
   r ix HIR-CTRL:MATCH-ARM ROW-CTRL? if ix MSCAN-CASE-OF true exit then
   false ;

\ One row, dispatched on the mode. The operand modes accept nothing else, which
\ is what makes a form whose operand is missing a refusal here rather than a
\ family resolved from the wrong token.
\
\ A BOUND LOCAL'S NAME IS NOT A KEYWORD HERE, and the test stands where it does
\ for a reason. It is BELOW the operand modes because those read by POSITION -
\ the token after `MATCH` is a type family and the token after a variant is
\ `of`, and the engine's own token machine consumes them the same way, so a
\ mention passed over there would move which token the family came from. It is
\ ABOVE the two keyword readers because that is where this pass reads a token as
\ a WORD, and a body that named a local `of` or `endcase` means its own value
\ there (DECLARE-LOCAL). The locals pass runs before this one, which is what
\ makes the question answerable at all.
: MSCAN-STEP ( IR-ARENA:arena n -- ) {: r:IR-ARENA:arena ix:n :}
   ix IN-DECL? if exit then
   MM @ MM-FAM = if ix MSCAN-MATCH-FAM exit then
   MM @ MM-VARIANT = if
      r ix HIR-CTRL:CLOSE-MATCH ROW-CTRL? if MSCAN-SEMI exit then
      ix MSCAN-VARIANT exit
   then
   MM @ MM-OF = if
      r ix HIR-CTRL:MATCH-ARM ROW-CTRL? 0= if E-NELAB-MATCH throw then
      ix MSCAN-OF exit
   then
   MM @ MM-CON-FAM = if ix MSCAN-CON-FAM exit then
   MM @ MM-CON-VAR = if ix MSCAN-CON-VAR exit then
   ix LOCAL-OF 0 >= if exit then
   r ix MSCAN-OPEN if exit then
   r ix MSCAN-CLOSE drop ;

\ Walk the body once, before anything reads the word model for a body word. A
\ form left open at the end is refused here rather than compiled into a dispatch
\ with no end.
: MATCH-SCAN ( IR-ARENA:arena n -- ) {: r:IR-ARENA:arena n:n :}
   MATCH-RESET
   n 1 ?do
      r i MSCAN-STEP
   loop
   MSN @ 0<> if E-NELAB-MATCH throw then
   MM @ MM-OFF <> if E-NELAB-MATCH throw then ;

\ ---- the deferred word `is` names --------------------------------------------
\ ONE ROW AFTER THE KEYWORD, AND IT IS DECIDED BY POSITION. `is FOO` binds the
\ quotation on the stack to FOO, and FOO is not a word this body CALLS - so the
\ row is marked here, before the model is read, exactly as a `MATCH` family
\ token is and for the same reason: reading it as a body word would resolve it
\ to the deferred word and compile a call to it.
\
\ THE TOKEN IS ON THE TAPE BECAUSE THE READER CONSUMED IT. src/core/checker.f
\ IS-TOK reads it out of the middle of its judgement and reports it straight
\ after the keyword's own row, so the row after `is` IS the deferred word's
\ name and nothing else can be there. A name in a comment or inside a string
\ cannot reach this position: a parenthesised comment is not a token at all and
\ a string literal is ONE token of the string kind, so the refusal below - the
\ row after `is` has to exist and has to be a NAME - is a structural test and
\ not a spelling one.
\
\ IT RUNS AFTER THE TAG-DISPATCH PASS because that pass clears every role, and
\ before the pass that resolves names, because that is the first reader of a
\ role. It also runs after the locals pass, which is what lets it pass over a
\ declaration row and a bound name the way every other reader of a body token
\ does. All three orders are stated in COLON.
\
\ PASSING OVER A BOUND NAME IS NOT DECORATION. The role this marks makes the
\ NEXT row an operand, and an operand is a row the walk steps over - so a body
\ that named a local `is` would, without these two tests, have lost the two
\ tokens after each mention and compiled a shorter program with no refusal
\ anywhere.
: DSCAN-STEP ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena n:n ix:n :}
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   r ix HIR-CTRL:BIND-DEFER ROW-CTRL? 0= if exit then
   ix 1+ {: t:n :}
   t n >= if E-NELAB-DEFER throw then
   VW t NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-DEFER throw then
   t MR-DEFER MROLE! ;

: DEFER-SCAN ( IR-ARENA:arena n -- ) {: r:IR-ARENA:arena n:n :}
   n 1 ?do
      r n i DSCAN-STEP
   loop ;

\ ---- the names the dialect does not model, before anything reads the model ----
\ THE DIALECT IS NOT THE WHOLE VOCABULARY AND NEVER WAS. It models the operations
\ this chain compiles into instructions; everything else a body names is some
\ OTHER word, and what a call site needs to know about another word - where its
\ code starts and how many cells it moves - is a fact of the running engine and
\ of the checker, not of the dialect. So before any pass reads the word model,
\ every name in the body that the model does not carry is put to the engine, and
\ the ones it and the checker can both answer for become callable rows.
\
\ AND NOT EVERY NAME OUTSIDE THE DIALECT IS A CALL. A `constant` and a `create`d
\ word are records whose whole body is one push of a value their definer decided,
\ and a body that writes such a name means that value - not a branch to the four
\ instructions that push it. The engine's record says which definer made it
\ (src/habu/layout.f DKIND), so that question is asked first here and the name
\ becomes a literal row rather than a callable one. It is worth more than the
\ instructions it saves: a call bars every register a callee with no clobber
\ record could destroy, so a body that named two constants could not be allocated
\ at all where the same body with the digits spelled out compiled (149 of the
\ chain census's refusals, dot habu-fold-a-named-052f4c4b).
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
   ix MOPERAND? if exit then
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   VW ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if exit then
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? if exit then
   CTX BLD r sy HIR-WORD:RESOLVE-FIXED if exit then
   CTX BLD r sy HIR-WORD:RESOLVE-CALLABLE drop ;

: RESOLVE-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   n 1 ?do
      r i RESOLVE-STEP
   loop ;

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
\ back into identities this module's word model can be asked about. It is the
\ word model's own KEY that is asked for, exactly as WSYM asks it of a token of
\ this definition's own tape, so a recorded body reaches the rows its own
\ compilation reached whichever case its source wrote them in.
: INL-SYM ( n n -- IR-ID:ir-symbol-id )
   {: entry:n k:n :}
   CTX BLD  entry k NINL:SPELL$  HIR-WORD:KEY-SPELL ;

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
\ none; either half of a locals group would bind names in the caller's own scope;
\ and a return-stack transfer would move a value between the CALLER's two vectors
\ while the copied body believes it is moving between its own. A copied body is
\ balanced in itself - the checker proved that of the callee - so splicing one
\ could be made to work by splicing the two vectors together as well, but nothing
\ measured asks for it: no definition in the tree both parks a value and is short
\ enough to copy. So the site calls the callee, which is what it did before this
\ meaning existed.
\
\ AND THE THREE LITERAL MEANINGS ANSWER `call`, WHICH IS HONEST RATHER THAN
\ ABSENT. All three belong to a TOKEN and never to a word:
\ src/compiler/native/hir-word.f's N>MEAN refuses their stored codes outright, so
\ MEANING@ - the only way a meaning reaches this table - cannot answer any of
\ them, and a token that really is a literal is answered by its KIND long before
\ this is asked. A row claiming one would be a corrupt row, and what a corrupt
\ row earns is not a copy.
: SPLICE-STAGING ( HIR:meaning -- staging )
   MATCH HIR:meaning
      literal      OF NELAB-STAGING:CALL ENDOF
      real-literal OF NELAB-STAGING:CALL ENDOF
      string-literal OF NELAB-STAGING:CALL ENDOF
      op           OF NELAB-STAGING:OP ENDOF
      const-op     OF NELAB-STAGING:CONST-OP ENDOF
      fixed        OF NELAB-STAGING:FIXED ENDOF
      rename       OF NELAB-STAGING:RENAME ENDOF
      rstack       OF NELAB-STAGING:CALL ENDOF
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

\ ---- which control actions stage a call, and which call ----------------------
\ FOUR OF THEM DO, AND THE LIST IS WRITTEN ONCE. `RECURSE` calls the definition
\ being compiled, `is` calls the engine's store-and-declare primitive,
\ `execute` calls the engine's own `execute`, and `catch` calls the engine's own
\ `catch`. Two questions turn on that list - whether the body calls at all, and
\ whether it needs the memory order - and a reader that kept two lists of the
\ same four members would eventually not.
\
\ AND THE ANSWER CARRIES THE OPERATION, because the order question is asked of
\ the SCHEMA and not assumed: `RECURSE` goes to a block of this function and the
\ other two go to an address, which is exactly the difference between hir.call
\ and hir.wordcall. The false answer carries an operation too rather than a
\ sentinel, so no caller can read one out of a "no".
: CTRL-CALL? ( HIR:ctrl -- HIR:opcode bool )
   {: k:HIR:ctrl :}
   k HIR-CTRL:SELF-CALL HIR-CTRL:EQ if HIR-OPCODE:CALL true exit then
   k HIR-CTRL:BIND-DEFER HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   k HIR-CTRL:EXEC HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   k HIR-CTRL:CATCH HIR-CTRL:EQ if HIR-OPCODE:WORDCALL true exit then
   HIR-OPCODE:CALL false ;

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
\ AND A CALLEE CONTROL DOES NOT COME BACK FROM IS NEVER COPIED. Its routine ends
\ in the terminator that leaves without returning, and a copy splices a body's
\ OPERATIONS into the middle of a block that goes on afterwards - so a copy of it
\ would put a terminator where the caller still has work to do, which is not a
\ block at all. The call is what such a callee needs and the call is what it
\ gets, which is also what makes the dead path a fact about ONE site: the block
\ the call ends is the caller's own.
: CALLEE-COPY? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:CALLEE-DEAD? if false exit then
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
      i MOPERAND? 0=  i IN-DECL? 0=  and  i LOCAL-OF 0 <  and if r i INL-STEP then
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
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:CALLABLE HIR-MEANING:EQ if
      ix INL-AT? if r  r sy HIR-WORD:ENTRY@  REC-BODY-ORDER? exit then
      CTX BLD  CTX BLD  HIR-OPCODE:WORDCALL HIR:OPCODE  TOKEN-OPERANDS
      0<> exit
   then
   m HIR-MEANING:CONTROL HIR-MEANING:EQ if
      r sy HIR-WORD:CTRL@ CTRL-CALL? {: op:HIR:opcode calls:bool :}
      calls 0= if false exit then
      CTX BLD  CTX BLD  op HIR:OPCODE  TOKEN-OPERANDS 0<> exit
   then
   r sy SYM-ORDER? ;

: MEM-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 TOK-NEED !
   n 1 ?do
      i MOPERAND? 0=  i IN-DECL? 0=  and  i LOCAL-OF 0 <  and if
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
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:CALLABLE HIR-MEANING:EQ if ix INL-AT? 0= exit then
   m HIR-MEANING:CONTROL HIR-MEANING:EQ if
      r sy HIR-WORD:CTRL@ CTRL-CALL? {: op:HIR:opcode calls:bool :}
      calls exit
   then
   false ;

\ ---- and does the call leave the caller anything? ----------------------------
\ A CALL DESTROYS REGISTERS, AND WHICH ONES IS A FACT THE CALLEE PUBLISHED OR DID
\ NOT. A routine this chain compiled records what its accepted allocation writes
\ (src/compiler/native/clobber.f), and everything downstream reads that record:
\ the allocator keeps a crossing value out of those registers
\ (src/compiler/native/regalloc.f MB-FORBID) and the validator re-derives the
\ same bar from the same record (src/compiler/native/regalloc-verify.f CLOB-AT).
\ A routine with NO row is taken to destroy the whole pool by both of them, and
\ then no register at all survives the branch.
\
\ WHICH IS WHY THIS QUESTION IS ASKED HERE, of all places. What the section below
\ decides is whether a local has to TRAVEL - be handed over at the call and taken
\ back from it, which puts it in a data-stack slot. That is worth doing exactly
\ when there is no register for it to stay in, and this is the fact that says so.
\ Asking it costs the elaborator a read of the callee's row, which is the same
\ class of fact as the callee's ADDRESS and its declared effect, both of which
\ this file already reads off the word model to build the call at all.
\
\ ONLY A NAMED CALLEE CAN ANSWER IT, AND EVERYTHING ELSE ANSWERS NO. A call this
\ file makes to an address it cannot name here - `execute`, `is`, RECURSE, every
\ control form CTRL-CALL? admits - has no row to consult, so it keeps nothing and
\ says so. That is the fail-closed direction: a call wrongly believed to keep a
\ register would leave a local in one the callee overwrites, and the two readers
\ named above would then be barring registers for a value that never told them it
\ was there.
\
\ IT IS ASKED ONLY OF A ROW WORD-CALL? HAS ALREADY ADMITTED, which is what
\ entitles it to read the meaning without asking whether the row models a word.
: CALL-KEEPS? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MEANING@ HIR-MEANING:CALLABLE HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:ENTRY@ NCLOB:KNOWN? ;

\ ---- which locals a call can reach -------------------------------------------
\ WHICH IS ONE OF THE TWO HALVES OF WHETHER A LOCAL TRAVELS, and this is the half
\ about the NAME: can a call get in front of a read of it. LCROSS? above holds
\ the other half and puts the two together, so what is recorded here is
\ reachability and nothing about the price. The answer is a walk of the tape, and
\ it has two parts because control has two directions.
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

\ Every word that opens a loop this scan has to count, which is both openers of
\ the counted loop as well as `begin`. This is the ONE reader of the loop
\ openers that is not a MATCH over the whole control vocabulary, so it is the one
\ that a new opener can be left out of - and leaving `?do` or `do` out shows up
\ at once as a closer with nothing open, which is LS-POP's own refusal.
: OPENS-LOOP? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:OPEN-BEGIN ROW-CTRL?
   r ix HIR-CTRL:OPEN-DO ROW-CTRL? or
   r ix HIR-CTRL:OPEN-DO-SKIP ROW-CTRL? or ;

: CLOSES-LOOP? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-CTRL:CLOSE-UNTIL ROW-CTRL?
   r ix HIR-CTRL:CLOSE-REPEAT ROW-CTRL? or
   r ix HIR-CTRL:CLOSE-AGAIN ROW-CTRL? or
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
   ix MOPERAND? if exit then
   ix IN-DECL? if exit then
   ix LOCAL-OF {: k:n :}
   k 0 >= if
      CALL-NEED @ 0<> if k LCROSS+ exit then
      k LS-PEND+ exit
   then
   r ix WORD-CALL? if
      1 CALL-NEED !
      r ix CALL-KEEPS? 0= if 1 CALL-BARE ! then
      LS-CALL+ exit
   then
   r ix OPENS-LOOP? if LS-PUSH exit then
   r ix CLOSES-LOOP? if LS-POP then ;

: CROSS-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 CALL-NEED !
   0 CALL-BARE !
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
\ `repeat` one, `do` one, `?do` three and `loop` three; everything else makes
\ none. Getting one of them wrong here would put a branch somewhere else, so
\ every closer
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
\ AN ARM THAT ALREADY ENDED HAS NOTHING TO CLOSE HERE, and what happens then
\ depends on how it ended. A DEAD arm closed its own block at the call that does
\ not come back, so `else` closes nothing and only opens the second arm; the
\ frame records that the first arm reached no join, which is what `then` needs to
\ know when the second arm ends too. An `exit` is still refused: it would need
\ the return block to exist before the structure it is in is closed, which is a
\ different capability and dot habu-let-exit-stand-d74f14ec carries it.
: SK-ELSE ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-ELSE? if E-NELAB-CTRL throw then
   PATH-END @ PATH-EXIT = if E-NELAB-CTRL throw then
   PATH-DEAD? if -1 t CS-END! else NB @ 1+ NB ! then
   PATH-LIVE PATH-END !
   t CS-JOIN@ NB @ JOIN!
   ix t CS-ARM! ;

\ Whether every path into this structure's join has ended, so that there is no
\ join at all: the walk is inside two arms and both of them are gone. It takes
\ THIS arm's state as an argument because both walks ask it at the same moment -
\ after the arm has been accounted for and before the frame is popped - and one
\ answer for two readers is what keeps the block counts equal.
\
\ WITH NO `else` THERE IS ALWAYS A JOIN. The `if`'s own false path is an edge
\ into it whatever the single arm did, so a structure of one arm never ends here
\ however that arm ended.
: SK-BOTH-ENDED? ( n bool -- bool )
   {: t:n armend:bool :}
   t CS-ELSE? 0= if false exit then
   armend 0= if false exit then
   t CS-END@ 0<> ;

\ `then`: the arm the walk is in ends at the join, and the join opens. Which
\ token the answer is written against is the one whose forward branch is still
\ unanswered - the `else`'s when there is one, and the `if`'s when there is not.
\
\ AND WHEN BOTH ARMS ENDED THERE IS NOTHING TO WRITE. No edge reaches the block
\ after this structure, so no such block is opened and none is counted; the
\ structure has ended, exactly as its arms did, and the word around it goes on
\ being closed by whatever closes IT. The row for this token stays unanswered,
\ which is right: nothing branches forward to a block that is not there, and
\ DO-ELSE below reads the row without demanding an answer for that reason.
: SK-CLOSE-IF ( -- )
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-PENDING {: key:n :}
   PATH-ENDED? {: armend:bool :}
   armend 0= if NB @ 1+ NB ! then
   t armend SK-BOTH-ENDED? if
      CS-POP
      PATH-DEAD PATH-END !
      exit
   then
   PATH-LIVE PATH-END !
   key NB @ JOIN!
   CS-POP ;

\ `while`: the test block ends here, its false edge leaves the loop through a
\ stub, and the body opens - the same two blocks `if` makes, for the same reason.
\ The loop it leaves is recorded, because that is what tells `until` it is the
\ wrong closer for this loop and `repeat` that it is the right one.
: SK-WHILE ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   PATH-ENDED? if E-NELAB-CTRL throw then
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

\ `again`: the body ends with the branch back to the header and NOTHING opens
\ after it. That is one block where `until` counts two and `repeat` counts one,
\ and the missing one is the whole of what this word is: the block after the loop
\ is reached by no edge, so it is not opened and not counted, and the path the
\ walk was on has ended exactly as a call that does not come back ends one.
\
\ A LOOP A `while` HAS LEFT CANNOT BE CLOSED THIS WAY, which is the same refusal
\ SK-UNTIL makes above and for the same reason: the block a `while` branches out
\ to is opened by `repeat`, this word opens no such block, and the values that
\ `while` handed over would arrive nowhere. The checker refuses the shape first -
\ CF-AGAIN in src/core/checker.f wants a frame no `while` has touched - so no
\ checked body reaches this line; it is written because the two derivations of
\ what this loop's blocks are have to agree here as everywhere else.
: SK-AGAIN ( -- )
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0<> if E-NELAB-CTRL throw then
   NB @ 1+ NB !
   CS-POP
   PATH-DEAD PATH-END ! ;

\ `leave`: the block the walk is in ends with a branch out of the innermost
\ counted loop, and no block opens after it. One block, exactly as `again` counts
\ one - and for the same reason, since both are a path ending in a branch to a
\ block somebody else opens. WHICH block it goes to needs no answer from this
\ walk: the loop's own frame carries the ordinal its openers read out of the
\ table, and `leave` reads that frame rather than a row of its own.
\
\ THE LOOP IS SEARCHED FOR RATHER THAN ASSUMED, which is what "innermost counted
\ loop" means: `leave` inside an `if` inside a `begin` inside a `?do` leaves the
\ `?do`. Only that the loop EXISTS can be asked here - DO-OPEN-N is the count and
\ it is the same table DO-FRAME searches below - because a `leave` with no
\ counted loop open is a body the walk after this one could not build either.
: SK-LEAVE ( -- )
   DO-OPEN-N 0= if E-NELAB-CTRL throw then
   NB @ 1+ NB !
   PATH-DEAD PATH-END ! ;

\ ---- what a tag dispatch counts ----------------------------------------------
\ `MATCH` and `case` build NOTHING when they open: the block the form stands in
\ becomes its first arm's test block, so a dispatch costs no block until an arm
\ arrives. Every arm then costs three - the test that compares the tag or the
\ key, the block its mismatch edge leaves through, and the arm's own body - and
\ the block after the form is the one the arms' ends branch to.
: SK-ARM ( n -- ) {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   NB @ 2 + NB !
   ix t CS-OFIX! ;

\ `endof`: the arm's block ends, and the block that opens is the one this arm's
\ mismatch edge was branched to - the next arm's test, or the default path of a
\ `case`. A `MATCH`'s LAST arm branched its mismatch to a trap instead, so
\ nothing opens after it and the next ordinal is the join's.
: SK-CLOSE-ARM ( -- )
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ {: ofix:n :}
   ofix 0 < if E-NELAB-CTRL throw then
   PATH-ENDED? 0= if
      NB @ 1+ NB !
      t CS-JOINED+
   then
   PATH-LIVE PATH-END !
   ofix MEND@ 0= if ofix NB @ JOIN! then
   -1 t CS-OFIX! ;

\ The block after a form, counted once for both of them. A dispatch every arm of
\ which ended - each one throwing, say - is reached by no edge at all, so no such
\ block is opened and none is counted: the form has ended exactly as its arms
\ did, which is the rule an `if` with two ended arms already keeps.
: SK-ADT-JOIN ( n -- ) {: t:n :}
   t CS-JOINED? if
      t CS-JOIN@ NB @ JOIN!
      PATH-LIVE PATH-END !
   else
      PATH-DEAD PATH-END !
   then
   CS-POP ;

: SK-CLOSE-MATCH ( -- )
   HIR-CTRL:OPEN-MATCH CS-OPENER-CK {: t:n :}
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   t SK-ADT-JOIN ;

\ `endcase` closes the DEFAULT path, which is the one path of a `case` that is
\ not an arm, and then the join opens.
: SK-CLOSE-CASE ( -- )
   HIR-CTRL:OPEN-CASE CS-OPENER-CK {: t:n :}
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   PATH-ENDED? 0= if
      NB @ 1+ NB !
      t CS-JOINED+
   then
   t SK-ADT-JOIN ;

\ A call this walk has to count a block for: one the definition really MAKES
\ (a copied body is not a call at all) to a word the checker says control does
\ not come back from. It closes the block it is in, exactly as an `exit` does,
\ and the walk below builds the same block for it - so both walks ask this one
\ question and neither has a rule of its own about which words are dead.
: SK-DEAD-CALL? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CALLABLE MODELED-AS? 0= if false exit then
   ix INL-AT? if false exit then
   r  ix WSYM  HIR-WORD:CALLEE-DEAD? ;

\ After a path has ended, the only tokens that may follow are the ones that
\ close the structure the ended path was an arm of. It is asked HERE as well as
\ in the walk because this walk COUNTS blocks: a token it went on to count after
\ a path ended would make the two walks disagree about a number they must agree
\ about, and the disagreement would surface as a branch into the wrong block
\ rather than as the refusal it is.
: SK-AFTER-END-CK ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CONTROL MODELED-AS? 0= if E-NELAB-CTRL throw then
   r  ix WSYM  HIR-WORD:CTRL@ {: k:HIR:ctrl :}
   k HIR-CTRL:CLOSE-IF HIR-CTRL:EQ if exit then
   k HIR-CTRL:MID-ELSE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-ARM HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-CASE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   E-NELAB-CTRL throw ;

: SK-STEP ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   ix QSKIP? if exit then
   ix MOPERAND? if exit then
   ix IN-DECL? if exit then
   ix LOCAL-OF 0 >= if exit then
   PATH-ENDED? if r ix SK-AFTER-END-CK then
   r ix SK-DEAD-CALL? if
      NB @ 1+ NB !
      PATH-DEAD PATH-END !
      exit
   then
   r ix ADMIT-AT
   HIR-MEANING:CONTROL HIR-MEANING:EQ 0= if exit then
   r  ix WSYM  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if      OF HIR-CTRL:OPEN-IF ix SK-PUSH  NB @ 2 + NB ! ENDOF
      mid-else     OF ix SK-ELSE ENDOF
      close-if     OF SK-CLOSE-IF ENDOF
      open-begin   OF HIR-CTRL:OPEN-BEGIN ix SK-PUSH  NB @ 1+ NB ! ENDOF
      mid-while    OF SK-WHILE ENDOF
      close-until  OF SK-UNTIL ENDOF
      close-repeat OF SK-REPEAT ENDOF
      close-again  OF SK-AGAIN ENDOF
      \ Both counted-loop openers push the same frame kind, because a frame
      \ records the structure and `loop` closes one structure. They differ in
      \ what they BUILD: a `do` ends the block it stands in and opens the header,
      \ and a `?do` puts a guard, a skip stub and a pre-header in front of that.
      open-do      OF HIR-CTRL:OPEN-DO ix SK-PUSH  NB @ 1+ NB ! ENDOF
      open-do-skip OF HIR-CTRL:OPEN-DO ix SK-PUSH  NB @ 3 + NB ! ENDOF
      close-loop   OF HIR-CTRL:OPEN-DO CS-OPENER-CK CS-JOIN@
                      NB @ 3 + NB !  NB @ JOIN!  CS-POP ENDOF
      index        OF ENDOF
      drop-loop    OF ENDOF
      early-leave  OF SK-LEAVE ENDOF
      early-exit   OF NB @ 1+ NB !  1 EXIT-USED !  PATH-EXIT PATH-END ! ENDOF
      self-call    OF ENDOF
      open-match   OF HIR-CTRL:OPEN-MATCH ix SK-PUSH ENDOF
      match-arm    OF ix SK-ARM ENDOF
      close-arm    OF SK-CLOSE-ARM ENDOF
      close-match  OF SK-CLOSE-MATCH ENDOF
      open-case    OF HIR-CTRL:OPEN-CASE ix SK-PUSH ENDOF
      close-case   OF SK-CLOSE-CASE ENDOF
      make-bundle  OF ENDOF
      \ A quotation leaves one value and builds no block of THIS function: what
      \ stands between the pair belongs to another function and QSKIP? above
      \ steps over it. The closer belongs to that function too, and a body's own
      \ range stops before it, so this arm is reachable from no walk at all and
      \ refuses rather than agreeing silently with a pre-scan that lost a span.
      open-quot    OF ENDOF
      close-quot   OF ix QUOT-REFUSE ENDOF
      \ `is`, `execute` and `catch` each stage one call and no block, so they
      \ count the same as an ordinary call does here: nothing. `catch` builds no
      \ block for its exceptional path either - the engine's handler resumes
      \ inside the routine this site branches to and leaves through the one
      \ return, so there is no second edge here to give a block to.
      bind-defer   OF ENDOF
      exec         OF ENDOF
      catch        OF ENDOF
   ;MATCH ;

\ Walk the body once, counting. A structure left open at the end of the body is
\ refused here rather than at the return, because the walk that follows would
\ otherwise build blocks against a join nobody ever named.
\ THE RANGE IS PASSED IN BECAUSE A TAPE HOLDS MORE THAN ONE BODY. A definition's
\ own body runs from the token after its name to the end of the tape; a
\ quotation's body runs between the two tokens that open and close it, on the
\ same tape and in the middle of the enclosing one. Both are counted by these
\ words and neither may see the other's tokens, so where a body starts and stops
\ is the caller's to say rather than something derived from the tape's length.
: SKELETON ( IR-ARENA:arena n n -- )
   {: r:IR-ARENA:arena lo:n hi:n :}
   hi TMAX > if E-NELAB-BLOCK throw then
   0 NB !
   JOIN-RESET
   CS-RESET
   EXIT-RESET
   hi lo ?do
      r i SK-STEP
   loop
   CS-N @ 0<> if E-NELAB-CTRL throw then
   PATH-END @ PATH-EXIT = if E-NELAB-CTRL throw then
   PATH-DEAD? {: dead:bool :}
   \ WHERE THE RETURN BLOCK LANDS DEPENDS ON WHETHER THE BODY REACHES IT. The
   \ fall-through closes one more block on its way there; a body whose last path
   \ ended has no fall-through, so its last block is already closed and the
   \ return block is the next ordinal rather than the one after that.
   EXIT-USED @ 0<> if
      dead if NB @ else NB @ 1+ then EXIT-ORD !
   then
   EXIT-USED @ 0<> if EXIT-ORD @ 1+ else NB @ then
   NFROZEN:BMAX > if E-NELAB-BLOCK throw then
   0 NB !
   PATH-LIVE PATH-END !
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
\ AND WHAT AN ENDED FIRST ARM CHANGES. It hands the join nothing, so it states
\ no width and closes no block here - its own last operation closed it. The
\ frame records that, because `then` has two questions to answer afterwards and
\ both turn on it: what width the join takes (the second arm's, since it is now
\ the only edge) and whether there is a join at all (there is not, if the second
\ arm ends too). The forward ordinal is read WITHOUT being demanded for the same
\ reason: when both arms end there is no such block and the skeleton wrote no
\ answer, and neither this word nor `then` reaches for it on that path.
: DO-ELSE ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   t CS-ELSE? if E-NELAB-CTRL throw then
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: e:n :}
   ix JOIN-OF {: j:n :}
   PATH-ENDED? if
      -1 t CS-END!
      0 t CS-ARM!
      0 t CS-ARMR!
      PATH-LIVE PATH-END !
   else
      j JOIN-CK drop
      VN @ t CS-ARM!
      RN @ t CS-ARMR!
      ix j TERM-BR
   then
   NB @ e <> if E-NELAB-CTRL throw then
   j t CS-JOIN!
   ix  d rd +  OPEN-ARGS ;

\ `then`: the arm the walk is in reaches the join too, and the join takes as many
\ arguments as every edge into it carries. An arm that left the stack a different
\ depth is refused here: the two paths would be handing the same block different
\ numbers of values.
\ WHAT THE JOIN TAKES WHEN ONE ARM DID NOT REACH IT. Its width is whatever the
\ edges that DO reach it carry, and with one arm gone there is exactly one such
\ edge: the surviving arm's, or - when the structure has no `else` - the `if`'s
\ own false stub, which carries the vector as the `if` found it. So a first arm
\ that ended leaves the second arm to state the width, and an only arm that
\ ended leaves the `if`'s depth standing, which is what it was already.
: DO-JOIN-WIDTH ( n -- n )
   {: t:n :}
   t CS-ELSE? 0= if t CS-DEPTH@ exit then
   t CS-END@ 0<> if VN @ exit then
   t CS-ARM@ ;

\ The same three answers about the parked values, and each for the reason its
\ data half is what it is. With no `else` the `if`'s own false stub is an edge
\ into the join and it carried what the `if` was holding. With an `else` whose
\ first arm ENDED the surviving arm is the only edge, so the walk's own live
\ count is the answer. And with two live arms the first arm's is, because the
\ join takes what that arm handed it and this arm is held to it below.
: DO-JOIN-RD ( n -- n )
   {: t:n :}
   t CS-ELSE? 0= if t CS-RD@ exit then
   t CS-END@ 0<> if RN @ exit then
   t CS-ARMR@ ;

: DO-CLOSE-IF ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-IF CS-OPENER-CK {: t:n :}
   PATH-ENDED? {: armend:bool :}
   t DO-JOIN-WIDTH {: w:n :}
   t DO-JOIN-RD {: rd:n :}
   t CS-JOIN@ {: j:n :}
   armend if
      PATH-LIVE PATH-END !
   else
      VN @ w <> if E-NELAB-JOIN throw then
      RN @ rd <> if E-NELAB-JOIN throw then
      ix j TERM-BR
   then
   t armend SK-BOTH-ENDED? if
      CS-POP
      PATH-DEAD PATH-END !
      exit
   then
   NB @ j <> if E-NELAB-CTRL throw then
   ix  w rd +  OPEN-ARGS
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
   ix  d CS-TOP CS-RD@ +  OPEN-ARGS ;

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
      RN @ t CS-XR@ <> if E-NELAB-JOIN throw then
   then
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   VN @ t CS-XD!
   RN @ t CS-XR!
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
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: h:n :}
   t CS-EXIT@ JOIN-CK {: j:n :}
   t CS-XD@ {: xd:n :}
   t CS-XR@ {: xr:n :}
   VN @ d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   ix h TERM-BR
   NB @ j <> if E-NELAB-CTRL throw then
   ix  xd xr +  OPEN-ARGS
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
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: h:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix h STUB
   ix OPEN-PLAIN
   CS-POP ;

\ `again` ( -- ): go round, always. The body ends by branching back to the header
\ carrying the loop's live values, which is the edge `repeat` builds too - and
\ then nothing else is built, because nothing reaches the block after this loop.
\ There is no test, so there is no two-way branch and no stub; there is no exit
\ edge, so there is no block for one to arrive at.
\
\ THE PATH HAS ENDED AND IT ENDS THE WAY A DEAD CALL ENDS ONE. What PATH-DEAD
\ records is that the block is closed and no edge into any block the walk opens
\ NEXT exists, and both of those are true here: the one edge this word makes goes
\ backwards, to a block that was opened before it. Everything downstream of that
\ record is what an `again` needs - `then`, `else`, `endof` and `endcase` may
\ follow, the body may simply stop, and a definition whose last path ended has no
\ fall-through and may have no return at all. A `begin … again` with no `exit` in
\ it is exactly that routine: every block of it names a successor, so it has no
\ block the results leave through, which is the NO-RET shape
\ src/compiler/native/regalloc.f already knows and the contract
\ src/compiler/native/migrate.f already picks for a word the checker certified as
\ never returning. The checker certifies this one for the same reason this walk
\ ends the path (src/core/checker.f DO-TOK1 makes `again` a dead owner), so the
\ two answers come from one fact.
\
\ THE BACK EDGE IS HELD TO THE HEADER'S WIDTH, as `repeat`'s is: the header takes
\ what the `begin` found on the vector, and a body that left some other number of
\ values would be handing that block a different list on the second turn than on
\ the first.
: DO-AGAIN ( n -- )
   {: ix:n :}
   HIR-CTRL:OPEN-BEGIN CS-OPENER-CK {: t:n :}
   t CS-NW@ 0<> if E-NELAB-CTRL throw then
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: h:n :}
   VN @ d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   ix h TERM-BR
   CS-POP
   PATH-DEAD PATH-END ! ;

\ THE COUNTED LOOP, WHICH TWO WORDS OPEN AND ONE CLOSES. `do` ( limit start -- )
\ and `?do` ( limit start -- ) build the same loop out of the same pieces; `?do`
\ puts one test in front of it. Everything after that test is DO-ENTER below,
\ which both of them call, so "`?do` is `do` with a zero-trip guard" is one word
\ shared rather than two constructions written twice and compared by eye.
\
\ WHAT THE ENGINE DOES, WHICH IS WHAT THESE TWO HAVE TO AGREE WITH. `loop` adds
\ one to the index and goes round while the sum is still BELOW the limit, signed.
\ So the body of a `do` always runs at least one turn - the test comes after it -
\ and `?do` is that same loop with `cmp start,limit` and a branch out when the
\ two are EQUAL in front (src/habu/habu2.f J-DO and J-?DO). Measured on this
\ engine: `0 0 do … loop` runs one turn and `0 0 ?do … loop` runs none, while
\ `0 5 do` and `0 5 ?do` both run one and `5 0 do` and `5 0 ?do` both run five.
\ Equality is the whole of the difference.
\
\ THE FRAME OPENS ONCE THE GUARD IS BEHIND US, WHICH IS THE WHOLE POINT OF WHERE
\ DO-ENTER STANDS. `?do`'s skip stub is the edge taken when the loop runs no
\ turns at all: it goes to the block AFTER the loop, where this loop's counters
\ are not live, so it is built while the frame is still closed. The branch into
\ the header goes INSIDE the loop, where they are, so the frame is open by then
\ and carries the starting index and limit; the header takes them back as
\ arguments and the frame names those from the first turn on. A `do` has no such
\ stub, so it reaches DO-ENTER from the block it stands in.
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

\ The two counters, read off the vector where the source left them: the start on
\ top and the limit under it. They are only READ here - what takes them off is
\ each opener's own business, because `?do` spends them on the subtraction it
\ tests and `do` has nothing to spend them on.
\
\ NEITHER MAY BE A DOUBLE, and this is the one place that says so for both. For
\ `?do` it would be said anyway, one step later: its subtraction is `hir.sub`,
\ whose operands are cells, so COERCE1 refuses a double there. A `do` stages no
\ operation over the pair at all, so without this the refusal would arrive at
\ `loop`'s own addition, a whole body further on and against a different token.
\ One rule, one seam, one error code, for two words that take the same pair.
: DO-PAIR ( -- IR-ID:ir-value-id IR-ID:ir-value-id )
   VN @ 2 < if E-NELAB-UNDER throw then
   VN @ 1- VAT {: st:IR-ID:ir-value-id :}
   VN @ 2 - VAT {: lm:IR-ID:ir-value-id :}
   st REAL-VALUE? if E-NELAB-TYPE throw then
   lm REAL-VALUE? if E-NELAB-TYPE throw then
   st lm ;

\ Open the frame and go into the header. Both openers end here, with the same
\ arguments meaning the same things: the block the header will be, the depth the
\ vector has once the counters are off it, and the block after the loop.
\
\ THE FRAME'S KIND IS `open-do` FOR BOTH, AND THAT IS THE STRUCTURE RATHER THAN
\ THE WORD. What a frame records is which structure is open, so that its closer
\ can check it has met the right one and so that `i`, `unloop` and every carrier
\ can find the counted loops they are inside. `do` and `?do` open ONE structure -
\ a counted loop closed by `loop` - so there is one kind here, exactly as
\ `begin`'s one kind serves both of its closers. The two words differ in the code
\ their openers emit, and that difference is spent by the time the frame exists.
: DO-ENTER ( IR-ID:ir-value-id IR-ID:ir-value-id n n n n -- )
   {: st:IR-ID:ir-value-id lm:IR-ID:ir-value-id ix:n h:n d:n j:n :}
   HIR-CTRL:OPEN-DO d j CS-PUSH
   h CS-TOP cells CS-HEAD + !
   st CS-TOP CS-IDX !
   lm CS-TOP CS-LIM !
   ix h  HEAD-CROSS-DO CROSS-L  TERM-BR-H
   ix  d CS-TOP CS-RD@ +  HEAD-CROSS-DO CROSS-L  OPEN-ARGS-H ;

\ `do` ( limit start -- ): the loop with no guard in front of it. The block the
\ walk is in ends by branching into the header, so this builds ONE block where
\ `?do` builds three, and the skeleton counts it as one.
\
\ THE COUNTERS LEAVE THE VECTOR WITHOUT AN OPERATION, which is what `do` IS:
\ Forth's loop parameters are not on the data stack, and nothing computes
\ anything on the way in. They are not lost - DO-ENTER puts both into the frame
\ and hands them to the header - so this drop is the same motion `?do`'s
\ subtraction and its test make, with the arithmetic that only the guard needed
\ left out.
: DO-OPEN-DO ( n -- )
   {: ix:n :}
   DO-PAIR {: st:IR-ID:ir-value-id lm:IR-ID:ir-value-id :}
   2 VDROP
   ix JOIN-OF JOIN-CK {: j:n :}
   st lm  ix  NB @ 1+  VN @  j  DO-ENTER ;

\ `?do` ( limit start -- ): the same loop, skipped when the limit and the start
\ are equal. The test is the subtraction of the two, which is zero exactly when
\ they are equal, wrap-around included; TERM-BRZ takes its FIRST successor on
\ zero, so the stub out of the loop is the zero edge and the loop is the other.
\ Turning the two round compiles a loop that runs exactly when it should not.
: DO-OPEN-DO-SKIP ( n -- )
   {: ix:n :}
   DO-PAIR {: st:IR-ID:ir-value-id lm:IR-ID:ir-value-id :}
   ix HIR-OPCODE:SUB EMIT-OPCODE
   VN @ 1- {: d:n :}
   NB @ {: c:n :}
   ix JOIN-OF JOIN-CK {: j:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix j STUB
   ix OPEN-PLAIN
   st lm  ix  c 3 +  d  j  DO-ENTER ;

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
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ {: j:n :}
   t CS-HEAD@ {: h:n :}
   t CS-IDX @ {: iv:IR-ID:ir-value-id :}
   t CS-LIM @ {: lv:IR-ID:ir-value-id :}
   VN @ d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
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
   ix  d rd +  OPEN-ARGS
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

\ `leave` ( -- ): leave the innermost counted loop from the middle of its body.
\ It is a branch to the block after that loop, carrying the live values - and
\ that block already exists as far as this walk is concerned: its ordinal is the
\ one the loop's opener read out of the skeleton's table and put in the frame, and
\ `loop`'s own exit stub and `?do`'s skip stub branch to the same one. So there
\ is no stub here and no new block: an unconditional branch carries values, and
\ what it carries is what a stub would have handed on.
\
\ WHICH LOOP IT LEAVES IS THE FRAME SEARCH `i` MAKES, and for the same reason: a
\ `begin` or an `if` between the `leave` and its `?do` changes nothing, because
\ Forth's `leave` names the innermost COUNTED loop. src/core/checker.f CF-LEAVE
\ searches the same way and stops at a quotation boundary; a quotation's body is
\ walked here with its own control stack, so a `leave` written inside one finds
\ no counted loop and is refused, which is that same rule arrived at from the
\ other side.
\
\ THE VECTOR HAS TO BE WHAT THE LOOP WAS ENTERED WITH, and this is not a rule of
\ this file's own: the block after the loop takes the values `loop`'s exit stub
\ hands it, which is the vector at the `do`, and the checker holds a `leave` to
\ exactly that row (CF-LEAVE unifies the stack at the `leave` with the DO-point
\ row). A body that left some other number of values would be handing one block
\ two different lists.
\
\ AND IT CROSSES WITH ONE LOOP FEWER, which is the seam EXIT-CROSS-DO already
\ names for `loop`'s exit stub: the block after the loop is OUTSIDE it, so this
\ loop's counters are dead on the way there and only the enclosing loops' travel.
\ The frame is still on the control stack when this runs, exactly as it is at
\ that stub, so the two ranges are written the same way and mean the same thing.
: DO-LEAVE ( n -- )
   {: ix:n :}
   DO-FRAME {: t:n :}
   t CS-DEPTH@ {: d:n :}
   t CS-RD@ {: rd:n :}
   t CS-JOIN@ JOIN-CK {: j:n :}
   VN @ d <> if E-NELAB-JOIN throw then
   RN @ rd <> if E-NELAB-JOIN throw then
   ix j  EXIT-CROSS-DO CROSS-L  TERM-BR-H
   PATH-DEAD PATH-END ! ;

\ ---- the three tag-dispatch forms --------------------------------------------
\ WHAT A `MATCH` IS ONCE IT IS BLOCKS. A value of a sum family is W flat cells
\ with its tag on top, so a dispatch over it is a chain of ordinary comparisons:
\ each arm's test block compares that tag with the constant its variant's
\ declaration order is and branches two ways, the matching edge falls into the
\ arm's body, and the mismatching edge goes on to the next arm's test. The last
\ arm's mismatching edge has nowhere left to go, and that is where the trap is.
\ Nothing here is new machinery - `hir.const`, `hir.eq`, `hir.brz`, `hir.br` and
\ `hir.trap` are the operations the chain already had, and the joins are the
\ block arguments an `if` already joins through.
\
\ WHY THE TEST BLOCKS NEED NO SNAPSHOT OF THE VECTOR. Each arm's mismatching edge
\ leaves through a stub, which is the same critical-edge split every two-way
\ branch of this file makes, and the stub hands the whole live vector to the next
\ test block - which therefore takes it as ORDINARY BLOCK ARGUMENTS. So the
\ scrutinee reaching the second arm's comparison is the value the first arm's
\ stub handed over rather than a value read out of a block that no longer
\ dominates it, and the walk keeps exactly the one vector it always kept.
\
\ AND WHY A `case` IS THE SAME MACHINERY. Its subject is one cell rather than a
\ bundle, the value an arm compares is the key the arm itself computed rather
\ than a constant off a declaration, and a matched arm consumes the selector
\ where a matched variant drops its tag and pads - but the blocks, the stubs, the
\ joins and the two-way branch are the same construction, and the frame decides
\ which of the two it is. The one place they differ in SHAPE is the end: a
\ `MATCH` is exhaustive and its last mismatch traps, while a `case` has a default
\ path, so its last arm's mismatch edge goes to the block `endcase` closes.

\ The subject the form is about to dispatch on, held against the two authorities
\ that know how wide it is. The registry declares how many cells a value of the
\ family occupies; the vector's own glue record says which of its entries are
\ cells of one value, written when the definition's arguments were stated or when
\ a call answered them. This is the one place the two can be compared, and they
\ have to agree.
\
\ A ONE-CELL VALUE IS NEVER GLUED, which is the rule src/compiler/native/dict.f
\ already keeps when it marks a declared row: a value that occupies one cell
\ cannot be taken apart by moving cells around, so nothing marks one and a mark
\ on it would be a bundle this pass does not have.
\
\ AND A DISAGREEMENT IS A REFUSAL RATHER THAN AN ADJUSTMENT. A parametric family
\ instantiated with a type argument that is itself several cells occupies MORE
\ cells than its declaration reserves - the checker records that difference as a
\ fact of its own and the engine's emitter reads it back, and the chain has no
\ way to ask for it - so what arrives here is a bundle wider than the width this
\ pass was told. Compiling the arms against the declared width would drop the
\ wrong cells and leave a program that runs. Two values of two families lying
\ next to each other are refused by the same test, for the honest reason that a
\ run of set bits does not say where one of them ends and the next begins.
: BUNDLE-CK ( n n -- ) {: base:n w:n :}
   w 1 = if
      base VGLUE-AT? if E-NELAB-MATCH throw then
      exit
   then
   w 0 ?do
      base i + VGLUE-AT? 0= if E-NELAB-MATCH throw then
   loop
   base 0 <= if exit then
   base 1- VGLUE-AT? if E-NELAB-MATCH throw then ;

\ `MATCH family`: open a frame over the bundle on top of the vector and build
\ nothing. The values below it are what every arm has to leave the vector at, the
\ width is the pre-pass's answer from the registry, and the ordinal is the number
\ a mismatch over this family traps with.
: DO-OPEN-MATCH ( n -- ) {: ix:n :}
   ix MWID@ {: w:n :}
   w VN @ > if E-NELAB-UNDER throw then
   VN @ w - {: base:n :}
   base w BUNDLE-CK
   HIR-CTRL:OPEN-MATCH base  ix JOIN-OF  CS-PUSH
   w CS-TOP CS-W!
   ix MTAG@ CS-TOP CS-TRAP! ;

\ `case`: the selector is the one value on top, and it stays there until an arm
\ matches or `endcase` drops it.
: DO-OPEN-CASE ( n -- ) {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- {: base:n :}
   base VGLUE-AT? if E-NELAB-MATCH throw then
   HIR-CTRL:OPEN-CASE base  ix JOIN-OF  CS-PUSH
   1 CS-TOP CS-W! ;

\ What the vector has to hold when an arm's test is reached: the values below the
\ form, and its subject - a `MATCH`'s whole bundle, or a `case`'s selector with
\ the key this arm just computed on top of it.
\
\ AND WHAT THE RETURN VECTOR HAS TO HOLD, which is what the form opened holding.
\ Every arm's test block is reached from the previous arm's mismatch stub, and a
\ stub carries what the block above it held; the first is the block the form
\ itself stands in. So a key computation between two arms that parked a value and
\ did not take it back would reach this test with a parked value the block was
\ never opened with, which is a program the checker cannot produce and this file
\ refuses rather than compiles into a guess.
: ARM-WIDTH-CK ( n -- ) {: t:n :}
   t CS-MATCH? if t CS-DEPTH@ t CS-W@ + else t CS-DEPTH@ 2 + then
   VN @ <> if E-NELAB-JOIN throw then
   RN @ t CS-RD@ <> if E-NELAB-JOIN throw then ;

\ The flag one arm tests. A `MATCH` compares the scrutinee's TAG, which is the
\ top cell of its bundle, with the constant its variant's declaration order is; a
\ `case` compares its selector with the key the arm computed. Either way the
\ value that has to survive the comparison is put back on the vector first,
\ because the next arm tests it again and an operation consumes what it reads -
\ and putting it back costs nothing at all, since a value already on the vector
\ appearing twice is what `dup` is.
: ARM-FLAG ( n n -- ) {: ix:n t:n :}
   t CS-MATCH? if
      VN @ 1- VAT VPUSH
      ix  ix MTAG@  EMIT-LIT
   else
      t CS-DEPTH@ VAT VPUSH
   then
   ix HIR-OPCODE:EQUAL EMIT-OPCODE ;

\ What the arm's own block starts with. A `MATCH` arm drops the tag and this
\ variant's zero pads and keeps its payload: the pads sit between the payload and
\ the tag, so dropping the top `1 + pads` cells leaves exactly the payload - the
\ same count the engine's own arm subtracts from the data-stack pointer
\ (src/habu/habu2.f EM-ADT-MATCH-OF). A `case` arm consumes the selector, which
\ is the source language's own rule for a matched arm.
\
\ AND WHAT THE CELLS IT KEEPS ARE. They were part of a bundle and the bundle is
\ gone, so the marks go with it - and what replaces them is the payload's own
\ answer. One cell is never a bundle. Several cells are one value exactly when
\ the variant's payload has fewer FIELDS than cells, because then one field is
\ wider than a cell and no exported per-field width says which; that is the same
\ answer, for the same reason, that src/compiler/native/dict.f gives a declared
\ row whose terms and cells disagree in number, and it is more than the truth in
\ the safe direction - it can only refuse a rename a finer answer would have
\ allowed, never let one through.
: ARM-GLUE ( n n bool -- ) {: base:n k:n one:bool :}
   base k VGLUE-CLEAR
   k 2 < if exit then
   one 0= if exit then
   base  k VRUN-MASK  VGLUE-RUN ;

: ARM-RESHAPE ( n n -- ) {: ix:n t:n :}
   t CS-MATCH? 0= if 1 VDROP exit then
   ix MPAD@ 1+ {: d:n :}
   d VN @ > if E-NELAB-UNDER throw then
   d VDROP
   t CS-DEPTH@ {: base:n :}
   base  VN @ base -  ix MONE@  ARM-GLUE ;

\ The block a tag that matched no variant runs into. It is the last arm's
\ mismatch edge and nothing else reaches it, so it takes no values: it stages the
\ family's ordinal and the terminator that leaves without returning, and
\ src/compiler/native/trap.f turns that ordinal back into the diagnostic the
\ process ends with.
\
\ THE ORDINAL IS STAGED FRESH AND THE LITERAL MEMO IS SCOPED AROUND THE WHOLE
\ BLOCK, for the two reasons the dead-call trap and the stub already give:
\ reusing an earlier block's value for a number this size would stretch that
\ value's live range across everything in between for the sake of code that never
\ runs, and a value THIS block defined would otherwise be visible to its SIBLING
\ - the arm's body, which the same two-way branch reaches and which this block
\ does not dominate.
: MATCH-TRAP ( n n -- ) {: ix:n ord:n :}
   LIT-MARK {: m:n :}
   ix OPEN-PLAIN
   ix ord HIR:ADDR-NONE STAGE-LIT
   ix HIR-OPCODE:TRAP EMIT-OPCODE
   CLOSE-BLOCK
   m LIT-RELEASE ;

\ `of`: the test, the two-way branch, the block the mismatch leaves through, and
\ the arm's own block. TERM-BRZ goes to its FIRST successor when the flag is
\ ZERO, so the first is the mismatch and the second is the arm - the polarity
\ `while` has and the opposite of `until`'s, and turning the two round would run
\ every arm but the one that matched.
: DO-ARM ( n -- ) {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   t ARM-WIDTH-CK
   ix t ARM-FLAG
   NB @ {: c:n :}
   ix  c 1+  c 2 +  TERM-BRZ
   ix MEND@ if ix t CS-TRAP@ MATCH-TRAP else ix  ix JOIN-OF JOIN-CK  STUB then
   ix OPEN-PLAIN
   ix t ARM-RESHAPE
   ix t CS-OFIX! ;

\ `endof`: the arm reaches the join, and the block that opens is the one its own
\ mismatch edge was branched to. The ordinal the build reached and the one the
\ skeleton wrote against this arm's `of` are two derivations of one number and
\ they are held against each other here.
: DO-CLOSE-ARM ( n -- ) {: ix:n :}
   CS-N @ 1 < if E-NELAB-CTRL throw then
   CS-TOP {: t:n :}
   t CS-ADT? 0= if E-NELAB-CTRL throw then
   t CS-OFIX@ {: ofix:n :}
   ofix 0 < if E-NELAB-CTRL throw then
   PATH-ENDED? if
      PATH-LIVE PATH-END !
   else
      ix  t CS-JOIN@ JOIN-CK  TERM-BR
      t CS-JOINED+
   then
   -1 t CS-OFIX!
   ofix MEND@ if exit then
   ofix JOIN-OF JOIN-CK {: nx:n :}
   NB @ nx <> if E-NELAB-CTRL throw then
   ix  t CS-DEPTH@ t CS-W@ +  t CS-RD@ +  OPEN-ARGS ;

\ The block after either form, taking the values every arm handed it. Its width
\ is the one the FIRST edge into it stated and every later edge was held to, so
\ it is read back off the block rather than derived a second time here; that the
\ block was stated at all is the same fact the frame recorded while the arms ran,
\ and the two are held against each other.
: ADT-JOIN ( n n -- ) {: ix:n t:n :}
   t CS-JOIN@ {: j:n :}
   t CS-JOINED? 0= if
      j 0 >= if E-NELAB-CTRL throw then
      PATH-DEAD PATH-END !
      exit
   then
   j JOIN-CK drop
   NB @ j <> if E-NELAB-CTRL throw then
   NB @ ARG-STATED? 0= if E-NELAB-JOIN throw then
   ix  NB @ ARG-WIDTH@  OPEN-ARGS
   PATH-LIVE PATH-END ! ;

: DO-CLOSE-MATCH ( n -- ) {: ix:n :}
   HIR-CTRL:OPEN-MATCH CS-OPENER-CK {: t:n :}
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   ix t ADT-JOIN
   CS-POP ;

\ `endcase`: the default path ends here, and it is the one path of a `case` that
\ is not an arm. It still holds the selector - the source language keeps it there
\ so that a default which produces a value leaves it on top - so the drop is the
\ whole of what this word stages, exactly as the engine stages one.
: DO-CLOSE-CASE ( n -- ) {: ix:n :}
   HIR-CTRL:OPEN-CASE CS-OPENER-CK {: t:n :}
   t CS-OFIX@ 0 >= if E-NELAB-CTRL throw then
   PATH-ENDED? if
      PATH-LIVE PATH-END !
   else
      VN @ t CS-DEPTH@ 1+ < if E-NELAB-UNDER throw then
      1 VDROP
      ix  t CS-JOIN@ JOIN-CK  TERM-BR
      t CS-JOINED+
   then
   ix t ADT-JOIN
   CS-POP ;

\ `construct family variant`: the payload is already on the vector, where the
\ source computed it, so what is left is what a value of the family carries
\ around it - this variant's zero pads, and then its tag. Both are constants, so
\ the form stages no operation this dialect did not have and no block at all: it
\ is the same two pushes the engine emits (src/habu/habu2.f EM-ADT-CON-PUSHES).
\
\ AND THE CELLS BECOME ONE VALUE, which is the only thing the vector learns here.
\ From this point on the payload, the pads and the tag are a value of the family,
\ so a rename that reached into them would take it apart - and one cell is not a
\ bundle, which is the same rule every other marker in this file follows.
: DO-MAKE-BUNDLE ( n -- ) {: ix:n :}
   ix MPAY@ {: k:n :}
   k VN @ > if E-NELAB-UNDER throw then
   VN @ k - {: base:n :}
   ix MPAD@ 0 ?do
      ix 0 EMIT-LIT
   loop
   ix  ix MTAG@  EMIT-LIT
   VN @ base - {: w:n :}
   base w VGLUE-CLEAR
   w 2 < if exit then
   base  w VRUN-MASK  VGLUE-RUN ;

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
\ then one per bound local, then one per parked return value, then the WHOLE
\ vector bottom first, whose top `a` values are the arguments.
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
   {: ix:n :}
   ix VN @ CELL-CROSS
   ix R-CROSS ;

\ The parked values as operands, bottom first, and they stand IN FRONT of the
\ vector for the reason the counters and the locals do: the arguments are the
\ last operands and nothing may come after them. They are live across the call
\ exactly as a crossing local is - the callee runs on the same machine and keeps
\ no register of the caller's - so a call consumes each of them and answers it
\ again, which is what puts them back below.
: R-OPERANDS+ ( -- )
   RN @ 0 ?do
      CTX BLD  i RAT  IR-BUILD:ADD-OPERAND
   loop ;

: CALL-OPERANDS+ ( -- )
   CALL-CROSS-CK
   NO-REAL-CK
   CTX BLD TOK IR-BUILD:ADD-OPERAND
   CROSS-DO LOOP-OPERANDS+
   CROSS-L LOCAL-OPERANDS+
   R-OPERANDS+
   VN @ 0 ?do
      CTX BLD  i VAT  IR-BUILD:ADD-OPERAND
   loop ;

\ Its results, one for one against those operands: the order again, then the
\ counters, then the locals, then the parked values, then one value per survivor
\ and one per output. `n` is the last group - what goes back on the vector -
\ because that is the only count either caller works out for itself.
\
\ IT READS `RN` AND SO IT IS ONE NUMBER ONLY WHILE ONE CALL IS BEING STAGED, which
\ is all either reader needs: CALL-RESULTS+ declares the results and CALL-CLOSE
\ reads them back, both inside the same staging, and nothing between them moves a
\ value between the two vectors.
: CROSS-RESULTS ( -- n )
   CROSS-N 2 *  CROSS-L +  RN @ + ;

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

\ And its answer for the parked values, which stand behind the locals. Each one
\ comes back a NEW value id, because the call really did consume it: the value the
\ caller parked is in a register the callee may destroy, and what the site holds
\ afterwards is the result the operation answered.
: R-RESULTS@ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   RN @ 0 ?do
      CTX BLD id  CROSS-N 2 * CROSS-L + i + 1+  IR-BUILD:OP-RESULT@  i RSTK !
   loop ;

: LOCAL-RESULTS@ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id l:n :}
   l LOCAL-CK 0= if exit then
   0 LRK !
   LBN @ 0 ?do
      i LCROSS? if
         CTX BLD id  CROSS-N 2 * LRK @ + 1+  IR-BUILD:OP-RESULT@  i LVAL !
         LRK @ 1+ LRK !
      then
   loop ;

\ Closing either call form: everything it consumed goes and what it answered
\ takes its place - the order into its slot, each loop's counters back into their
\ frame, each local's value back under its name, each parked value back into the
\ return vector, and the survivors and outputs onto the vector.
\ THE SURVIVORS COME BACK AS THE SAME VALUES AND SO WITH THE SAME MARKS. A call
\ hands the whole vector over and takes it back, so every value the caller still
\ held is answered again; a value that was several cells before the call is the
\ same several cells after it, and forgetting that here would let a rename below
\ the call take apart what a rename above it could not. The callee's own results
\ are marked from its declared effect, which is the other of the two places
\ anything wider than a cell enters this vector.
: CALL-CLOSE ( n n n -- )
   {: n:n out:n oglue:n :}
   VGLUE @ {: keep:n :}
   VQ-SAVE
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   VN @ VDROP
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK!
   id CROSS-DO LOOP-RESULTS@
   id CROSS-L LOCAL-RESULTS@
   id R-RESULTS@
   n 0 ?do
      CTX BLD id  i 1+ CROSS-RESULTS +  IR-BUILD:OP-RESULT@ VPUSH
   loop
   n out - {: k:n :}
   k VQ-KEEP
   0  keep k VGLUE-LOW  VGLUE-RUN
   k oglue VGLUE-RUN ;

\ ---- a call control does not come back from ----------------------------------
\ WHAT A DEAD CALL IS AND WHAT IT IS NOT. It is an ORDINARY call: the same
\ operation, the same operands, the same branch-with-link to the same address.
\ `throw` really runs, and a `catch` around the caller really catches it -
\ trapping INSTEAD of calling would turn a catchable throw into a process exit
\ and change what the program does. What is different is only what follows: the
\ checker certified that control does not come back, so the values the vector
\ holds afterwards reach nothing and the path stops here.
\
\ AND A BLOCK STILL HAS TO END. src/compiler/ir/verify.f wants exactly one
\ terminator and it has to be the block's last operation, so the instruction
\ after the call is not optional - without one the block would fall into
\ whichever block the emitter laid out next. `hir.return` cannot be it: a
\ routine publishes its results in one place and src/compiler/native/select.f
\ refuses a second (E-A64SEL-PLACE). `hir.trap` is the terminator that leaves
\ without returning, and this is what it says here: if control ever DOES arrive
\ at this instruction, the certificate this routine was compiled against was
\ false, and the trap routine names the callee that broke it and ends the
\ process. It is unreachable by construction and it is not decoration - it is
\ the difference between a false certificate being reported and a routine
\ quietly running somebody else's block.
128 constant DN-CAP                  \ bytes of a callee's spelling this asks about
here CELL 1- and CELL swap - CELL 1- and allot
create DN-BUF DN-CAP allot

\ The ordinal the trap site carries: src/compiler/native/trap.f's own number for
\ "this word returned", keyed by the CALLEE's spelling, which is the only name
\ that would be worth printing. Registering is idempotent, so every site over
\ one callee carries one number and a re-elaborated definition adds no row.
: DEAD-ORD ( IR-ID:ir-symbol-id -- n )
   {: sy:IR-ID:ir-symbol-id :}
   CTX BLD sy DN-BUF DN-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   DN-BUF u NTRAP:NO-RETURN ;

\ THE ORDINAL IS STAGED FRESH AND NOT TAKEN OFF THE LITERAL MEMO, which is the
\ one place the memo would cost something real. The memo answers with a value an
\ EARLIER block defined, and every ordinal is a small number that a body is
\ likely to have staged already - so a trap would read a value defined before the
\ branch, giving that value a live range that reaches from wherever it was
\ defined, across every call on the way, to an instruction that never runs. The
\ register allocator then has to keep a register alive across those calls for the
\ sake of unreachable code, and a class it may not put in a frame - a value read
\ outside the entry and exit blocks - has nowhere else to be (E-A64RA-POOL,
\ measured on `: JT ( n n -- n ) 0 = if drop E-A-EMPTY throw then ;`, whose
\ ordinal happened to be the same 0 its own comparison had staged). A constant of
\ its own costs one instruction on a path that never runs and nothing anywhere
\ else.
: DEAD-END ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix  ix WSYM DEAD-ORD  HIR:ADDR-NONE STAGE-LIT
   ix HIR-OPCODE:TRAP EMIT-OPCODE
   CLOSE-BLOCK
   PATH-DEAD PATH-END ! ;

\ A self-call empties the memo without asking, because the routine it calls is
\ the one being compiled: its clobber record is written when its allocation is
\ accepted, which is after this walk, so there is nothing to ask and the whole
\ pool is what a crossing value is refused.
: DO-SELF-CALL ( n -- )
   {: ix:n :}
   IN-N @ OUT-N @ CALL-LIVE  OUT-N @ + {: back:n :}
   ix CALL-CROSS
   CTX BLD HIR-OPCODE:CALL HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CALL-OPERANDS+
   back CALL-RESULTS+
   back OUT-N @ OUT-GLUE @ CALL-CLOSE
   LIT-RESET ;

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
\ One call to an address, staged. It is written once because there are two
\ callers with two different ways of knowing the three numbers: a name the body
\ CALLS reads them off the word model's row, and `is` works them out from the
\ deferred word the token after it names. What the staging itself does is the
\ same either way, and it is exactly `RECURSE`'s minus the arity: everything the
\ caller holds crosses the operation, for the reason
\ src/compiler/native/hir.f gives.
: STAGE-WCALL ( n n n n n -- )
   {: ix:n entry:n a:n o:n oglue:n :}
   a o CALL-LIVE o + {: back:n :}
   ix CALL-CROSS
   CTX BLD HIR-OPCODE:WORDCALL HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CALL-OPERANDS+
   back CALL-RESULTS+
   entry a o WCALL-ATTRS+
   back o oglue CALL-CLOSE
   entry LIT-CALL-BARRIER ;

: DO-WORD-CALL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   ix  r sy HIR-WORD:ENTRY@
   r sy HIR-WORD:CALLEE-IN@  r sy HIR-WORD:CALLEE-OUT@
   r sy HIR-WORD:OUT-GLUE@  STAGE-WCALL
   r sy HIR-WORD:CALLEE-DEAD? if r ix DEAD-END then ;

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
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
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
   ix base o CELL-CROSS-RUN
   \ A COPIED CALL LEAVES THE SAME VALUES A MADE ONE WOULD. The spliced tokens
   \ are literals and operations, so each of them leaves one cell and marks
   \ nothing - but what the callee DECLARED it leaves is unchanged by the copy,
   \ and a value of its several cells is still one value here. So the callee's
   \ own answer is stated over the results, exactly as DO-WORD-CALL states it.
   base  r sy HIR-WORD:OUT-GLUE@  VGLUE-RUN ;

\ ---- what `[:` stages ---------------------------------------------------------
\ ONE VALUE AND NONE OF THE BODY'S OPERATIONS. The tokens between the pair are
\ another function of this emission and this walk never reaches them - the
\ pre-scan marked them and STEP steps over them - so what stands here is one
\ value: the address that function is entered at, which is `hir.quot`.
\
\ WHICH FUNCTION IT IS, AS AN ORDINAL. It is this definition's own function plus
\ the body's row plus one, which is the order the loop at the end of COLON builds
\ them in - and that loop ASSERTS the agreement rather than trusting it, because
\ an ordinal stated here and a function built there are two derivations of one
\ number and a disagreement would be a value pointing into another routine.
\
\ THE ENTRY IS MARKED, AND THAT IS THE WHOLE OF HOW A BODY LEARNS ITS ARITY.
\ Nothing here knows what the body takes and leaves; the numbers belong to
\ whichever term consumes the value. So the vector entry this operation pushed is
\ marked with the body's row, the mark travels with the entry through every
\ rename and every call that carries it, and the consumer writes the numbers.
: DO-QUOT ( n -- )
   {: ix:n :}
   ix QOPENED@ {: k:n :}
   k 0 < if ix QUOT-REFUSE then
   CTX BLD HIR-OPCODE:QUOT HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   CTX BLD VW MKEY ix op OPEN
   CTX BLD op RESULTS+
   CTX BLD  CTX BLD HIR:KEY-FUN
   CTX BLD  k QFUN@  IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD op CLOSE
   k  VN @ 1-  VQ! ;

\ ---- binding a quotation to a deferred word -----------------------------------
\ WHAT `is` DOES, AND WHY IT IS A CALL. `[: … ;] is FOO` stores an execution
\ token into FOO's dispatch cell - and that cell, once it holds one, is a cell of
\ the DP heap holding a JIT-region address, which a snapshot has to move with the
\ region or a restored image jumps into the writing run's memory on FOO's first
\ call (dot habu-relocate-persisted-defer-7aa681c4). Storing and declaring the
\ cell are therefore one operation, and the engine already has it: `xt!`
\ (src/habu/habu2.f BXTSTORE), whose own header says the two are one primitive
\ "so neither half can be done without the other and the declaration cannot drift
\ away from the store it describes". So this compiles to a call to that
\ primitive rather than to a store of its own, and there is no way to emit the
\ store while forgetting the declaration.
\
\ WHERE THE ARITY OF THE QUOTATION COMES FROM, AND IT IS NOT FROM HERE. The
\ checker binds `is` by taking the DEFERRED WORD's declared effect, making a
\ quotation of it and unifying that against the value on the stack
\ (src/core/checker.f IS-TOK, which reads the target's signature through
\ CHECKER-FIND-ACTIVE-SIG and applies it with IS-APPLY). So the deferred word's
\ own declared effect IS what the body must be, and that is what fills the
\ consumption row - the same number the checker held the program to, read off
\ the same authority every other arity in this file comes from.
\
\ THE VALUE NEED NOT BE A BODY THIS DEFINITION WROTE. `: X ( [ n -- n ] -- ) is
\ FOO ;` binds a quotation that arrived as an argument, and there is no row here
\ to fill; the store is the same store. So the row is filled when there is one
\ and the call is staged either way.
\
\ AND THE ADDRESS IS A CONSTANT OF THIS EMISSION. Which cell FOO dispatches
\ through is decided when FOO is declared and never again, so it is a literal -
\ exactly as a `create`d word's address is - staged by the same word an integer
\ in the source stages. The cell goes on top of the quotation because that is
\ the order `xt!` takes them: the token is the deeper of the two, which is the
\ order Forth writes `value address !` in.
: DO-IS ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   ix 1+ QSPELL {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u NDICT:SPELL-DEFER-CELL {: cell:n :}
   cell 0= if E-NELAB-DEFER throw then
   a u NDICT:SPELL-ARITY {: din:n dout:n :}
   din NDICT:ARITY-NONE = if E-NELAB-DEFER throw then
   dout NDICT:ARITY-NONE = if E-NELAB-DEFER throw then
   s" xt!" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if E-NELAB-DEFER throw then
   VN @ 1- VQ@ {: k:n :}
   k 0 >= if k din dout QFILL then
   ix cell EMIT-LIT
   ix entry 2 0 NDICT:GLUE-NONE STAGE-WCALL ;

\ ---- entering the routine a quotation names ----------------------------------
\ WHAT `execute` COMPILES TO. A branch-with-link to the engine's own `execute`,
\ which is what a compiled `execute` is in the engine too: the routine it enters
\ is not known where the call is written, so there is nothing to inline and
\ nothing to fold. The one thing this site knows and the callee does not is how
\ many cells cross it, and that is what the operation carries.
\
\ THE ARITY IS THE QUOTATION'S CERTIFIED EFFECT AND NOTHING ELSE. It takes one
\ cell more than the quotation takes - the execution token itself, which the
\ engine pops before it branches - and leaves exactly what the quotation leaves.
\ Both numbers come off the row the value's mark names, which is the checker's
\ answer either way: a body written here was told by the term that consumed it,
\ and a quotation that arrived was told by the effect this definition declared.
\
\ GUESSING FROM THE SITE'S STACK SHAPE IS NOT AN OPTION and the refusal below is
\ what says so. A cell reaching `execute` with no row is a cell nothing certified
\ as a quotation - an xt out of a variable, a quotation carried across a branch,
\ or a body this definition wrote that no consumer ever gave an arity - and the
\ vector's depth would let a guess through every time. The checker refuses the
\ first of those on its own (an opaque xt is not a T-QUOT); the other two are
\ refused here, by the same name every other quotation refusal carries.
\
\ NOTHING IS SAVED ACROSS IT, which is right rather than unfortunate. The
\ operation consumes every live value and answers it again, exactly as any call
\ to an address does, so the caller keeps full save discipline against a callee
\ nobody can name - which is the only sound assumption about a routine chosen at
\ run time.
: DO-EXEC ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- VQ@ {: k:n :}
   k 0 < if ix QUOT-REFUSE then
   k QIN@ {: qin:n :}
   k QOUT@ {: qout:n :}
   qin QNONE = if ix QUOT-REFUSE then
   s" execute" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if ix QUOT-REFUSE then
   ix entry  qin 1+  qout  NDICT:GLUE-NONE  STAGE-WCALL ;

\ ---- running a quotation and coming back either way --------------------------
\ WHAT `catch` COMPILES TO, AND WHY IT IS THE SAME KIND OF THING `execute` IS. A
\ branch-with-link to the engine's own `catch`, which is what a compiled `catch`
\ is in the engine too. The exceptional path is not a second edge out of this
\ site and there is nothing here to model one with: the engine's handler resumes
\ INSIDE its own `catch`, and both paths leave it through the one return to the
\ one caller, so a two-successor terminator would describe a control transfer the
\ machine does not make.
\
\ WHAT THE TWO COUNTS SAY, AND WHY BOTH OF THEM ARE THE WINDOW. The engine puts
\ the data stack back to its DEPTH when a caught body throws, and never to its
\ CONTENTS: every cell of the window may hold something the body left there, and
\ the caller is expected to read it. Residency across the call is NOT what this
\ site has to arrange - CALL-OPERANDS+ hands the WHOLE vector over and
\ CALL-CLOSE takes it back, so every live value crosses any call through its
\ data-stack home and comes back a new value. What this site has to state is
\ which of those cells are the callee's OUTPUTS rather than survivors, and the
\ window is all of them: a survivor comes back in the position it left and keeps
\ what the vector knew about it, and an output is a value the callee produced.
\ A caught throw can replace any cell of the window - including one that held the
\ address of a quotation body - so calling those cells survivors would carry a
\ mark across a call that may have overwritten what it names.
\
\ AND THE TWO COUNTS HAVE TO AGREE WITH EACH OTHER. Measured: declaring the
\ window on one side only leaves the vector `win` cells short or long at the
\ return and is refused (E-NELAB-ARITY, both directions). Declaring it on
\ NEITHER side compiles and answers correctly today, because the whole-vector
\ crossing above already puts the cells through memory - which is why the
\ statement here is the callee's real contract rather than a residency trick:
\ what the engine's `catch` consumes and produces at this site is the window
\ plus one, twice over.
\
\ AND THE WINDOW IS THE CHECKER'S ANSWER AND NOT THIS SITE'S STACK DEPTH. The
\ vector's depth would let a guess through at every catch, exactly as it would at
\ `execute` - the refusal above says so for the same reason - and it would be
\ WRONG in the ordinary direction too: a body that takes nothing may be caught
\ over a stack ten deep. src/compiler/native/dict.f answers what the checker
\ certified for THIS site, and a site it has no answer for is refused by name.
\
\ A PARKED VALUE CROSSES THIS CALL THE WAY IT CROSSES ANY OTHER, and that is a
\ consequence of going through STAGE-WCALL rather than a rule stated here. The
\ return stack's live values are operands and results of every call - R-OPERANDS+
\ stands inside CALL-OPERANDS+ and R-RESULTS@ inside CALL-CLOSE - so a `>r` whose
\ `r>` is on the far side of a catch is handed over and answered again exactly as
\ a data value is, and there is nothing about the exceptional path for this site
\ to do about it: the engine's handler restores the user return stack's depth
\ itself (src/habu/habu2.f), and under this lowering the parked values were never
\ in that memory to begin with. Measured both ways in
\ test/compiler/native-catch.f: 42 parked across a catch comes back 42 whether
\ the caught body threw or returned, while the window cell under it answers the
\ body's leavings on one path and its result on the other.
\
\ THE TOKEN AND THE CODE ARE THE SAME CELL'S WORTH. The execution token is
\ consumed by the engine before it branches and the throw code is pushed in its
\ place, so the call takes the window plus the token and leaves the window plus
\ the code. The two counts being equal is a fact about `catch` rather than an
\ arithmetic coincidence: catch is stack-preserving by construction, which is why
\ the checker can fit-check one live row against both of the quotation's rows,
\ and this site holds the two widths it was given against each other.
\
\ A BODY THAT NEVER RETURNS IS REFUSED HERE, AND THE REASON IS NOT THIS SITE'S.
\ The checker answers absent for such a body's output row - it has no
\ fall-through to instantiate one for - and everything this site would do with it
\ is settled: the window is the same on both sides, because the engine restores
\ the depth it took. What cannot be built is the BODY, and the ceiling is one
\ module further down: src/compiler/native/select.f takes ONE routine contract
\ for the whole module and lowers every function under it, so a routine with no
\ return inside a definition that has one is lowered as though a return followed
\ and leaves its last memory order unread. Measured both ways on this tree:
\ `: X ( n -- n n ) [: drop 5 throw ;] catch ;` is refused by the allocation
\ validator (E-A64RAV-ORDER), and the same body inside a definition that ends in
\ a throw of its own compiles and runs. Refusing it here rather than letting the
\ first shape through is the fail-closed direction AND the honest one: a rule
\ that admitted a body because the definition around it happens to be dead would
\ be resting on the enclosing word's shape, which is not what makes it sound. The
\ capability is dot habu-compile-a-quotation-7efa798e, which already carries this
\ exact finding - a contract describes function zero only, and a body that never
\ returns needs a per-function control statement no contract field expresses. The
\ engine's answers for the refused shape are pinned in
\ test/compiler/native-catch.f so they are ready for it.
: DO-CATCH ( n -- )
   {: ix:n :}
   VN @ 1 < if E-NELAB-UNDER throw then
   VN @ 1- VQ@ {: k:n :}
   k 0 < if ix QUOT-REFUSE then
   ix NDICT:CATCH-CELLS {: win:n back:n :}
   win NDICT:CATCH-NONE = if ix QUOT-REFUSE then
   back NDICT:CATCH-NONE = if ix QUOT-REFUSE then
   back win <> if ix QUOT-REFUSE then
   VN @ 1- win < if E-NELAB-UNDER throw then
   s" catch" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if ix QUOT-REFUSE then
   k win win QFILL
   ix entry  win 1+  win 1+  NDICT:GLUE-NONE  STAGE-WCALL ;

\ Either way of reaching another word's body. Which one this token is was decided
\ once, before any walk started, and is read here rather than asked again.
\
\ WHAT THE CALL IS TOLD BEFORE IT IS BUILT is which of the cells it consumes are
\ quotation bodies, and what this callee declares each of them to be. It is asked
\ here rather than inside either arm because it is the same question whichever way
\ the callee is reached: a copied call consumes the caller's cells exactly as a
\ made one does, and a body handed to one is a body handed to the other.
: DO-CALL ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   ix  r  ix WSYM  HIR-WORD:CALLEE-IN@  QCALL-FILL
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
\ to the block the return is in. The arm is finished: the path-end record says
\ so, and the only word that may follow is the `then` that closes it.
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
   PATH-EXIT PATH-END ! ;

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
\ the row the pre-pass recorded for the NEXT group to bind, so the two walks
\ agree about where the groups are and about their order; and no control
\ structure may be open, because a group inside one would bind names on a path
\ that does not dominate the rest of the body and this elaborator has no scoping
\ rule for that (dot habu-scope-a-locals-2faa3d7a).
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
\
\ ONLY THE NAMES THIS GROUP JUST BOUND ARE CROSSED, because they are the only
\ ones that can still hold a double: an earlier group's names were crossed at
\ their own closer and a later group's hold no value at all.
: BIND-CROSS-ONE ( n n -- )
   {: ix:n k:n :}
   k LCROSS? 0= if exit then
   k LVAL @ REAL-VALUE? 0= if exit then
   ix  k LVAL @  HIR-OPCODE:REALBITS CROSS-VALUE  k LVAL ! ;

: LOCAL-BIND-CROSS ( n n n -- )
   {: ix:n from:n k:n :}
   k 0 ?do
      ix  from i +  BIND-CROSS-ONE
   loop ;

: DO-CLOSE-LOCALS ( n -- )
   {: ix:n :}
   LGB @ LG-N @ >= if E-NELAB-LOCAL throw then
   ix LGB @ LG-B@ <> if E-NELAB-LOCAL throw then
   CS-N @ 0<> if E-NELAB-LOCAL throw then
   LGB @ LG-K@ {: k:n :}
   LBN @ {: from:n :}
   k VN @ > if E-NELAB-UNDER throw then
   VN @ k - {: base:n :}
   k 0 ?do
      base i + VAT  from i + LVAL !
      base i + VQ@  from i + LQ!
   loop
   k VDROP
   ix from k LOCAL-BIND-CROSS
   from k + LBN !
   LGB @ 1+ LGB ! ;

\ A mention of a bound local in the body: the value it names goes back on the
\ vector. It produces no operation, exactly as a rename does, because the value
\ already exists - whatever computed it - and this only says where it is used.
\
\ THE SECOND TEST IS A BACKSTOP AND IS ASKED ANYWAY. LOCAL-OF answers only for a
\ name whose group closed on an EARLIER row, and the walk reaches the rows in
\ order and binds each group as it passes its closer, so a name it answers for
\ is one the walk has already bound. Reaching the refusal would mean those two
\ orders had come apart.
: LOCAL-READ? ( n -- bool )
   {: ix:n :}
   ix LOCAL-OF {: k:n :}
   k 0 < if false exit then
   k LBN @ >= if E-NELAB-LOCAL throw then
   k LAT LVAL @ VPUSH
   k LQ@  VN @ 1-  VQ!
   true ;

\ The whole control table. Every arm names the blocks one source control word
\ builds; nothing else in this file decides what a control word means.
: DO-CONTROL ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r  ix WSYM  HIR-WORD:CTRL@
   MATCH HIR:ctrl
      open-if      OF ix DO-OPEN-IF ENDOF
      mid-else     OF ix DO-ELSE ENDOF
      close-if     OF ix DO-CLOSE-IF ENDOF
      open-begin   OF ix DO-OPEN-BEGIN ENDOF
      mid-while    OF ix DO-WHILE ENDOF
      close-until  OF ix DO-CLOSE-UNTIL ENDOF
      close-repeat OF ix DO-CLOSE-REPEAT ENDOF
      close-again  OF ix DO-AGAIN ENDOF
      open-do      OF ix DO-OPEN-DO ENDOF
      open-do-skip OF ix DO-OPEN-DO-SKIP ENDOF
      close-loop   OF ix DO-CLOSE-LOOP ENDOF
      index        OF DO-INDEX ENDOF
      drop-loop    OF DO-UNLOOP ENDOF
      early-leave  OF ix DO-LEAVE ENDOF
      early-exit   OF ix DO-EXIT ENDOF
      self-call    OF ix DO-SELF-CALL ENDOF
      open-match   OF ix DO-OPEN-MATCH ENDOF
      match-arm    OF ix DO-ARM ENDOF
      close-arm    OF ix DO-CLOSE-ARM ENDOF
      close-match  OF ix DO-CLOSE-MATCH ENDOF
      open-case    OF ix DO-OPEN-CASE ENDOF
      close-case   OF ix DO-CLOSE-CASE ENDOF
      make-bundle  OF ix DO-MAKE-BUNDLE ENDOF
      open-quot    OF ix DO-QUOT ENDOF
      close-quot   OF ix QUOT-REFUSE ENDOF
      bind-defer   OF ix DO-IS ENDOF
      exec         OF ix DO-EXEC ENDOF
      catch        OF ix DO-CATCH ENDOF
   ;MATCH ;

\ ---- the walk ----------------------------------------------------------------
variable IX                          \ the body token the walk stands on

\ One body token. The word model answers what it is; a literal, an operation word
\ and a constant-and-operation word each stage operations, a rename stages none,
\ and a control word builds blocks. `unmodeled` never reaches the match -
\ HIR-WORD:ADMIT refuses it first - and the arm throws the same refusal rather
\ than inventing a second name for it.
\ After a path has ended, the only token that may follow is the one that closes
\ the structure the ended path was an arm of - and which of the two closers that
\ is depends on HOW the path ended. A dead arm may be followed by its `else`,
\ because the block it left is closed and the second arm opens a new one; an
\ `exit` may not, for the reason SK-ELSE gives. This is the same question the
\ skeleton asks, and it is asked here again because the two walks are separate
\ readings of one body and each has to be able to refuse it alone.
: AFTER-END-CK ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena ix:n :}
   r ix HIR-MEANING:CONTROL MODELED-AS? 0= if E-NELAB-CTRL throw then
   r  ix WSYM  HIR-WORD:CTRL@ {: k:HIR:ctrl :}
   k HIR-CTRL:CLOSE-IF HIR-CTRL:EQ if exit then
   k HIR-CTRL:MID-ELSE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-ARM HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   k HIR-CTRL:CLOSE-CASE HIR-CTRL:EQ  PATH-DEAD?  and if exit then
   E-NELAB-CTRL throw ;

: STEP ( IR-ARENA:arena IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena ix:n :}
   VW ix NTAPE-MODE:COMPILING MODE-CK
   ix QSKIP? if exit then
   ix MOPERAND? if exit then
   PATH-ENDED? if r ix AFTER-END-CK then
   ix IN-DECL? if exit then
   ix LOCAL-READ? if exit then
   r ix ADMIT-AT
   MATCH HIR:meaning
      literal      OF ix EMIT-CONST ENDOF
      real-literal OF ix EMIT-FCONST ENDOF
      string-literal OF ix EMIT-STRING ENDOF
      op           OF r ix EMIT-OP ENDOF
      const-op     OF r ix EMIT-CONST-OP ENDOF
      fixed        OF r ix EMIT-FIXED ENDOF
      callable     OF p r ix DO-CALL ENDOF
      control      OF r ix DO-CONTROL ENDOF
      rename       OF p r  ix WSYM  RENAME ENDOF
      rstack       OF r  ix WSYM  RSTACK-STEP ENDOF
      open-locals  OF E-NELAB-LOCAL throw ENDOF
      close-locals OF ix DO-CLOSE-LOCALS ENDOF
      unmodeled    OF E-HIR-UNMODELED throw ENDOF
   ;MATCH ;

\ Walk the body: every row after the name, to the end of the tape. The tape's
\ end is the definition's end, because the tape IS one definition - the unit the
\ producer opened and sealed around one scan - so there is nothing to look for
\ and nothing can follow.
: WALK ( IR-ARENA:arena IR-ARENA:arena n n -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena lo:n hi:n :}
   lo IX !
   begin
      IX @ hi <
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
: SK-KEEP ( IR-ARENA:arena n n -- IR-ARENA:arena n n )
   {: r:IR-ARENA:arena lo:n hi:n :}
   r lo hi SKELETON
   r lo hi ;

: SKELETON-TRY ( IR-ARENA:arena n n -- )
   [: SK-KEEP ;] catch {: rc:n :}
   2drop drop
   rc 0= if exit then
   RF-RECORD
   rc throw ;

: WALK-KEEP ( IR-ARENA:arena IR-ARENA:arena n n -- IR-ARENA:arena IR-ARENA:arena n n )
   {: p:IR-ARENA:arena r:IR-ARENA:arena lo:n hi:n :}
   p r lo hi WALK
   p r lo hi ;

: WALK-TRY ( IR-ARENA:arena IR-ARENA:arena n n -- )
   [: WALK-KEEP ;] catch {: rc:n :}
   2drop 2drop
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

\ ---- the name a quotation's own function carries ------------------------------
\ A module's function table is keyed by a SYMBOL, so every function of an
\ emission needs a name of its own and the tokens a quotation is written with are
\ `[:` for all of them. The name is built here from two things that are already
\ unique: the definition's own name, which no other function of this module
\ carries, and the body's row, which no other body of this definition carries. It
\ is spelled so a reader of a dump knows what they are looking at -
\ `QP-ACT[:0` - and it is a name and not a number because that is what the table
\ takes.
\
\ A NAME THAT DOES NOT FIT IS REFUSED RATHER THAN TRUNCATED, for the reason the
\ refusal record above gives about a spelling: a truncated name is some OTHER
\ name, and two bodies of one long-named definition would then be one symbol.
128 constant QNAME-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create QNAME-BUF QNAME-CAP allot
variable QNAME-U

: QNAME+ ( ptr u8 n -- )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   QNAME-U @ u + QNAME-CAP > if E-NELAB-QUOT-CAP throw then
   a  QNAME-BUF QNAME-U @ +  u BYTE-COPY
   QNAME-U @ u + QNAME-U ! ;

variable QNAME-P                     \ the place value the digit loop is on

: QNAME-DIGIT ( n -- )
   {: d:n :}
   QNAME-U @ 1+ QNAME-CAP > if E-NELAB-QUOT-CAP throw then
   QNAME-BUF QNAME-U @ +  48 d +  swap c!
   QNAME-U @ 1+ QNAME-U ! ;

\ The row as digits, most significant first. It is written as a division loop
\ over the whole number rather than for the two digits QMAX needs today, so a
\ raised FMAX needs nothing here and no second ceiling is stated that could come
\ to disagree with the one at the top of this section.
: QNAME-DIGITS ( n -- )
   {: k:n :}
   1 QNAME-P !
   begin  k  QNAME-P @ 10 *  >=  while
      QNAME-P @ 10 * QNAME-P !
   repeat
   begin
      k QNAME-P @ /  10 mod  QNAME-DIGIT
      QNAME-P @ 10 / QNAME-P !
      QNAME-P @ 0=
   until ;

: QNAME ( n -- ptr u8 n )
   {: k:n :}
   0 QNAME-U !
   0 QSPELL QNAME+
   s" [:" QNAME+
   k QNAME-DIGITS
   QNAME-BUF QNAME-U @ ;

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
   loop
   0 IN-GLUE @ VGLUE-RUN
   in QPARAMS-OPEN ;

\ ---- one quotation body, as a function of its own -----------------------------
\ THE SAME FOUR STEPS THE DEFINITION'S OWN FUNCTION IS BUILT WITH, over a range
\ of the same tape and under the arity its consumer stated. Everything a body
\ needs was decided before the enclosing walk ran: which tokens are its own, what
\ each of them means, which of its calls are copied, whether an order has to be
\ minted - all of those pre-scans read the WHOLE tape, so their tables already
\ cover a body's tokens and no second derivation is made here.
\
\ IT IS BUILT AFTER THE ENCLOSING FUNCTION IS CLOSED, which is measured and not
\ chosen: src/compiler/ir/build.f refuses a second BEGIN-FUN while one is open
\ (E-IR-BUILD-STAGE), so there is no arrangement in which a body is built where
\ its `[:` stands.
\
\ ITS NAME IS ITS OWN AND ITS VISIBILITY IS HIDDEN. A quotation body is reached
\ only through the address the enclosing routine holds - nothing outside this
\ module can name it, and src/compiler/native/publish.f puts no record in the
\ dictionary for it - so exporting it would claim a way in that does not exist.
\ Its span is the `[:` the source wrote, which is the token a diagnostic about
\ the body can point a reader at.
: QOPEN-FUN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      k:n :}
   k QIN@ {: in:n :}
   k QOUT@ {: out:n :}
   in out ARITY-CK
   c b  c b  k QNAME  IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   c b  c b in out SIGNATURE  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:HIDDEN IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  v key k QAT@ NTAPE:SPAN@  IR-BUILD:SET-FUN-SPAN ;

: QOPEN-BLOCK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      k:n :}
   k QIN@ {: in:n :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  v key k QAT@ NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   LIT-RESET
   in 0 ?do
      c b  c b CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG VPUSH
   loop ;

\ The body's own return, staged at the `;]` that ends it - the token whose whole
\ meaning is "this routine leaves here".
: QEMIT-RETURN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      k:n :}
   k QOUT@ {: out:n :}
   VN @ out <> if k QAT@ QUOT-REFUSE then
   RN @ 0<> if k QAT@ QUOT-REFUSE then
   out RETURN-CROSS
   c b HIR-OPCODE:RETURN HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b v key  k QHI@  op OPEN
   out 0 ?do
      c b  VN @ out - i + VAT  IR-BUILD:ADD-OPERAND
   loop
   out VDROP
   c b IR-BUILD:END-OP drop ;

\ WHAT THE ORDINAL ASSERTION IS FOR. Every `hir.quot` staged during the enclosing
\ walk named its callee by ORDINAL, worked out from the row it was staged for;
\ this loop builds the functions those ordinals have to denote. Two derivations of
\ one number, so the second is held against the first: a body built in another
\ order, or a builder that had already held a function nobody counted, would leave
\ every quotation value pointing at the wrong routine with nothing downstream able
\ to tell - a code address is an ordinary cell to every pass after this one.
: QBUILD ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      p:IR-ARENA:arena r:IR-ARENA:arena k:n :}
   b IR-BUILD:FUNS  k QFUN@  <> if k QAT@ QUOT-REFUSE then
   k QLO@ {: lo:n :}
   k QHI@ {: hi:n :}
   k QCUR !
   r lo hi SKELETON-TRY
   c b v key k QOPEN-FUN
   c b v key k QOPEN-BLOCK
   TOK-NEED @ 0<> if k QAT@ EMIT-MEM then
   PATH-LIVE PATH-END !
   p r lo hi WALK-TRY
   CS-N @ 0<> if E-NELAB-CTRL throw then
   PATH-END @ PATH-EXIT = if E-NELAB-CTRL throw then
   EXIT-USED @ 0<> if k QAT@ QUOT-REFUSE then
   \ A BODY WITH NO FALL-THROUGH WOULD HAVE NO RETURN, and this chain cannot
   \ compile one yet. It is not the elaborator that stops it: the shape is
   \ straightforward here - stage no return, exactly as a definition's own
   \ function does when its last path ended - and it was measured working, but
   \ only inside a definition that never returns either. The reason is one module
   \ ceiling further down: src/compiler/native/select.f takes ONE routine contract
   \ for the whole module and lowers every function of it under that contract, so
   \ a never-returning body inside a definition that DOES return is lowered as if
   \ a return followed it and leaves its memory order unread (E-A64RAV-ORDER,
   \ measured). Both consumers refuse such a body before it reaches here - DO-CATCH
   \ by name, and `execute` because src/compiler/native/dict.f declines the descent
   \ for a quotation that can throw - so this is the wall that keeps the refusal a
   \ loud failure if either of them is ever relaxed without the contract becoming
   \ per-function (dot habu-compile-a-quotation-7efa798e).
   PATH-DEAD? if k QAT@ QUOT-REFUSE then
   c b v key k QEMIT-RETURN
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   QOWNER-DEF QCUR ! ;

: QBUILD-ALL ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      p:IR-ARENA:arena r:IR-ARENA:arena :}
   QN @ 0 ?do
      i QFUN@ QPARAM <> if c b v key p r i QBUILD then
   loop ;

\ A `{: … :}` group inside a quotation body, which this leaf refuses. The group
\ machinery binds its names with a cursor that the enclosing walk advances as it
\ passes each closer, and a walk that SKIPS a body passes none of the body's - so
\ the body's own build would read the cursor at whatever the enclosing walk left
\ it. Refusing is the loud answer; making the cursor a lookup instead of a running
\ count is dot habu-bind-a-locals-923668b9. The measured cost today is
\ nothing: no quotation of the tree declares locals.
: QLOCALS-CK ( -- )
   LG-N @ 0 ?do
      i LG-B@ QINSIDE? if i LG-B@ QUOT-REFUSE then
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
\
\ THE TOKEN'S KEY IS PRESENTED RATHER THAN DERIVED, for the reason
\ src/compiler/native/hir-word.f ADMIT-TOKEN gives: deriving it needs the module's
\ interner, and the caller is the one that holds it. It is HIR-WORD:KEY-SYM of the
\ symbol the tape recorded for row `ix`, which is what WSYM answers for a token of
\ the definition this pass is elaborating.
: SPLICEABLE? ( IR-ARENA:view IR-ARENA:arena n IR-ID:ir-symbol-id -- bool )
   {: v:IR-ARENA:view r:IR-ARENA:arena ix:n sy:IR-ID:ir-symbol-id :}
   v ix NTAPE:KIND@ {: kd:NTAPE:kind :}
   kd NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if true exit then
   kd NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   r sy HIR-WORD:MODELS? 0= if false exit then
   r sy HIR-WORD:MEANING@ SPLICE-MEANING? ;

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
   r  ix WSYM  HIR-WORD:ENTRY@ ;

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
   ix WSYM {: sy:IR-ID:ir-symbol-id :}
   r sy HIR-WORD:MEANING@ HIR-MEANING:CALLABLE HIR-MEANING:EQ 0= if false exit then
   r sy HIR-WORD:CALLEE-IN@ IN-N @ <> if false exit then
   r sy HIR-WORD:CALLEE-OUT@ OUT-N @ = ;

\ A call control really does come back from. It is WORD-CALL? with the callee's
\ own control effect asked as well, because a call to a word that never returns
\ leaves this routine nothing to come back TO: no return address of ours is read
\ again on that path, and no value of ours is either. So a body whose only calls
\ are dead ones needs no frame and saves no link register - the same sentence
\ the tail decision below makes about the one call a body leaves through, and
\ the same one src/compiler/native/publish.f makes about a branch to a routine
\ that ends the process.
: BACK-CALL? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena ix:n :}
   ix MOPERAND? if false exit then
   r ix WORD-CALL? 0= if false exit then
   r ix HIR-MEANING:CALLABLE MODELED-AS? 0= if true exit then
   r  ix WSYM  HIR-WORD:CALLEE-DEAD? 0= ;

: BACK-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 CALL-BACK !
   n 1 ?do
      r i BACK-CALL? if 1 CALL-BACK ! leave then
   loop ;

: TAIL-SCAN ( IR-ARENA:arena n -- )
   {: r:IR-ARENA:arena n:n :}
   0 TAIL-NEED !
   0 TAIL-ENTRY !
   r n BACK-SCAN
   IN-N @ OUT-N @ <> if exit then
   IN-N @ 0= if exit then
   EXIT-USED @ 0<> if exit then
   NB @ 0<> if exit then
   n 2 < if exit then
   r n 1- WORD-CALL? 0= if exit then
   r n 1- TAIL-CALLEE? 0= if exit then
   1 TAIL-NEED !
   r  n 1- WSYM  HIR-WORD:ENTRY@ TAIL-ENTRY !
   0 CALL-BACK !
   n 1- 1 ?do
      r i BACK-CALL? if 1 CALL-BACK ! leave then
   loop ;

\ Elaborate the one colon definition this sealed tape holds, and answer the
\ function it became. The arenas are, in order, the tape's sealed view, the word
\ model's pick pool and the word model's rows; the two counts are the values the
\ word takes and the values it leaves. Every identity read off the tape is
\ checked against this builder's module by the table that owns it, so a tape of
\ another module cannot be elaborated into this one.
\ WHICH OF THE DEFINITION'S OWN ARGUMENTS AND RESULTS ARE CELLS OF A MULTI-CELL
\ VALUE, stated at the same seam and by the same caller that states how many
\ there are, and for the same reason: the fact belongs to the checker's accepted
\ effect, and until a recorded unit carries that effect (dot
\ habu-bind-checker-env-ed4f9f87) a name lookup here would answer about whatever
\ word carries the name now rather than about the tape being compiled.
\
\ IT IS SEPARATE FROM COLON'S ARGUMENTS RATHER THAN TWO MORE OF THEM so that a
\ caller which knows nothing about bundles keeps compiling exactly what it
\ compiled before: unstated reads as nothing bundled, which is the truth for
\ every row whose values are one cell each, and that is all but a handful.
\ COLON consumes the statement and clears it, so one definition's answer can
\ never be read by the next.
: FRAME-GLUE! ( n n -- )
   {: gin:n gout:n :}
   gin FR-GIN !
   gout FR-GOUT ! ;

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
   FR-GIN @ IN-GLUE !  FR-GOUT @ OUT-GLUE !
   0 FR-GIN !  0 FR-GOUT !
   b IR-BUILD:FUNS QBASE !
   r n QUOT-SCAN
   r n LOCALS-SCAN
   QLOCALS-CK
   r n MATCH-SCAN
   r n DEFER-SCAN
   r n RESOLVE-SCAN
   r n INLINE-SCAN
   r n MEM-SCAN
   r n CROSS-SCAN
   r 1 n SKELETON-TRY
   c b v key in out OPEN-FUN
   c b v key in OPEN-BLOCK
   TOK-NEED @ 0<> if 0 EMIT-MEM then
   PATH-LIVE PATH-END !
   p r 1 n WALK-TRY
   CS-N @ 0<> if E-NELAB-CTRL throw then
   PATH-END @ PATH-EXIT = if E-NELAB-CTRL throw then
   \ A BODY WHOSE LAST PATH ENDED HAS NO FALL-THROUGH AND MAY HAVE NO RETURN AT
   \ ALL. The block it was in was closed where the path ended, so nothing here
   \ branches to the return block and nothing here closes a block. Whether the
   \ ROUTINE still has a return depends on the `exit`s: one of them is an edge
   \ into the return block, so that block still has to be opened and the return
   \ still staged in it. A body with no `exit` and no live last path leaves
   \ through its trap and has no return convention at all, which is the shape
   \ src/compiler/native/regalloc.f and src/compiler/native/emit.f already know
   \ as NO-RET.
   PATH-DEAD? {: dead:bool :}
   EXIT-USED @ 0<> if
      dead 0= if
         VN @ out <> if E-NELAB-ARITY throw then
         0 EXIT-ORD @ 0 0 0 TERM-BR-H
      then
      NB @ EXIT-ORD @ <> if E-NELAB-CTRL throw then
      0 out 0 0 0 OPEN-ARGS-H
   then
   dead  EXIT-USED @ 0=  and 0= if
      out QRET-FILL
      c b v key out EMIT-RETURN
      c b IR-BUILD:END-BLOCK drop
   then
   r n TAIL-SCAN
   QCONSUMED-CK
   c b IR-BUILD:END-FUN {: f:IR-ID:ir-fun-id :}
   c b v key p r QBUILD-ALL
   f ;

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
