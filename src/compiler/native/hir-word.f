\ hir-word.f - the straight-line HIR dialect's source-word model: what a Habu
\ word means to the elaborator, and which words it refuses to compile at all.
\
\ docs/compiler-ir-design.md section 7.2 with section 7.3 line 758. This is the
\ second half of the dialect that src/compiler/native/hir.f opens:
\ src/compiler/native/hir.f says which operations exist, and this file says
\ which Habu source words the dialect can compile and what each one means. The
\ two are separate packages because each seals its own wordlists, and the
\ dependency runs one way: HIR-WORD names HIR's opcodes and HIR knows nothing
\ about source words.
\
\ THREE MEANINGS AND A REFUSAL. A word of the straight-line subset means exactly
\ one of these:
\   op        it elaborates to one operation of this dialect;
\   rename    it only rearranges the compile-time value vector and produces no
\             operation at all;
\   unmodeled a named boundary: checked source may not compile it yet, and the
\             row says which capability has to land first.
\ A fourth meaning, `literal`, belongs to a source-tape token rather than to a
\ word, so no row ever stores it - an integer literal is not a call, and the
\ tape's own token kind is what says it is a literal. A word this table never
\ declared is refused exactly as a declared unmodeled boundary is: to checked
\ source they are the same event, this dialect cannot compile that word.
\
\ WHY A RENAME IS DATA AND NOT FOUR SPECIAL CASES. Section 7.3 line 758 says
\ `DUP`, `DROP`, `SWAP` and `OVER` "produce no SIR operation and therefore no
\ runtime instruction": they change the compile-time value vector and nothing
\ else. A row therefore records that change as data - how many values the word
\ consumes off the top, and which of them it puts back, in order - so the
\ stack-to-SSA converter applies a rename by reading it rather than by carrying
\ its own copy of what `OVER` means. An input is named by its depth in the
\ consumed window, zero being the top:
\   dup  ( a -- a a )        consumes 1, puts back 0 0
\   drop ( a -- )            consumes 1, puts back nothing
\   swap ( a b -- b a )      consumes 2, puts back 0 1
\   over ( a b -- a b a )    consumes 2, puts back 1 0 1
\ Picks are listed bottom first, which is the order they are pushed. A rename
\ may repeat an input, as `DUP` and `OVER` do, and may drop one, as `DROP` does;
\ the one rule is that it can only put back an input it consumed.
\
\ THE OPCODE A WORD MEANS IS A TYPE, NOT A LOOKUP. A row stores the stable code
\ of a `HIR:opcode`, so binding a word to an operation this dialect does not
\ have is not a runtime check that can be forgotten - it is unwritable, and a
\ stored code outside the five is refused by the decoder at first touch.
\
\ WHAT A DECLARATION CHECKS, AND WHAT IT CANNOT. Every symbol a row holds is
\ checked to belong to this table's module, no word is declared twice, and both
\ ceilings are committed at creation. What is not checked is that a presented
\ symbol was really interned: IR-BUILD owns its module's symbol rows privately
\ and hands out no live reader for them, so this table cannot ask the interner
\ the way src/compiler/native/immediate.f can. It does not need to. A symbol id
\ can only be forged through IR-ID:PACK-SYMBOL with this module's key, and a
\ forged ordinal either matches no interned symbol - in which case no source
\ token can ever spell it and the row is inert - or matches one, in which case
\ the row is exactly the row an honest declaration of that symbol would have
\ written. A forgery can therefore only do what a caller could already do
\ honestly. Dot habu-expose-live-ir-f0eaed6b tracks the live IR-BUILD
\ readers that would make the check direct anyway.
\
\ SPELLINGS ARE BYTES, AND THE LEXER OWNS THEIR CASE. REGISTER-WORDS interns the
\ seven words of the subset exactly as `docs/forth.md` spells them: built-ins
\ stay lower case. Symbol interning is byte equality, so a producer that hands
\ the tape `DUP` where this table declared `dup` will find no row and be refused
\ by name. Which spelling the real lexer records is the tape producer's fact and
\ is tracked by dot habu-feed-the-src-f7ed8733; nothing here guesses at it.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/build.f
require src/compiler/native/tape.f
require src/compiler/native/hir.f

package HIR-WORD
private

CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n ) ;

\ ---- layout ------------------------------------------------------------------
$48575031 constant WPOOL-MAGIC       \ "HWP1": the pick-pool header format tag
$48575231 constant WROW-MAGIC        \ "HWR1": the word-table header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-SYM                   \ the source word's symbol ordinal
1 constant OFF-MEAN                  \ the stored meaning code
2 constant OFF-A                     \ op: the opcode code; rename: the pick-list start; unmodeled: the reason ordinal plus one
3 constant OFF-IN                    \ rename: the number of values consumed; otherwise zero
4 constant OFF-N                     \ rename: the number of values put back; otherwise zero
5 constant ROW-CELLS
0 constant UNUSED                    \ a payload cell this meaning does not use
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant ROW-CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CAP-MAX

\ The deepest classical Forth stack rename is `2over` ( a b c d -- a b c d a b ):
\ it consumes four values and puts back six. These ceilings hold that shape with
\ headroom and keep the staging buffer a fixed array; a rename that wants more
\ is a capability to add here, not a value to widen silently.
4 constant INPUT-MAX
8 constant PICK-MAX

\ ---- stored codes ------------------------------------------------------------
\ The stored codes are this table's stable vocabulary. Both decoders are exact
\ cases, so a row written past this package's declarers cannot decode as some
\ other meaning or some other operation.
: MEAN-CODE ( HIR:meaning -- n )
   MATCH HIR:meaning
      literal   OF 0 ENDOF
      op        OF 1 ENDOF
      rename    OF 2 ENDOF
      unmodeled OF 3 ENDOF
   ;MATCH ;

\ Code zero, `literal`, is deliberately absent: a literal is a token's meaning,
\ so a row that claims it is corrupt rather than unusual.
: N>MEAN ( n -- HIR:meaning )
   case
      1 of HIR-MEANING:OP endof
      2 of HIR-MEANING:RENAME endof
      3 of HIR-MEANING:UNMODELED endof
      E-HIR-CLASS throw
   endcase ;

: OPCODE-CODE ( HIR:opcode -- n )
   MATCH HIR:opcode
      const  OF 0 ENDOF
      add    OF 1 ENDOF
      sub    OF 2 ENDOF
      mul    OF 3 ENDOF
      return OF 4 ENDOF
   ;MATCH ;

: N>OPCODE ( n -- HIR:opcode )
   case
      0 of HIR-OPCODE:CONST endof
      1 of HIR-OPCODE:ADD endof
      2 of HIR-OPCODE:SUB endof
      3 of HIR-OPCODE:MUL endof
      4 of HIR-OPCODE:RETURN endof
      E-HIR-OPCODE throw
   endcase ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

\ ---- headers and shape -------------------------------------------------------
: RSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-HIR-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-HIR-STATE throw then ;

: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-HIR-STATE throw then ;

\ The two arenas of this table and the module's other arenas all have the same
\ checked type, so each one rechecks its own header tag: a pair swapped at a
\ call site dies on the tag instead of reading a foreign row.
: RHDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED RSHAPE-CK
   a HC-MAGIC LCELL@ WROW-MAGIC <> if E-HIR-STATE throw then ;

: PHDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED PSHAPE-CK
   a HC-MAGIC LCELL@ WPOOL-MAGIC <> if E-HIR-STATE throw then ;

: CNT ( IR-ARENA:arena -- n )
   IR-ARENA:USED HDR-CELLS - ROW-CELLS / ;

: PCELLS ( IR-ARENA:arena -- n )
   IR-ARENA:USED HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-HIR-OWNER throw then ;

: KEY-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   r RHDR-CK
   r HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

\ Both arenas must be halves of one table, which is one more thing a swapped
\ pair cannot survive.
: PAIR-CK ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   p PHDR-CK
   r RHDR-CK
   p HC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK ;

\ A declared symbol carries its owning module, so no presented key is needed to
\ bind a row to this table's module.
: SYM-OWNER-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r RHDR-CK
   r HC-SERIAL LCELL@ id IR-ID:SYMBOL-OWNER MID-SERIAL SERIAL-CK ;

\ ---- row addressing ----------------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

: PC@ ( IR-ARENA:arena n -- n )
   HDR-CELLS + LCELL@ ;

\ The row that models this symbol, or a negative answer. One scan serves the
\ lookup, the duplicate check, and the inventory walk.
: FIND ( IR-ARENA:arena n -- n )
   {: r:IR-ARENA:arena so:n :}
   -1
   r CNT 0 ?do
      r i OFF-SYM RC@ so = if drop i leave then
   loop ;

: ROW-CAP-OK ( n -- )
   dup 1 < over ROW-CAP-MAX > or if E-HIR-CAP throw then
   drop ;

: POOL-CAP-OK ( n -- )
   dup 0 < over POOL-CAP-MAX > or if E-HIR-CAP throw then
   drop ;

: ROW-ROOM-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r CNT r HC-CAP LCELL@ >= if E-HIR-CAP throw then ;

: POOL-ROOM-CK ( IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena want:n :}
   p PCELLS want + p HC-CAP LCELL@ > if E-HIR-CAP throw then ;

public

\ ---- creation ----------------------------------------------------------------
\ Create a module's word model: the pick pool committed to exactly pcap cells
\ and the row table to exactly rcap words, both headers bound to key's module
\ serial. The two handles plus the key are the table, and it dies with the
\ owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key rcap:n pcap:n :}
   rcap ROW-CAP-OK
   pcap POOL-CAP-OK
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p WPOOL-MAGIC IR-ARENA:PUSH drop
   c p key KEY-SERIAL IR-ARENA:PUSH drop
   c p pcap IR-ARENA:PUSH drop
   c rcap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r WROW-MAGIC IR-ARENA:PUSH drop
   c r key KEY-SERIAL IR-ARENA:PUSH drop
   c r rcap IR-ARENA:PUSH drop
   p r ;

private

\ Append one validated row. Every declarer ends here, so the ownership, the
\ duplicate rule and the ceiling are proved in one place.
: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-symbol-id n n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena id:IR-ID:ir-symbol-id
      mean:n a:n in:n n:n :}
   r id SYM-OWNER-CK
   id IR-ID:SYMBOL-LOCAL {: so:n :}
   r so FIND 0 < 0= if E-HIR-DUP throw then
   r ROW-ROOM-CK
   c r so IR-ARENA:PUSH drop
   c r mean IR-ARENA:PUSH drop
   c r a IR-ARENA:PUSH drop
   c r in IR-ARENA:PUSH drop
   c r n IR-ARENA:PUSH drop ;

public

\ Declare that a source word elaborates to one operation of this dialect.
: DECLARE-OP ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode -- )
   {: o:HIR:opcode :}
   HIR-MEANING:OP MEAN-CODE
   o OPCODE-CODE
   UNUSED UNUSED
   ROW-ADD ;

\ Declare a named boundary this dialect cannot compile. The reason symbol names
\ the capability whose absence is why, so a refusal can say what has to land
\ before the boundary can be retired.
: DECLARE-UNMODELED ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena id:IR-ID:ir-symbol-id why:IR-ID:ir-symbol-id :}
   r why SYM-OWNER-CK
   c r id
   HIR-MEANING:UNMODELED MEAN-CODE
   why IR-ID:SYMBOL-LOCAL 1+
   UNUSED UNUSED
   ROW-ADD ;

private

\ ---- the staged rename -------------------------------------------------------
\ One package-owned stage under the single-task compilation discipline, the same
\ protocol IR-TYPE, IR-ATTR and IR-SCHEMA use: a begin opens it, the picks fill
\ it, and the end validates and appends. Any end consumes the stage whatever its
\ outcome, so no half-staged rename survives into the next declaration.
0 constant MODE-NONE
1 constant MODE-OPEN

here CELL 1- and CELL swap - CELL 1- and allot
variable STG-MODE
MODE-NONE STG-MODE !
variable STG-IN
variable STG-N
create STG-PICK PICK-MAX cells allot

: STG-OPEN-CK ( -- )
   STG-MODE @ MODE-OPEN <> if E-HIR-STAGE throw then ;

: STG-TAKE ( -- )
   STG-MODE @ {: have:n :}
   MODE-NONE STG-MODE !
   have MODE-OPEN <> if E-HIR-STAGE throw then ;

: SP@ ( n -- n )
   cells STG-PICK + @ ;

: SP! ( n n -- )
   cells STG-PICK + ! ;

public

\ Open a rename that consumes `in` values off the top of the compile-time value
\ vector.
: BEGIN-RENAME ( n -- )
   {: in:n :}
   STG-MODE @ MODE-NONE <> if E-HIR-STAGE throw then
   in 0 < in INPUT-MAX > or if E-HIR-PICK throw then
   MODE-OPEN STG-MODE !
   in STG-IN !
   0 STG-N ! ;

\ Put one of the consumed values back, named by its depth in the consumed
\ window with zero being the top. Picks are listed bottom first.
: ADD-PICK ( n -- )
   {: d:n :}
   STG-OPEN-CK
   d 0 < d STG-IN @ >= or if E-HIR-PICK throw then
   STG-N @ {: n:n :}
   n PICK-MAX >= if E-HIR-PICK throw then
   d n SP!
   n 1+ STG-N ! ;

\ Abandon an open rename without declaring it.
: ABANDON-RENAME ( -- )
   STG-TAKE ;

\ Close the staged rename and bind it to a source word. The picks land in the
\ pool before the row that points at them, so a refused declaration leaves the
\ table without a row that names cells outside it.
: DECLARE-RENAME ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   STG-TAKE
   p r PAIR-CK
   r id SYM-OWNER-CK
   id IR-ID:SYMBOL-LOCAL {: so:n :}
   r so FIND 0 < 0= if E-HIR-DUP throw then
   r ROW-ROOM-CK
   p STG-N @ POOL-ROOM-CK
   p PCELLS {: st:n :}
   STG-N @ 0 ?do
      c p i SP@ IR-ARENA:PUSH drop
   loop
   c r id
   HIR-MEANING:RENAME MEAN-CODE
   st STG-IN @ STG-N @
   ROW-ADD ;

\ ---- reading -----------------------------------------------------------------
: MODELED ( IR-ARENA:arena -- n )
   dup RHDR-CK CNT ;

private

: ROW-OF ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id SYM-OWNER-CK
   r id IR-ID:SYMBOL-LOCAL FIND
   dup 0 < if E-HIR-UNMODELED throw then ;

\ The row of a word this table models with the meaning the caller is about to
\ read. Asking a rename for its opcode, or an op for its picks, is a category
\ error rather than a missing value.
: ROW-AS ( IR-ARENA:arena IR-ID:ir-symbol-id HIR:meaning -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id want:HIR:meaning :}
   r id ROW-OF {: l:n :}
   r l OFF-MEAN RC@ N>MEAN want HIR-MEANING:EQ
   0= if E-HIR-CLASS throw then
   l ;

public

\ What this table says a word means. A word it never declared has no meaning at
\ all, which is the same refusal a declared boundary gets.
: MEANING@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:meaning )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id ROW-OF {: l:n :}
   r l OFF-MEAN RC@ N>MEAN ;

\ The gate. Answers what checked source may compile this word into, and refuses
\ everything else by name.
: ADMIT ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:meaning )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:UNMODELED HIR-MEANING:EQ if E-HIR-UNMODELED throw then
   m ;

: OPCODE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:opcode )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:OP ROW-AS {: l:n :}
   r l OFF-A RC@ N>OPCODE ;

\ How many values a rename consumes off the top of the value vector.
: INPUTS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RENAME ROW-AS {: l:n :}
   r l OFF-IN RC@ ;

\ How many values it puts back.
: PICKS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RENAME ROW-AS {: l:n :}
   r l OFF-N RC@ ;

\ The i-th value it puts back, named by its depth in the consumed window.
\ Bottom first. Both the window into the pool and the depth are rechecked, so a
\ row written past this package's declarers cannot read a cell outside the live
\ pool or name a value the rename never consumed.
: PICK@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: p:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id i:n :}
   p r PAIR-CK
   r id HIR-MEANING:RENAME ROW-AS {: l:n :}
   r l OFF-N RC@ {: n:n :}
   i 0 < i n >= or if E-HIR-BOUND throw then
   r l OFF-A RC@ {: st:n :}
   st 0 < st n + p PCELLS > or if E-HIR-STATE throw then
   p st i + PC@ {: d:n :}
   d 0 < d r l OFF-IN RC@ >= or if E-HIR-PICK throw then
   d ;

\ The capability a boundary is waiting for. Only an unmodeled entry names one.
: REASON@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-symbol-id :}
   r key KEY-CK
   r id HIR-MEANING:UNMODELED ROW-AS {: l:n :}
   r l OFF-A RC@
   dup UNUSED = if E-HIR-STATE throw then
   1- key swap IR-ID:PACK-SYMBOL ;

\ The i-th declared word, in declaration order. This is what an inventory of the
\ remaining boundaries walks.
: AT ( IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key i:n :}
   r key KEY-CK
   i 0 < if E-HIR-BOUND throw then
   i r CNT >= if E-HIR-BOUND throw then
   key r i OFF-SYM RC@ IR-ID:PACK-SYMBOL ;

\ ---- the tape join -----------------------------------------------------------
\ The elaborator walks a sealed source tape and asks, token by token, what this
\ dialect makes of it. An integer literal is a literal whatever any table says,
\ because its kind is what makes it one. A name is looked up by its spelling. A
\ character or string literal is a kind the straight-line subset does not model
\ at all, and is refused as such rather than resolved as a name.
: ADMIT-TOKEN ( IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena n -- HIR:meaning )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key r:IR-ARENA:arena i:n :}
   v i NTAPE:KIND@ {: k:NTAPE:kind :}
   k NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if HIR-MEANING:LITERAL exit then
   k NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-HIR-KIND throw then
   r v key i NTAPE:SPELL@ ADMIT ;

\ ---- the subset's vocabulary -------------------------------------------------
\ The seven words the straight-line subset models, and the exact ceilings they
\ need, so a caller commits a table to what this registration writes and not to
\ a guess.
7 constant WORDS
7 constant PICK-CELLS

private

\ The three words this dialect has operations for.
: DEF-ARITH ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c r c b s" +" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:ADD DECLARE-OP
   c r c b s" -" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:SUB DECLARE-OP
   c r c b s" *" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:MUL DECLARE-OP ;

\ dup ( a -- a a ): consume the top value and put it back twice.
: DEF-DUP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   1 BEGIN-RENAME
   0 ADD-PICK
   0 ADD-PICK
   c p r c b s" dup" IR-BUILD:INTERN-SYMBOL DECLARE-RENAME ;

\ drop ( a -- ): consume the top value and put nothing back.
: DEF-DROP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   1 BEGIN-RENAME
   c p r c b s" drop" IR-BUILD:INTERN-SYMBOL DECLARE-RENAME ;

\ swap ( a b -- b a ): consume two and put them back the other way round.
: DEF-SWAP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   0 ADD-PICK
   1 ADD-PICK
   c p r c b s" swap" IR-BUILD:INTERN-SYMBOL DECLARE-RENAME ;

\ over ( a b -- a b a ): consume two and put back three, the lower one twice.
: DEF-OVER ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   1 ADD-PICK
   c p r c b s" over" IR-BUILD:INTERN-SYMBOL DECLARE-RENAME ;

public

\ Declare the whole straight-line source vocabulary into one word model: the
\ three arithmetic words this dialect has operations for, and the four stack
\ words that only rename values. The builder is the module's symbol interner,
\ so the spellings become identities of the same module the table is bound to.
: REGISTER-WORDS ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b r DEF-ARITH
   c b p r DEF-DUP
   c b p r DEF-DROP
   c b p r DEF-SWAP
   c b p r DEF-OVER ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
