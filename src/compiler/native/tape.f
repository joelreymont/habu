\ tape.f - the native stage N0 source tape: the exact token stream the
\ compiler consumed, captured once, sealed, and digested.
\
\ docs/compiler-ir-design.md section 7.1 ("Stage N0: source tape") and Wave 2's
\ pipeline line `source tape -> HIR -> SIR -> LIR -> A64IR -> allocation ->
\ bytes`. The tape exists for one reason: so that checking, elaboration,
\ diagnostics, and code generation can prove they are talking about the same
\ tokens. It is not a syntax tree. It has no opinion about what a name means,
\ it never resolves a word, and it never touches AArch64.
\
\ WHAT A TOKEN RECORDS. Section 7.1 names six fields, and a row here is exactly
\ those six: the token kind, the byte span it was read from, the resolved
\ spelling, the literal value where the kind has one, the parser mode in force
\ when it was consumed, and its origin. Nothing else is stored, so nothing else
\ can drift.
\
\ THE SPELLING IS AN INTERNED SYMBOL, NOT A SECOND SLICE. Section 7.1 calls it a
\ "resolved spelling slice". The byte span already says where the token was read
\ from; a second byte range would say the same thing twice and would still leave
\ the reader to re-lex the bytes to learn the name. An IR-SYM symbol id is the
\ resolved spelling: it is module-owned, it deduplicates equal spellings, and
\ the elaborator can compare two names without looking at any bytes. A string
\ literal's spelling is its body, which is precisely the slice a raw byte span
\ could not distinguish from the quoting syntax around it.
\
\ ORIGIN IS THE EXPANSION PARENT TOKEN. A directly lexed token has no origin. A
\ token produced by expanding an earlier token records that earlier token's
\ ordinal, so a diagnostic can walk back to the token the programmer wrote.
\ IR-SOURCE already records the include/expansion parent of a whole SOURCE; this
\ is the same relation one level down, between tokens. It is acyclic by
\ construction for the same reason IR-SOURCE's is: a parent must already be
\ appended, so its ordinal is strictly below its child's, and any cycle needs
\ some member to name a token that does not exist yet. Every walk re-verifies
\ the strict decrease, so a walk terminates on any tape state.
\
\ A TOKEN ORDINAL IS A TAPE-LOCAL INDEX. PUSH answers the ordinal it appended
\ at, and every word that takes one bound-checks it against the count the tape
\ records. There is no sealed token-id family: design section 6.1's identity
\ families are the IR substrate's, and the tape sits below the IR. An ordinal
\ from one tape presented to another is caught by the bound check whenever the
\ two counts differ, and by the module-key check whenever the modules differ.
\
\ BUILD LIVE, READ FROZEN. Design rule 5.1 makes a pass input immutable. The
\ tape is written once by the lexer and then published, so the only live
\ operations are NEW, the four appends and PUSHED; every reader, the structural
\ CHECK, and the digest take the frozen view SEAL answers. That is why there is
\ no live/frozen pair of every reader: a tape that could still grow has no
\ digest worth sharing.
\
\ THE DIGEST IS THE THING THE THREE STAGES SHARE. DIGEST folds the whole sealed
\ tape into one value: a per-token record digest, chained, exactly the shape
\ IR-SCHEMA uses for its table digest, so no buffer grows with the tape. Each
\ stage records the digest of the tape it read and hands it back through VERIFY;
\ a tape that is not bit-for-bit the one that stage read cannot pass. The
\ digest deliberately excludes the module serial, which is allocated per
\ process: two structurally identical tapes must digest identically or a cached
\ result could never be reused across runs.
\
\ WHAT THE DIGEST DOES NOT COVER. The digest is over the cells the tape itself
\ owns - kinds, modes, ordinals, spans, literals, origins. It does not reach
\ into the source registry for the bytes behind a span, or into the symbol store
\ for the bytes behind a spelling. Those tables carry their own content digests
\ (IR-SOURCE:DIGEST@ per source), and a stage that needs content identity binds
\ both. Reading inside another table's rows to digest them here would give this
\ file an opinion about their row shape, an authority that stays with the table
\ that owns the rows.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f

package NTAPE
public

\ The token vocabulary. Closed: a lexer that meets something else has met a
\ construct this stage has not been taught, and that is a capability to add
\ here, not a value to smuggle through as an integer.
\ `name` is a name to be resolved later - the tape never resolves it.
\ `int-literal` carries its value, `char-literal` its code point, and
\ `string-literal`'s spelling is the body, not the quoting syntax around it.
\ `real-literal` carries the CELL the double is. A double on a Habu stack is one
\ unboxed cell holding its own IEEE754 bit pattern, so the value a real literal
\ carries is a bit pattern and not a second representation of it - which is why
\ it rides in the same literal field the integer kinds use rather than in a field
\ of its own, and why its rule below is the signed one: the sign bit of a
\ negative double is the sign bit of that cell.
ENUM kind DERIVE eq
   name
   int-literal
   char-literal
   string-literal
   real-literal
;ENUM

\ The parser mode in force when the token was consumed. Forth has exactly two,
\ and which one applied is a fact about the token, not about the tape.
ENUM mode DERIVE eq
   interpreting
   compiling
;ENUM

\ One token, as a value. A value is not authority: the generated constructor is
\ open, so PUSH revalidates every field against the module's registries before a
\ row exists. The four minting words below are what checked callers use, and
\ each one enforces its kind's literal rule at the point of construction.
\ The byte span is the last field, so UNMAKE leaves it on top and a caller can
\ take it apart in one more step. The checker cannot yet bind a local of a
\ multi-cell structure type (dot habu-bind-multi-cell-d2e153ed), so every word
\ here that receives a token or a span unmakes it at entry, exactly as
\ src/compiler/ir/source.f does with its spans.
STRUCTURE token 0
   FIELD kind kind
   FIELD mode mode
   FIELD spell IR-ID:ir-symbol-id
   FIELD lit n
   FIELD span IR-SOURCE:span
;STRUCTURE

private

\ The one raw crossing this package needs: a one-way projection of the sealed
\ module key onto its serial, for header binding. Nothing here re-mints a raw
\ cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n ) ;

\ ---- layout ------------------------------------------------------------------
$4E545031 constant MAGIC        \ "NTP1": the tape header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-KIND
1 constant OFF-MODE
2 constant OFF-SRC                   \ the span's source ordinal
3 constant OFF-ST                    \ the span's start byte
4 constant OFF-LN                    \ the span's byte length
5 constant OFF-SYM                   \ the spelling's symbol ordinal
6 constant OFF-LIT                   \ the literal value, or zero where the kind has none
7 constant OFF-ORG                   \ the expansion parent's ordinal plus one
8 constant ROW-CELLS
0 constant ORG-NONE                  \ origin cell of a directly lexed token
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX

\ ---- stored codes ------------------------------------------------------------
\ The stored codes are this stage's stable vocabulary: they are part of every
\ token preimage, so they never change value once a digest has been published.
: KIND-CODE ( NTAPE:kind -- n )
   MATCH kind
      name           OF 0 ENDOF
      int-literal    OF 1 ENDOF
      char-literal   OF 2 ENDOF
      string-literal OF 3 ENDOF
      real-literal   OF 4 ENDOF
   ;MATCH ;

: N>KIND ( n -- NTAPE:kind )
   case
      0 of NTAPE-KIND:NAME endof
      1 of NTAPE-KIND:INT-LITERAL endof
      2 of NTAPE-KIND:CHAR-LITERAL endof
      3 of NTAPE-KIND:STRING-LITERAL endof
      4 of NTAPE-KIND:REAL-LITERAL endof
      E-NTAPE-KIND throw
   endcase ;

: MODE-CODE ( NTAPE:mode -- n )
   MATCH mode
      interpreting OF 0 ENDOF
      compiling    OF 1 ENDOF
   ;MATCH ;

: N>MODE ( n -- NTAPE:mode )
   case
      0 of NTAPE-MODE:INTERPRETING endof
      1 of NTAPE-MODE:COMPILING endof
      E-NTAPE-MODE throw
   endcase ;

\ ---- the per-kind literal rule -----------------------------------------------
\ Which kinds carry a literal value is a property of the kind, so the tape
\ stores no separate "has a literal" flag: there is no second piece of state to
\ contradict the first. A kind without a literal stores exactly zero, and a
\ character literal is a code point, so it is never negative.
: LIT-KIND? ( NTAPE:kind -- bool )
   MATCH kind
      name           OF false ENDOF
      int-literal    OF true ENDOF
      char-literal   OF true ENDOF
      string-literal OF false ENDOF
      real-literal   OF true ENDOF
   ;MATCH ;

: SIGNED-KIND? ( NTAPE:kind -- bool )
   MATCH kind
      name           OF false ENDOF
      int-literal    OF true ENDOF
      char-literal   OF false ENDOF
      string-literal OF false ENDOF
      real-literal   OF true ENDOF
   ;MATCH ;

: LIT-CK ( NTAPE:kind n -- )
   {: k:NTAPE:kind v:n :}
   k LIT-KIND? 0= if
      v 0 <> if E-NTAPE-LITERAL throw then
      exit
   then
   k SIGNED-KIND? if exit then
   v 0 < if E-NTAPE-LITERAL throw then ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

: FCELL@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view k:n :}
   v v k IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

\ ---- header and shape --------------------------------------------------------
: SHAPE-CK ( n -- )
   dup HDR-CELLS < if E-NTAPE-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-NTAPE-STATE throw then ;

: MAGIC-CK ( n -- )
   MAGIC <> if E-NTAPE-STATE throw then ;

: HDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED SHAPE-CK
   a HC-MAGIC LCELL@ MAGIC-CK ;

: FHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE SHAPE-CK
   v HC-MAGIC FCELL@ MAGIC-CK ;

: USED>CNT ( n -- n )
   HDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:arena -- n )
   IR-ARENA:USED USED>CNT ;

: FCNT ( IR-ARENA:view -- n )
   IR-ARENA:SIZE USED>CNT ;

\ ---- ownership ---------------------------------------------------------------
\ Three arenas of the same type meet at the appending words: the tape, the
\ module's source registry, and the module's symbol rows. The checker cannot
\ tell them apart, so each one's own package rechecks its own header tag, and a
\ pair swapped at a call site dies on the tag rather than reading a foreign row.
: SERIAL-CK ( n n -- )
   <> if E-NTAPE-OWNER throw then ;

: KEY-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: a:IR-ARENA:arena key:IR-ID:ir-module-key :}
   a HDR-CK
   a HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

: FKEY-CK ( IR-ARENA:view IR-ID:ir-module-key -- )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key :}
   v FHDR-CK
   v HC-SERIAL FCELL@ key KEY-SERIAL SERIAL-CK ;

\ Appending needs no module key: the token's own identities carry their owning
\ module, so the tape is bound to them directly. That is a stronger check than
\ a presented key - a caller cannot supply the wrong one - and it is why PUSH
\ takes one argument fewer than IR-SOURCE:REGISTER, which has to MINT an
\ identity and therefore does need the key.
: SRC-OWNER-CK ( IR-ARENA:arena IR-ID:ir-source-id -- )
   {: a:IR-ARENA:arena id:IR-ID:ir-source-id :}
   a HDR-CK
   a HC-SERIAL LCELL@ id IR-ID:SOURCE-OWNER MID-SERIAL SERIAL-CK ;

: SYM-OWNER-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: a:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   a HC-SERIAL LCELL@ id IR-ID:SYMBOL-OWNER MID-SERIAL SERIAL-CK ;

\ ---- row addressing ----------------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

: FRC@ ( IR-ARENA:view n n -- n )
   ROW-CELL FCELL@ ;

: ORD-CK ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view i:n :}
   v FHDR-CK
   i 0 < if E-NTAPE-BOUND throw then
   i v FCNT >= if E-NTAPE-BOUND throw then
   i ;

\ ---- creation ----------------------------------------------------------------
: CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-NTAPE-CAP throw then
   drop ;

public

\ Create a module's source tape: an IR-ARENA arena owned by ctx, its cell
\ ceiling committed to exactly cap tokens, its header bound to key's module
\ serial. The arena handle plus the key is the tape; it dies with its context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key cap:n :}
   cap CAP-OK
   c cap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a MAGIC IR-ARENA:PUSH drop
   c a key KEY-SERIAL IR-ARENA:PUSH drop
   c a cap IR-ARENA:PUSH drop
   a ;

\ ---- minting tokens ----------------------------------------------------------
\ Five constructors, one per kind, so the literal rule is structural: there is
\ no way to ask for a name token that carries a value, and no way to ask for an
\ integer literal without one.
private

: MK ( IR-SOURCE:span NTAPE:kind NTAPE:mode IR-ID:ir-symbol-id n -- NTAPE:token )
   {: k:NTAPE:kind m:NTAPE:mode sy:IR-ID:ir-symbol-id v:n :}
   IR--SOURCE-SPAN:UNMAKE {: sid:IR-ID:ir-source-id st:n ln:n :}
   k v LIT-CK
   k m sy v sid st ln IR--SOURCE-SPAN:MAKE NTAPE-TOKEN:MAKE ;

public

: NAME-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode :}
   NTAPE-KIND:NAME m sy 0 MK ;

: STRING-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode :}
   NTAPE-KIND:STRING-LITERAL m sy 0 MK ;

: INT-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode n -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode v:n :}
   NTAPE-KIND:INT-LITERAL m sy v MK ;

: CHAR-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode n -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode v:n :}
   NTAPE-KIND:CHAR-LITERAL m sy v MK ;

\ A double, as the cell it is. The value is the literal's bit pattern, so this
\ constructor states no format and performs no conversion: whatever produced the
\ bits owns that question, and the tape records what it produced.
: REAL-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode n -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode v:n :}
   NTAPE-KIND:REAL-LITERAL m sy v MK ;

\ ---- appending ---------------------------------------------------------------
private

: ROOM-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a CNT a HC-CAP LCELL@ >= if E-NTAPE-CAP throw then ;

\ The stored origin cell is the expansion parent's ordinal plus one, so zero
\ means "directly lexed" without a sentinel inside the ordinal range. A parent
\ must already be a token of this tape, which is what makes the relation
\ acyclic: a self cycle or any multi-node cycle needs some member to name a
\ token that has not been appended yet, and that edge dies here.
: ORG-CK ( IR-ARENA:arena n -- )
   {: a:IR-ARENA:arena og:n :}
   og ORG-NONE = if exit then
   og 0 < if E-NTAPE-ORIGIN throw then
   og 1- a CNT >= if E-NTAPE-ORIGIN throw then ;

\ Revalidate a token that may have come from the open generated constructor.
\ These are the fields the tape alone can judge: both identities against the
\ module this tape is bound to, the origin against the tokens already appended,
\ and the literal against its kind's rule. It runs before the module's other
\ tables are consulted, so a tape handed an arena that is not a tape still dies
\ on its own header tag rather than inside another package's reader.
: FIELD-CK ( IR-ARENA:arena NTAPE:kind IR-ID:ir-symbol-id n IR-ID:ir-source-id n -- )
   {: a:IR-ARENA:arena k:NTAPE:kind id:IR-ID:ir-symbol-id v:n
      sid:IR-ID:ir-source-id og:n :}
   a sid SRC-OWNER-CK
   a id SYM-OWNER-CK
   a og ORG-CK
   k v LIT-CK ;

\ Write the row. The only word here that appends a cell, and it is reached only
\ through the two fronts below, each of which has proved the whole token first:
\ the fields above, the span against the source registry that owns byte ranges,
\ and the spelling against the symbol store that owns names.
: WRITE ( IR-CTX:ctx IR-ARENA:arena NTAPE:kind NTAPE:mode IR-ID:ir-symbol-id n IR-ID:ir-source-id n n n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena
      k:NTAPE:kind m:NTAPE:mode id:IR-ID:ir-symbol-id v:n
      sid:IR-ID:ir-source-id st:n ln:n og:n :}
   a ROOM-CK
   a CNT {: i:n :}
   c a k KIND-CODE IR-ARENA:PUSH drop
   c a m MODE-CODE IR-ARENA:PUSH drop
   c a sid IR-ID:SOURCE-LOCAL IR-ARENA:PUSH drop
   c a st IR-ARENA:PUSH drop
   c a ln IR-ARENA:PUSH drop
   c a id IR-ID:SYMBOL-LOCAL IR-ARENA:PUSH drop
   c a v IR-ARENA:PUSH drop
   c a og IR-ARENA:PUSH drop
   i ;

\ ---- the two ways to reach the module a token belongs to ---------------------
\ A token names a source and a spelling, and both have to be checked against the
\ tables that own them. A tape of a module whose tables the caller holds hands
\ those two tables over directly. A tape of a module still being built through
\ src/compiler/ir/build.f cannot: that package holds its tables privately so it
\ stays the module's only mutation route, and it answers the same two questions
\ through its live readers instead. The two fronts differ in nothing else - each
\ checks the token's own fields, then the span, then the spelling, then appends
\ through the one WRITE above - which is why a tape and the IR module it was
\ lexed into can now be two halves of one module rather than two modules that
\ merely agree.
: TABLE-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena NTAPE:kind NTAPE:mode IR-ID:ir-symbol-id n IR-ID:ir-source-id n n n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena sr:IR-ARENA:arena sy:IR-ARENA:arena
      k:NTAPE:kind m:NTAPE:mode id:IR-ID:ir-symbol-id v:n
      sid:IR-ID:ir-source-id st:n ln:n og:n :}
   a k id v sid og FIELD-CK
   sr sid st ln IR--SOURCE-SPAN:MAKE IR-SOURCE:SPAN-CK
   sy id IR-SYM:LEN@ drop
   c a k m id v sid st ln og WRITE ;

: LIVE-ADD ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena NTAPE:kind NTAPE:mode IR-ID:ir-symbol-id n IR-ID:ir-source-id n n n -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ARENA:arena
      k:NTAPE:kind m:NTAPE:mode id:IR-ID:ir-symbol-id v:n
      sid:IR-ID:ir-source-id st:n ln:n og:n :}
   a k id v sid og FIELD-CK
   c b  sid st ln IR--SOURCE-SPAN:MAKE  IR-BUILD:SPAN-CK
   c b id IR-BUILD:SYMBOL-CK
   c a k m id v sid st ln og WRITE ;

public

\ Append a directly lexed token and answer its tape-local ordinal. The three
\ arenas are, in order, the tape, the module's source registry, and the
\ module's symbol rows; each rechecks its own header tag, so a pair swapped at
\ the call site dies on the tag instead of reading a foreign row.
: PUSH ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena NTAPE:token -- n )
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE ORG-NONE TABLE-ADD ;

\ Append a token produced by expanding an already appended token. The parent
\ ordinal rides on top so the token beneath it can be taken apart in place.
: PUSH-FROM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena NTAPE:token n -- n )
   {: parent:n :}
   parent 0 < if E-NTAPE-ORIGIN throw then
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE parent 1+ TABLE-ADD ;

\ The same two appends for a tape of a module that is still being built: the
\ builder answers for the module's source registry and symbol interner, and
\ refuses a foreign context, a frozen builder and an aborted one by their own
\ names before this tape is touched.
: PUSH-INTO ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena NTAPE:token -- n )
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE ORG-NONE LIVE-ADD ;

: PUSH-INTO-FROM ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena NTAPE:token n -- n )
   {: parent:n :}
   parent 0 < if E-NTAPE-ORIGIN throw then
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE parent 1+ LIVE-ADD ;

\ How many tokens have been appended so far. The only live reader: everything
\ else reads the sealed view.
: PUSHED ( IR-ARENA:arena -- n )
   dup HDR-CK CNT ;

\ Seal the tape. After this the builder handle rejects every append with
\ E-IR-ARENA-FROZEN, and the view is what checking, elaboration, diagnostics,
\ and code generation read.
: SEAL ( IR-ARENA:arena -- IR-ARENA:view )
   dup HDR-CK IR-ARENA:FREEZE ;

\ ---- reading a sealed tape ---------------------------------------------------
: TOKENS ( IR-ARENA:view -- n )
   dup FHDR-CK FCNT ;

: KIND@ ( IR-ARENA:view n -- NTAPE:kind )
   {: v:IR-ARENA:view i:n :}
   v i ORD-CK {: l:n :}
   v l OFF-KIND FRC@ N>KIND ;

: MODE@ ( IR-ARENA:view n -- NTAPE:mode )
   {: v:IR-ARENA:view i:n :}
   v i ORD-CK {: l:n :}
   v l OFF-MODE FRC@ N>MODE ;

: SPAN@ ( IR-ARENA:view IR-ID:ir-module-key n -- IR-SOURCE:span )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key i:n :}
   v key FKEY-CK
   v i ORD-CK {: l:n :}
   key v l OFF-SRC FRC@ IR-ID:PACK-SOURCE
   v l OFF-ST FRC@
   v l OFF-LN FRC@
   IR--SOURCE-SPAN:MAKE ;

: SPELL@ ( IR-ARENA:view IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key i:n :}
   v key FKEY-CK
   v i ORD-CK {: l:n :}
   key v l OFF-SYM FRC@ IR-ID:PACK-SYMBOL ;

\ The literal value. Probe the kind first: a kind that carries no literal
\ throws E-NTAPE-KIND rather than answering the zero the row stores, so a
\ caller cannot mistake "no literal" for "the value zero".
: LIT@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view i:n :}
   v i ORD-CK {: l:n :}
   v l OFF-KIND FRC@ N>KIND LIT-KIND? 0= if E-NTAPE-KIND throw then
   v l OFF-LIT FRC@ ;

: TOKEN@ ( IR-ARENA:view IR-ID:ir-module-key n -- NTAPE:token )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key i:n :}
   v key FKEY-CK
   v i ORD-CK {: l:n :}
   v l OFF-KIND FRC@ N>KIND
   v l OFF-MODE FRC@ N>MODE
   v key i SPELL@
   v l OFF-LIT FRC@
   v key i SPAN@
   NTAPE-TOKEN:MAKE ;

\ ---- origin chains -----------------------------------------------------------
: EXPANDED? ( IR-ARENA:view n -- bool )
   {: v:IR-ARENA:view i:n :}
   v i ORD-CK {: l:n :}
   v l OFF-ORG FRC@ ORG-NONE <> ;

private

\ Decode an origin cell against its child's ordinal: a directly lexed token
\ rejects, and a parent ordinal that fails the strict decrease is a corrupted
\ row rather than a caller error.
: ORG-LOCAL ( n n -- n )
   {: l:n og:n :}
   og ORG-NONE = if E-NTAPE-ROOT throw then
   og 1-
   dup l >= if E-NTAPE-STATE throw then
   dup 0 < if E-NTAPE-STATE throw then ;

public

\ The expansion parent's ordinal. Directly lexed tokens have none: probe with
\ EXPANDED? first; reading one throws E-NTAPE-ROOT.
: ORIGIN@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view i:n :}
   v i ORD-CK {: l:n :}
   l v l OFF-ORG FRC@ ORG-LOCAL ;

\ The expansion-chain length down to the directly lexed token. Each step
\ re-verifies the strict ordinal decrease, so the walk terminates on any tape
\ state, corrupted or not.
: DEPTH ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view i:n :}
   v i ORD-CK
   0 swap
   begin
      v over OFF-ORG FRC@ ORG-NONE <>
   while
      v over OFF-ORG FRC@ ORG-LOCAL
      swap 1+ swap
   repeat
   drop ;

\ ---- structural check --------------------------------------------------------
\ Everything PUSH proved when a row was written, proved again over a sealed
\ tape against the module's own frozen registries. A holder who bypasses this
\ package and appends raw cells to the arena is exactly who this catches:
\ the row shape, the stored codes, the byte spans, the spellings, the literal
\ rule, and the strict origin decrease all have to hold again.
private

: ROW-CK ( IR-ARENA:view IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key n -- )
   {: v:IR-ARENA:view sv:IR-ARENA:view yv:IR-ARENA:view
      key:IR-ID:ir-module-key l:n :}
   v l OFF-KIND FRC@ N>KIND {: k:NTAPE:kind :}
   v l OFF-MODE FRC@ N>MODE drop
   sv
      key v l OFF-SRC FRC@ IR-ID:PACK-SOURCE
      v l OFF-ST FRC@
      v l OFF-LN FRC@
      IR--SOURCE-SPAN:MAKE
   IR-SOURCE:FSPAN-CK
   yv key v l OFF-SYM FRC@ IR-ID:PACK-SYMBOL IR-SYM:FLEN@ drop
   k v l OFF-LIT FRC@ LIT-CK
   v l OFF-ORG FRC@ ORG-NONE = if exit then
   l v l OFF-ORG FRC@ ORG-LOCAL drop ;

public

\ The views are, in order, the tape, the module's frozen source registry, and
\ the module's frozen symbol rows.
: CHECK ( IR-ARENA:view IR-ID:ir-module-key IR-ARENA:view IR-ARENA:view -- )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key
      sv:IR-ARENA:view yv:IR-ARENA:view :}
   v key FKEY-CK
   v FCNT 0 ?do
      v sv yv key i ROW-CK
   loop ;

\ ---- the shared digest -------------------------------------------------------
private

1 constant PRE-VER                   \ the preimage schema version

\ Token record preimage: tag, schema version, then the eight stored cells. The
\ token's own ordinal is deliberately absent: the chain below folds the rows in
\ tape order, so a row's position is already bound by where its digest enters
\ the fold. Two tapes that hold the same tokens in a different order digest
\ differently with or without the ordinal, so storing it was a slot no test
\ could falsify. Which end the fold starts from is a convention, not a
\ property: reversing it relabels every digest at once and no comparison can
\ see it.
0 constant DS-TAG
1 constant DS-VER
2 constant DS-KIND
3 constant DS-MODE
4 constant DS-SRC
5 constant DS-ST
6 constant DS-LN
7 constant DS-SYM
8 constant DS-LIT
9 constant DS-ORG
10 constant DS-SLOTS

DS-SLOTS CDIGEST:SLOT-BYTES * constant DPRE-BYTES
create DPRE DPRE-BYTES allot

: DP! ( n n -- )
   DPRE swap CDIGEST:SLOT! ;

: ROW-DIGEST ( IR-ARENA:view n -- CDIGEST:digest )
   {: v:IR-ARENA:view l:n :}
   CDIGEST:TAG-TAPE-TOKEN DS-TAG DP!
   PRE-VER DS-VER DP!
   v l OFF-KIND FRC@ DS-KIND DP!
   v l OFF-MODE FRC@ DS-MODE DP!
   v l OFF-SRC FRC@ DS-SRC DP!
   v l OFF-ST FRC@ DS-ST DP!
   v l OFF-LN FRC@ DS-LN DP!
   v l OFF-SYM FRC@ DS-SYM DP!
   v l OFF-LIT FRC@ DS-LIT DP!
   v l OFF-ORG FRC@ DS-ORG DP!
   DPRE DPRE-BYTES CDIGEST:COMPUTE ;

\ The tape digest is a chain: seed over the token count, then one fold step per
\ token digest. Deterministic, covers every row in order, and needs no buffer
\ that grows with the tape - the same shape IR-SCHEMA's table digest uses. The
\ module serial is deliberately absent: it is allocated per process, and a
\ digest that moved between runs could never key a cache or bind a certificate.
\
\ The token count in the seed is the one field here no test can falsify. The
\ fold is fixed-arity, so there is no concatenation ambiguity a length prefix
\ would resolve, and any tape whose length differs already differs in its fold;
\ telling the seeded and unseeded chains apart would take a SHA-256 collision.
\ It is kept because the seed should state the tape's length and because this
\ chain then has exactly the shape of the sibling chain in IR-SCHEMA, not
\ because a mutation of it goes red. Every other slot in both preimages is
\ falsifiable and is falsified by test/compiler/native-tape.f.
0 constant TS-TAG
1 constant TS-VER
2 constant TS-A0
6 constant TS-B0
10 constant TS-SLOTS

TS-SLOTS CDIGEST:SLOT-BYTES * constant TPRE-BYTES
create TPRE TPRE-BYTES allot

: TP! ( n n -- )
   TPRE swap CDIGEST:SLOT! ;

: CHAIN-HEAD ( -- )
   CDIGEST:TAG-TAPE TS-TAG TP!
   PRE-VER TS-VER TP! ;

: CHAIN-SEED ( n -- CDIGEST:digest )
   {: cnt:n :}
   CHAIN-HEAD
   cnt TS-A0 TP!  0 TS-A0 1+ TP!  0 TS-A0 2 + TP!  0 TS-A0 3 + TP!
   0 TS-B0 TP!  0 TS-B0 1+ TP!  0 TS-B0 2 + TP!  0 TS-B0 3 + TP!
   TPRE TPRE-BYTES CDIGEST:COMPUTE ;

: CHAIN-STEP ( CDIGEST:digest CDIGEST:digest -- CDIGEST:digest )
   CDIGEST-DIGEST:UNMAKE {: v0:n v1:n v2:n v3:n :}
   CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   CHAIN-HEAD
   w0 TS-A0 TP!  w1 TS-A0 1+ TP!  w2 TS-A0 2 + TP!  w3 TS-A0 3 + TP!
   v0 TS-B0 TP!  v1 TS-B0 1+ TP!  v2 TS-B0 2 + TP!  v3 TS-B0 3 + TP!
   TPRE TPRE-BYTES CDIGEST:COMPUTE ;

public

\ The one value the source, checking, and elaboration stages share. Every stage
\ that reads a tape records this, and hands it back through VERIFY.
: DIGEST ( IR-ARENA:view -- CDIGEST:digest )
   {: v:IR-ARENA:view :}
   v FHDR-CK
   v FCNT CHAIN-SEED
   v FCNT 0 ?do
      v i ROW-DIGEST CHAIN-STEP
   loop ;

\ Recompute the tape digest and reject a presented one that differs. This is
\ what makes "the checker and the elaborator read the same tape" a checked
\ fact rather than a convention.
: VERIFY ( IR-ARENA:view CDIGEST:digest -- )
   CDIGEST-DIGEST:UNMAKE
   {: v:IR-ARENA:view w0:n w1:n w2:n w3:n :}
   v DIGEST  w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   0= if E-NTAPE-DIGEST throw then ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
