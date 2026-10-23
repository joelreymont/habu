\ tape.f - the native stage N0 source tape: the exact token stream the
\ compiler consumed, captured once, and sealed.
\
\ Build live, read frozen: NEW and the appends are the only live
\ operations, because readers need a complete token stream.
\
\ A token's resolved spelling is an interned IR-SYM symbol, not a second byte
\ slice: the span already says where it was read, and the symbol lets a reader
\ compare names without re-lexing. A string literal's spelling is its body.
\
\ A token's origin is its expansion parent TOKEN, the token-level twin of
\ IR-SOURCE's parent source, so a diagnostic walks back to the token the
\ programmer wrote. An origin ordinal is strictly below its child's, which is
\ what makes the expansion relation acyclic; every walk re-verifies that
\ decrease.
\
\ Whether a token carries a literal is a property of its kind, with one
\ minting word per kind and no stored flag to contradict it: LIT@ on a kind
\ that carries none throws E-NTAPE-KIND rather than answer the stored zero.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f

package NTAPE
public

\ Closed: a construct this stage has not been taught is a capability to add here.
\ `real-literal` carries the CELL the double is - a Habu double is one unboxed
\ cell holding its IEEE754 bits - so it rides the same field an integer does.
ENUM kind DERIVE eq
   name
   int-literal
   char-literal
   string-literal
   real-literal
   counted-string-literal
   printed-string-literal
;ENUM

\ Which mode applied is a fact about the token, not about the tape.
ENUM mode DERIVE eq
   interpreting
   compiling
;ENUM

\ A value is not authority: the generated constructor is open, so PUSH revalidates
\ every field against the module's registries before a row exists.
\ A multi-cell value binds whole to a typed local, so a word that only carries a
\ token or a span names it; the append fronts still unmake one, because there
\ every field becomes its own arena cell.
STRUCTURE token
   FIELD kind kind
   FIELD mode mode
   FIELD spell IR-ID:ir-symbol-id
   FIELD lit n
   FIELD span IR-SOURCE:span
;STRUCTURE

private

\ A one-way projection of the sealed module key onto its serial, for the header.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

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
\ Stored in token rows; existing codes keep their meaning.
: KIND-CODE ( NTAPE:kind -- n )
   MATCH kind
      name           OF 0 ENDOF
      int-literal    OF 1 ENDOF
      char-literal   OF 2 ENDOF
      string-literal OF 3 ENDOF
      real-literal   OF 4 ENDOF
      counted-string-literal OF 5 ENDOF
      printed-string-literal OF 6 ENDOF
   ;MATCH ;

: N>KIND ( n -- NTAPE:kind )
   case
      0 of NTAPE-KIND:NAME endof
      1 of NTAPE-KIND:INT-LITERAL endof
      2 of NTAPE-KIND:CHAR-LITERAL endof
      3 of NTAPE-KIND:STRING-LITERAL endof
      4 of NTAPE-KIND:REAL-LITERAL endof
      5 of NTAPE-KIND:COUNTED-STRING-LITERAL endof
      6 of NTAPE-KIND:PRINTED-STRING-LITERAL endof
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
\ Which kinds carry a literal is a property of the kind, so there is no separate
\ flag to contradict it; a kind without one stores exactly zero.
: LIT-KIND? ( NTAPE:kind -- bool )
   MATCH kind
      name           OF false ENDOF
      int-literal    OF true ENDOF
      char-literal   OF true ENDOF
      string-literal OF false ENDOF
      real-literal   OF true ENDOF
      counted-string-literal OF false ENDOF
      printed-string-literal OF false ENDOF
   ;MATCH ;

: SIGNED-KIND? ( NTAPE:kind -- bool )
   MATCH kind
      name           OF false ENDOF
      int-literal    OF true ENDOF
      char-literal   OF false ENDOF
      string-literal OF false ENDOF
      real-literal   OF true ENDOF
      counted-string-literal OF false ENDOF
      printed-string-literal OF false ENDOF
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
\ Every read below goes through an IR-ARENA reader opened once per public word.
\ A reader carries the state it was opened against and refuses the other with
\ the error that handle gave, so the live and frozen helpers are one set.

\ ---- header and shape --------------------------------------------------------
: SHAPE-CK ( n -- )
   dup HDR-CELLS < if E-NTAPE-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-NTAPE-STATE throw then ;

: MAGIC-CK ( n -- )
   MAGIC <> if E-NTAPE-STATE throw then ;

: HDR-CK ( IR-ARENA:reader -- )
   {: r:IR-ARENA:reader :}
   r IR-ARENA:RD-SIZE SHAPE-CK
   r HC-MAGIC IR-ARENA:RD@ MAGIC-CK ;

: USED>CNT ( n -- n )
   HDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:reader -- n )
   IR-ARENA:RD-SIZE USED>CNT ;

\ ---- ownership ---------------------------------------------------------------
\ Three arenas of the same type meet at the appends and the checker cannot tell
\ them apart, so each package rechecks its own header tag.
: SERIAL-CK ( n n -- )
   <> if E-NTAPE-OWNER throw then ;

: KEY-CK ( IR-ARENA:reader IR-ID:ir-module-key -- )
   {: r:IR-ARENA:reader key:IR-ID:ir-module-key :}
   r HDR-CK
   r HC-SERIAL IR-ARENA:RD@ key KEY-SERIAL SERIAL-CK ;

\ Appending needs no module key: the token's own identities carry their owning
\ module, which is a check a caller cannot supply the wrong value for.
: SRC-OWNER-CK ( IR-ARENA:reader IR-ID:ir-source-id -- )
   {: r:IR-ARENA:reader id:IR-ID:ir-source-id :}
   r HDR-CK
   r HC-SERIAL IR-ARENA:RD@ id IR-ID:SOURCE-OWNER MID-SERIAL SERIAL-CK ;

: SYM-OWNER-CK ( IR-ARENA:reader IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:reader id:IR-ID:ir-symbol-id :}
   r HC-SERIAL IR-ARENA:RD@ id IR-ID:SYMBOL-OWNER MID-SERIAL SERIAL-CK ;

\ ---- row addressing ----------------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:reader n n -- n )
   ROW-CELL IR-ARENA:RD@ ;

: ORD-CK ( IR-ARENA:reader n -- n )
   {: r:IR-ARENA:reader i:n :}
   r HDR-CK
   i 0 < if E-NTAPE-BOUND throw then
   i r CNT >= if E-NTAPE-BOUND throw then
   i ;

\ ---- creation ----------------------------------------------------------------
: CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-NTAPE-CAP throw then
   drop ;

public

\ The cell ceiling is committed to exactly cap tokens; the tape dies with its ctx.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key cap:n :}
   cap CAP-OK
   c cap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a HDR-CELLS IR-ARENA:RESERVE
   c a MAGIC IR-ARENA:PUSH drop
   c a key KEY-SERIAL IR-ARENA:PUSH drop
   c a cap IR-ARENA:PUSH drop
   a ;

\ ---- minting tokens ----------------------------------------------------------
\ One constructor per kind, so the literal rule is structural.
private

: MK ( IR-SOURCE:span NTAPE:kind NTAPE:mode IR-ID:ir-symbol-id n -- NTAPE:token )
   {: sp:IR-SOURCE:span k:NTAPE:kind m:NTAPE:mode sy:IR-ID:ir-symbol-id v:n :}
   k v LIT-CK
   k m sy v sp NTAPE-TOKEN:MAKE ;

public

: NAME-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode :}
   NTAPE-KIND:NAME m sy 0 MK ;

: STRING-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode :}
   NTAPE-KIND:STRING-LITERAL m sy 0 MK ;


: COUNTED-STRING-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode :}
   NTAPE-KIND:COUNTED-STRING-LITERAL m sy 0 MK ;


: PRINTED-STRING-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode :}
   NTAPE-KIND:PRINTED-STRING-LITERAL m sy 0 MK ;


: INT-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode n -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode v:n :}
   NTAPE-KIND:INT-LITERAL m sy v MK ;

: CHAR-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode n -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode v:n :}
   NTAPE-KIND:CHAR-LITERAL m sy v MK ;

\ The value is the literal's bit pattern: this states no format and converts nothing.
: REAL-TOKEN ( IR-SOURCE:span IR-ID:ir-symbol-id NTAPE:mode n -- NTAPE:token )
   {: sy:IR-ID:ir-symbol-id m:NTAPE:mode v:n :}
   NTAPE-KIND:REAL-LITERAL m sy v MK ;

\ ---- appending ---------------------------------------------------------------
private

: ROOM-CK ( IR-ARENA:reader -- )
   {: r:IR-ARENA:reader :}
   r CNT r HC-CAP IR-ARENA:RD@ >= if E-NTAPE-CAP throw then ;

\ Stored as the parent's ordinal plus one, so zero means "directly lexed" without
\ a sentinel inside the ordinal range. A parent must already be on this tape.
: ORG-CK ( IR-ARENA:reader n -- )
   {: r:IR-ARENA:reader og:n :}
   og ORG-NONE = if exit then
   og 0 < if E-NTAPE-ORIGIN throw then
   og 1- r CNT >= if E-NTAPE-ORIGIN throw then ;

\ The fields the tape alone can judge, checked before the module's other tables
\ are consulted, so a non-tape arena dies on its own header tag.
: FIELD-CK ( IR-ARENA:reader NTAPE:kind IR-ID:ir-symbol-id n IR-ID:ir-source-id n -- )
   {: r:IR-ARENA:reader k:NTAPE:kind id:IR-ID:ir-symbol-id v:n
      sid:IR-ID:ir-source-id og:n :}
   r sid SRC-OWNER-CK
   r id SYM-OWNER-CK
   r og ORG-CK
   k v LIT-CK ;

\ The only word here that appends a cell, reached only through the two fronts.
\ The reader outlives the reservation deliberately: a RESERVE changes neither
\ the generation nor the state, and the count comes from the row on every read.
: WRITE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:reader NTAPE:kind NTAPE:mode IR-ID:ir-symbol-id n IR-ID:ir-source-id n n n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:reader
      k:NTAPE:kind m:NTAPE:mode id:IR-ID:ir-symbol-id v:n
      sid:IR-ID:ir-source-id st:n ln:n og:n :}
   r ROOM-CK
   c a ROW-CELLS IR-ARENA:RESERVE
   r CNT {: i:n :}
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
\ A module still being built holds its tables privately in ir/build.f so it stays
\ the only mutation route, and answers the same two questions through its readers.
: TABLE-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena NTAPE:kind NTAPE:mode IR-ID:ir-symbol-id n IR-ID:ir-source-id n n n -- n )
   {: c:IR-CTX:ctx a:IR-ARENA:arena sr:IR-ARENA:arena sy:IR-ARENA:arena
      k:NTAPE:kind m:NTAPE:mode id:IR-ID:ir-symbol-id v:n
      sid:IR-ID:ir-source-id st:n ln:n og:n :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r k id v sid og FIELD-CK
   sr sid st ln IR--SOURCE-SPAN:MAKE IR-SOURCE:SPAN-CK
   sy id IR-SYM:LEN@ drop
   c a r k m id v sid st ln og WRITE ;

: LIVE-ADD ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena NTAPE:kind NTAPE:mode IR-ID:ir-symbol-id n IR-ID:ir-source-id n n n -- n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ARENA:arena
      k:NTAPE:kind m:NTAPE:mode id:IR-ID:ir-symbol-id v:n
      sid:IR-ID:ir-source-id st:n ln:n og:n :}
   a IR-ARENA:OPEN-LIVE {: r:IR-ARENA:reader :}
   r k id v sid og FIELD-CK
   c b  sid st ln IR--SOURCE-SPAN:MAKE  IR-BUILD:SPAN-CK
   c b id IR-BUILD:SYMBOL-CK
   c a r k m id v sid st ln og WRITE ;

public

\ Arena order: the tape, the module's source registry, the module's symbol rows.
: PUSH ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena NTAPE:token -- n )
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE ORG-NONE TABLE-ADD ;

\ The parent ordinal rides on top so the token beneath can be unmade in place.
: PUSH-FROM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena NTAPE:token n -- n )
   {: parent:n :}
   parent 0 < if E-NTAPE-ORIGIN throw then
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE parent 1+ TABLE-ADD ;

: PUSH-INTO ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena NTAPE:token -- n )
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE ORG-NONE LIVE-ADD ;

: PUSH-INTO-FROM ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena NTAPE:token n -- n )
   {: parent:n :}
   parent 0 < if E-NTAPE-ORIGIN throw then
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE parent 1+ LIVE-ADD ;

\ After this the builder handle rejects every append with E-IR-ARENA-FROZEN.
: SEAL ( IR-ARENA:arena -- IR-ARENA:view )
   dup IR-ARENA:OPEN-LIVE HDR-CK IR-ARENA:FREEZE ;

\ ---- reading a sealed tape ---------------------------------------------------
: TOKENS ( IR-ARENA:view -- n )
   IR-ARENA:OPEN dup HDR-CK CNT ;

: KIND@ ( IR-ARENA:view n -- NTAPE:kind )
   {: v:IR-ARENA:view i:n :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r i ORD-CK {: l:n :}
   r l OFF-KIND RC@ N>KIND ;

: MODE@ ( IR-ARENA:view n -- NTAPE:mode )
   {: v:IR-ARENA:view i:n :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r i ORD-CK {: l:n :}
   r l OFF-MODE RC@ N>MODE ;

: SPAN@ ( IR-ARENA:view IR-ID:ir-module-key n -- IR-SOURCE:span )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key i:n :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r key KEY-CK
   r i ORD-CK {: l:n :}
   key r l OFF-SRC RC@ IR-ID:PACK-SOURCE
   r l OFF-ST RC@
   r l OFF-LN RC@
   IR--SOURCE-SPAN:MAKE ;

: SPELL@ ( IR-ARENA:view IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key i:n :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r key KEY-CK
   r i ORD-CK {: l:n :}
   key r l OFF-SYM RC@ IR-ID:PACK-SYMBOL ;

\ Probe the kind first: a kind carrying no literal throws rather than answering
\ the zero the row stores, so "no literal" cannot be read as the value zero.
: LIT@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view i:n :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r i ORD-CK {: l:n :}
   r l OFF-KIND RC@ N>KIND LIT-KIND? 0= if E-NTAPE-KIND throw then
   r l OFF-LIT RC@ ;

\ ---- origin chains -----------------------------------------------------------
: EXPANDED? ( IR-ARENA:view n -- bool )
   {: v:IR-ARENA:view i:n :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r i ORD-CK {: l:n :}
   r l OFF-ORG RC@ ORG-NONE <> ;

private

\ A parent ordinal failing the strict decrease is a corrupted row, not a caller error.
: ORG-LOCAL ( n n -- n )
   {: l:n og:n :}
   og ORG-NONE = if E-NTAPE-ROOT throw then
   og 1-
   dup l >= if E-NTAPE-STATE throw then
   dup 0 < if E-NTAPE-STATE throw then ;

public

\ Directly lexed tokens have none: probe with EXPANDED? first.
: ORIGIN@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view i:n :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r i ORD-CK {: l:n :}
   l r l OFF-ORG RC@ ORG-LOCAL ;

\ Each step re-verifies the strict decrease, so the walk terminates on any state;
\ what it no longer does is resolve the view once per step.
: DEPTH ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view i:n :}
   v IR-ARENA:OPEN {: r:IR-ARENA:reader :}
   r i ORD-CK
   0 swap
   begin
      r over OFF-ORG RC@ ORG-NONE <>
   while
      r over OFF-ORG RC@ ORG-LOCAL
      swap 1+ swap
   repeat
   drop ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
