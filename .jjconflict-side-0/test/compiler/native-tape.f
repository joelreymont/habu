\ native-tape.f - checked stage N0 source-tape tests.
\
\ Proves the section 7.1 contract of src/compiler/native/tape.f: a token
\ records exactly its kind, byte span, resolved spelling, literal value, parser
\ mode and origin; the four minting words make the per-kind literal rule
\ structural and the append revalidates a constructor-forged token against the
\ module's own source registry and symbol store; expansion origins must already
\ be appended, so cycles cannot be built and walks terminate; a sealed tape is
\ read-only and answers one digest that every stage can bind to; and a holder
\ who bypasses the package and appends raw cells is caught by CHECK, one named
\ error per corrupted field.

require lib/test.f
require test/checker-assert.f
require test/compiler/ir-starve.f
require src/compiler/native/tape.f

package NTAPE-TEST
private

\ ---- fixtures ----------------------------------------------------------------
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ The text every fixture registers as its one source. Twelve bytes, so a span
\ of 0..12 is legal and 5..12 is not.
: TEXT ( -- ptr u8 n )
   s" : SQUARE 2 ;" ;

\ A whole module: key, source registry, symbol pool, symbol rows, tape.
: MOD-NEW ( IR-CTX:ctx n -- IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx cap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key
   c key 4 IR-SOURCE:NEW
   c key 8 64 IR-SYM:NEW
   c key cap NTAPE:NEW ;

\ The one registered source of such a module.
: SRC ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-source-id )
   {: c:IR-CTX:ctx sr:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c sr key TEXT IR-SOURCE:REGISTER ;

\ ---- a token is one commit, at the scratch edge --------------------------------
\ A tape token is eight appends, and an append grows the arena by taking a span
\ from the context mapping. Pushing a token at the edge of that mapping used to
\ stop part way through the eight - a cell count eight does not divide - and
\ every later read of the tape failed its shape check. The token's storage is
\ now reserved before its first cell.
\
\ Header and one token put the tape at eleven of sixteen cells, so the second
\ token needs a doubled span and cannot have one. If that stops being true the
\ push succeeds, its code is zero, and this case fails.
3 constant T-HDR-CELLS
8 constant T-ROW-CELLS

\ Depth-neutral: the inputs ride beneath the push that fails.
: TORN-PUSH ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-source-id IR-ID:ir-symbol-id -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-source-id IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena sr:IR-ARENA:arena sy:IR-ARENA:arena key:IR-ID:ir-module-key s0:IR-ID:ir-source-id n1:IR-ID:ir-symbol-id :}
   c tp sr sy key s0 n1
   c tp sr sy
      sr s0 9 1 IR-SOURCE:SPAN n1 NTAPE-MODE:COMPILING 2 NTAPE:INT-TOKEN
      NTAPE:PUSH drop ;

\ Everything the refusal must have left alone: the tape reads, holds exactly
\ the one token that landed, still answers its fields, and has not advanced a
\ cell. Sealing is how a tape's fields are read at all, and it is the operation
\ a torn tape refuses first.
: TORN-INTACT ( IR-ARENA:arena IR-ID:ir-module-key -- IR-ARENA:arena IR-ID:ir-module-key )
   {: tp:IR-ARENA:arena key:IR-ID:ir-module-key :}
   tp key
   tp NTAPE:PUSHED 1 <> if E-NTAPE-STATE throw then
   tp IR-ARENA:USED T-HDR-CELLS T-ROW-CELLS + <> if E-NTAPE-STATE throw then
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   v NTAPE:TOKENS 1 <> if E-NTAPE-STATE throw then
   v 0 NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NTAPE-STATE throw then
   v key 0 NTAPE:SPAN@ IR-SOURCE:SPAN-LEN 6 <> if E-NTAPE-STATE throw then ;

: TORN-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" SQUARE" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c sp sy key s" 2" IR-SYM:INTERN {: n1:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 2 6 IR-SOURCE:SPAN n0 NTAPE-MODE:INTERPRETING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c IR-STARVE:EDGE
   c tp sr sy key s0 n1 [: TORN-PUSH ;] catch
   {: c2:IR-CTX:ctx tp2:IR-ARENA:arena sr2:IR-ARENA:arena sy2:IR-ARENA:arena key2:IR-ID:ir-module-key s02:IR-ID:ir-source-id n12:IR-ID:ir-symbol-id rc:n :}
   rc
   tp2 key2 [: TORN-INTACT ;] catch nip nip ;

: TORN-CASE ( -- )
   s" a token refused mid-row leaves the tape whole and readable" T-LABEL
   BND [: TORN-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= E-IR-CTX-SCRATCH T= ;

\ ---- appending and reading back ----------------------------------------------
\ One token of every kind, appended in parser order, read back through the
\ sealed view. This is the whole six-field record of section 7.1 in one case.
: RB-BODY ( IR-CTX:ctx -- n n bool bool n n n n n )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" SQUARE" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c sp sy key s" 2" IR-SYM:INTERN {: n1:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 2 6 IR-SOURCE:SPAN n0 NTAPE-MODE:INTERPRETING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 9 1 IR-SOURCE:SPAN n1 NTAPE-MODE:COMPILING 2 NTAPE:INT-TOKEN
      NTAPE:PUSH drop
   tp NTAPE:PUSHED
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   v NTAPE:TOKENS
   v 0 NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ
   v 1 NTAPE:MODE@ NTAPE-MODE:COMPILING NTAPE-MODE:EQ
   v key 0 NTAPE:SPAN@ IR-SOURCE:SPAN-START
   v key 0 NTAPE:SPAN@ IR-SOURCE:SPAN-LEN
   v key 1 NTAPE:SPELL@ IR-ID:SYMBOL-LOCAL
   v 1 NTAPE:LIT@
   v 0 NTAPE:EXPANDED? if 1 else 0 then ;

: RB-CASE ( -- )
   s" every field of a token survives the seal" T-LABEL
   BND [: RB-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= 1 T= 6 T= 2 T= TTRUE TTRUE 2 T= 2 T= ;

\ TOKEN@ answers the value that was appended, field for field.
: TK-BODY ( IR-CTX:ctx -- bool bool n n n )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" A" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 3 4 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING 65 NTAPE:CHAR-TOKEN
      NTAPE:PUSH drop
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   v key 0 NTAPE:TOKEN@ NTAPE-TOKEN:UNMAKE
   IR--SOURCE-SPAN:UNMAKE {: sid:IR-ID:ir-source-id st:n ln:n :}
   {: k:NTAPE:kind m:NTAPE:mode id:IR-ID:ir-symbol-id lv:n :}
   k NTAPE-KIND:CHAR-LITERAL NTAPE-KIND:EQ
   m NTAPE-MODE:COMPILING NTAPE-MODE:EQ
   lv
   st
   ln ;

: TK-CASE ( -- )
   s" TOKEN@ answers the token that was appended" T-LABEL
   BND [: TK-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 3 T= 65 T= TTRUE TTRUE ;

\ ---- the per-kind literal rule -----------------------------------------------
\ A character literal is a code point, so a negative one is refused where the
\ token is minted, before any tape is involved.
: NEGCHAR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" A" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING -1 NTAPE:CHAR-TOKEN
   NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE drop drop drop drop drop drop drop ;

: NEGCHAR ( -- )
   BND [: NEGCHAR-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The generated constructor is open, so a name token carrying a value can be
\ assembled. The append recheck is what refuses it.
: FORGELIT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" X" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      NTAPE-KIND:NAME NTAPE-MODE:COMPILING n0 7
      sr s0 0 1 IR-SOURCE:SPAN NTAPE-TOKEN:MAKE
      NTAPE:PUSH drop ;

: FORGELIT ( -- )
   BND [: FORGELIT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Reading a literal from a kind that has none is refused rather than answered
\ with the zero the row stores, so "no literal" cannot be read as "zero".
: NOLIT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" X" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   tp NTAPE:SEAL 0 NTAPE:LIT@ drop ;

: NOLIT ( -- )
   BND [: NOLIT-BODY ;] IR-CTX:WITH-CONTEXT ;

: LIT-CASES ( -- )
   s" a negative character code is refused where the token is minted" T-LABEL
   [: NEGCHAR ;] E-NTAPE-LITERAL TTHROWSQ
   s" a constructor-forged literal on a name token is refused at append" T-LABEL
   [: FORGELIT ;] E-NTAPE-LITERAL TTHROWSQ
   s" reading a literal from a kind that has none is refused" T-LABEL
   [: NOLIT ;] E-NTAPE-KIND TTHROWSQ ;

\ ---- spans are the source registry's concern ---------------------------------
\ A span assembled past its source's end never reaches a row: the append hands
\ it back to IR-SOURCE, which owns byte ranges and names the error.
: BADSPAN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" X" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      NTAPE-KIND:NAME NTAPE-MODE:COMPILING n0 0
      s0 5 99 IR--SOURCE-SPAN:MAKE NTAPE-TOKEN:MAKE
      NTAPE:PUSH drop ;

: BADSPAN ( -- )
   BND [: BADSPAN-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A span naming a source ordinal the registry never registered dies the same
\ way, on the registry's own bound check.
: GHOSTSRC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC drop
   c sp sy key s" X" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      NTAPE-KIND:NAME NTAPE-MODE:COMPILING n0 0
      key 3 IR-ID:PACK-SOURCE 0 1 IR--SOURCE-SPAN:MAKE NTAPE-TOKEN:MAKE
      NTAPE:PUSH drop ;

: GHOSTSRC ( -- )
   BND [: GHOSTSRC-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A spelling the symbol store never interned dies on the interner's bound
\ check, which is the store that owns names.
: GHOSTSYM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" X" IR-SYM:INTERN drop
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN key 5 IR-ID:PACK-SYMBOL
      NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop ;

: GHOSTSYM ( -- )
   BND [: GHOSTSYM-BODY ;] IR-CTX:WITH-CONTEXT ;

: SPAN-CASES ( -- )
   s" a span crossing its source's end is refused at append" T-LABEL
   [: BADSPAN ;] E-IR-SRC-SPAN TTHROWSQ
   s" a span naming an unregistered source is refused at append" T-LABEL
   [: GHOSTSRC ;] E-IR-SRC-BOUND TTHROWSQ
   s" a spelling the interner never minted is refused at append" T-LABEL
   [: GHOSTSYM ;] E-IR-SYM-BOUND TTHROWSQ ;

\ ---- module ownership --------------------------------------------------------
\ A source-id minted for another module cannot enter this tape, and neither can
\ another module's spelling. The tape is bound to a module serial, and the
\ token's own identities carry theirs, so no presented key is involved.
: XSRC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 8 MOD-NEW
   {: k2:IR-ID:ir-module-key sr2:IR-ARENA:arena
      sp2:IR-ARENA:arena sy2:IR-ARENA:arena tp2:IR-ARENA:arena :}
   c sr2 k2 SRC {: s2:IR-ID:ir-source-id :}
   c sp sy key s" X" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr2 sy
      sr2 s2 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop ;

: XSRC ( -- )
   BND [: XSRC-BODY ;] IR-CTX:WITH-CONTEXT ;

: XSYM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 8 MOD-NEW
   {: k2:IR-ID:ir-module-key sr2:IR-ARENA:arena
      sp2:IR-ARENA:arena sy2:IR-ARENA:arena tp2:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp2 sy2 k2 s" X" IR-SYM:INTERN {: n2:IR-ID:ir-symbol-id :}
   c tp sr sy2
      sr s0 0 1 IR-SOURCE:SPAN n2 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop ;

: XSYM ( -- )
   BND [: XSYM-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Three arenas of the same checker type meet at PUSH. Handing it the source
\ registry where the tape belongs cannot be caught by the checker, so every
\ arena carries its own header tag and the swap dies on the tag.
: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" X" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c sr tp sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop ;

: SWAPPED ( -- )
   BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A foreign key rejects on a reader, which is where a key is genuinely needed:
\ SPAN@ re-mints an identity and must do it under the owning module.
: XKEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c 8 MOD-NEW
   {: k2:IR-ID:ir-module-key sr2:IR-ARENA:arena
      sp2:IR-ARENA:arena sy2:IR-ARENA:arena tp2:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" X" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   tp NTAPE:SEAL k2 0 NTAPE:SPAN@ IR-SOURCE:SPAN-LEN drop ;

: XKEY ( -- )
   BND [: XKEY-BODY ;] IR-CTX:WITH-CONTEXT ;

: OWNER-CASES ( -- )
   s" another module's source-id cannot enter this tape" T-LABEL
   [: XSRC ;] E-NTAPE-OWNER TTHROWSQ
   s" another module's spelling cannot enter this tape" T-LABEL
   [: XSYM ;] E-NTAPE-OWNER TTHROWSQ
   s" a tape and a source registry swapped at the call site die on the tag" T-LABEL
   [: SWAPPED ;] E-NTAPE-STATE TTHROWSQ
   s" a foreign module key is refused by a reader" T-LABEL
   [: XKEY ;] E-NTAPE-OWNER TTHROWSQ ;

\ ---- expansion origins -------------------------------------------------------
: OG-BODY ( IR-CTX:ctx -- n n n bool bool )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" M" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH {: t0:n :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      t0 NTAPE:PUSH-FROM {: t1:n :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      t1 NTAPE:PUSH-FROM {: t2:n :}
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   v t2 NTAPE:DEPTH
   v t0 NTAPE:DEPTH
   v t2 NTAPE:ORIGIN@
   v t0 NTAPE:EXPANDED?
   v t2 NTAPE:EXPANDED? ;

: OG-CASE ( -- )
   s" expansion origins record parents and walks terminate" T-LABEL
   BND [: OG-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE 1 T= 0 T= 2 T= ;

: OGROOT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" M" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   tp NTAPE:SEAL 0 NTAPE:ORIGIN@ drop ;

: OGROOT ( -- )
   BND [: OGROOT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A self cycle: the origin names the very ordinal the append is about to take.
\ A parent must already exist, so the edge dies before the row does.
: OGSELF-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" M" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      0 NTAPE:PUSH-FROM drop ;

: OGSELF ( -- )
   BND [: OGSELF-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A negative parent must not fold into the "directly lexed" encoding.
: OGNEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" M" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      -1 NTAPE:PUSH-FROM drop ;

: OGNEG ( -- )
   BND [: OGNEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: OG-REJECT-CASES ( -- )
   s" reading the origin of a directly lexed token is refused" T-LABEL
   [: OGROOT ;] E-NTAPE-ROOT TTHROWSQ
   s" an origin naming the ordinal about to be taken is refused" T-LABEL
   [: OGSELF ;] E-NTAPE-ORIGIN TTHROWSQ
   s" a negative expansion parent cannot pass as directly lexed" T-LABEL
   [: OGNEG ;] E-NTAPE-ORIGIN TTHROWSQ ;

\ ---- bounds and capacity -----------------------------------------------------
: BD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   tp NTAPE:SEAL 0 NTAPE:KIND@ drop ;

: BD ( -- )
   BND [: BD-BODY ;] IR-CTX:WITH-CONTEXT ;

: BDNEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   tp NTAPE:SEAL -1 NTAPE:KIND@ drop ;

: BDNEG ( -- )
   BND [: BDNEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAPZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 0 NTAPE:NEW IR-ARENA:ABORT ;

: CAPZERO ( -- )
   BND [: CAPZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAPNEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key -4 NTAPE:NEW IR-ARENA:ABORT ;

: CAPNEG ( -- )
   BND [: CAPNEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAPHUGE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key $30000000 NTAPE:NEW IR-ARENA:ABORT ;

: CAPHUGE ( -- )
   BND [: CAPHUGE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The caught quotation re-pushes its inputs beneath the append that overflows,
\ so they survive the throw and the tape can be inspected afterwards.
: CAPTHIRD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-source-id IR-ID:ir-symbol-id -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-source-id IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena sr:IR-ARENA:arena sy:IR-ARENA:arena
      s0:IR-ID:ir-source-id n0:IR-ID:ir-symbol-id :}
   c tp sr sy s0 n0
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop ;

: CAPFULL-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 2 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" M" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c tp sr sy s0 n0 [: CAPTHIRD ;] catch
   {: c2:IR-CTX:ctx tp2:IR-ARENA:arena sr2:IR-ARENA:arena sy2:IR-ARENA:arena
      s2:IR-ID:ir-source-id n2:IR-ID:ir-symbol-id rc:n :}
   rc
   tp2 NTAPE:PUSHED ;

: BOUND-CASES ( -- )
   s" a token ordinal past the count is refused" T-LABEL
   [: BD ;] E-NTAPE-BOUND TTHROWSQ
   s" a negative token ordinal is refused" T-LABEL
   [: BDNEG ;] E-NTAPE-BOUND TTHROWSQ
   s" a zero tape capacity is refused at creation" T-LABEL
   [: CAPZERO ;] E-NTAPE-CAP TTHROWSQ
   s" a negative tape capacity is refused at creation" T-LABEL
   [: CAPNEG ;] E-NTAPE-CAP TTHROWSQ
   s" a capacity past the arena ordinal range is refused at creation" T-LABEL
   [: CAPHUGE ;] E-NTAPE-CAP TTHROWSQ
   s" a full tape refuses named and stays whole" T-LABEL
   BND [: CAPFULL-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= E-NTAPE-CAP T= ;

\ ---- sealing -----------------------------------------------------------------
: SEALED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" M" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   tp NTAPE:SEAL drop
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop ;

: SEALED ( -- )
   BND [: SEALED-BODY ;] IR-CTX:WITH-CONTEXT ;

: SEAL-CASE ( -- )
   s" a sealed tape refuses every further append" T-LABEL
   [: SEALED ;] E-IR-ARENA-FROZEN TTHROWSQ ;

\ ---- arenas that are not tapes -----------------------------------------------
: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 16 IR-ARENA:NEW NTAPE:PUSHED drop ;

: RAW ( -- )
   BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHAPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c tp 7 IR-ARENA:PUSH drop
   tp NTAPE:PUSHED drop ;

: SHAPE ( -- )
   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A three-cell arena has the row shape of an empty tape, so the shape check
\ passes it and only the header tag can tell it apart. Without this fixture the
\ tag check is dead weight the suite could not see.
: TAG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 3 IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a 99 IR-ARENA:PUSH drop
   c a 1 IR-ARENA:PUSH drop
   c a 1 IR-ARENA:PUSH drop
   a NTAPE:PUSHED drop ;

: TAG ( -- )
   BND [: TAG-BODY ;] IR-CTX:WITH-CONTEXT ;

: STATE-CASES ( -- )
   s" a bare arena is not a source tape" T-LABEL
   [: RAW ;] E-NTAPE-STATE TTHROWSQ
   s" a misaligned row shape is refused" T-LABEL
   [: SHAPE ;] E-NTAPE-STATE TTHROWSQ
   s" an arena of the right shape and the wrong tag is refused" T-LABEL
   [: TAG ;] E-NTAPE-STATE TTHROWSQ ;

\ ---- corrupted rows caught by CHECK ------------------------------------------
\ A holder who bypasses this package and appends raw cells writes a row whose
\ shape is right and whose content is not. CHECK is what re-proves every field
\ of a sealed tape against the module's own frozen tables.
: RAW-ROW ( IR-CTX:ctx IR-ARENA:arena n n n n n n n n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena
      k:n m:n s:n st:n ln:n sy:n li:n og:n :}
   c tp k IR-ARENA:PUSH drop
   c tp m IR-ARENA:PUSH drop
   c tp s IR-ARENA:PUSH drop
   c tp st IR-ARENA:PUSH drop
   c tp ln IR-ARENA:PUSH drop
   c tp sy IR-ARENA:PUSH drop
   c tp li IR-ARENA:PUSH drop
   c tp og IR-ARENA:PUSH drop ;

\ Build a module whose tape holds exactly one hand-written row, then check it.
: CORRUPT ( IR-CTX:ctx n n n n n n n n -- )
   {: c:IR-CTX:ctx k:n m:n s:n st:n ln:n sy:n li:n og:n :}
   c 4 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy2:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC drop
   c sp sy2 key s" M" IR-SYM:INTERN drop
   c tp k m s st ln sy li og RAW-ROW
   tp NTAPE:SEAL key sr IR-ARENA:FREEZE sy2 IR-ARENA:FREEZE NTAPE:CHECK ;

: OK-ROW ( IR-CTX:ctx -- )
   0 0 0 0 1 0 0 0 CORRUPT ;

: BAD-KIND ( IR-CTX:ctx -- )
   9 0 0 0 1 0 0 0 CORRUPT ;

: BAD-MODE ( IR-CTX:ctx -- )
   0 5 0 0 1 0 0 0 CORRUPT ;

: BAD-SPAN ( IR-CTX:ctx -- )
   0 0 0 6 90 0 0 0 CORRUPT ;

: BAD-SRC ( IR-CTX:ctx -- )
   0 0 4 0 1 0 0 0 CORRUPT ;

: BAD-SYM ( IR-CTX:ctx -- )
   0 0 0 0 1 6 0 0 CORRUPT ;

: BAD-LIT ( IR-CTX:ctx -- )
   0 0 0 0 1 0 3 0 CORRUPT ;

: BAD-ORG ( IR-CTX:ctx -- )
   0 0 0 0 1 0 0 4 CORRUPT ;

: RUN-OK ( -- )     BND [: OK-ROW ;]   IR-CTX:WITH-CONTEXT ;
: RUN-KIND ( -- )   BND [: BAD-KIND ;] IR-CTX:WITH-CONTEXT ;
: RUN-MODE ( -- )   BND [: BAD-MODE ;] IR-CTX:WITH-CONTEXT ;
: RUN-SPAN ( -- )   BND [: BAD-SPAN ;] IR-CTX:WITH-CONTEXT ;
: RUN-SRC ( -- )    BND [: BAD-SRC ;]  IR-CTX:WITH-CONTEXT ;
: RUN-SYM ( -- )    BND [: BAD-SYM ;]  IR-CTX:WITH-CONTEXT ;
: RUN-LIT ( -- )    BND [: BAD-LIT ;]  IR-CTX:WITH-CONTEXT ;
: RUN-ORG ( -- )    BND [: BAD-ORG ;]  IR-CTX:WITH-CONTEXT ;

: CHECK-CASES ( -- )
   \ positive control: the same hand-written row, with every field legal,
   \ passes - so the eight rejections below fail for their stated reason
   s" a hand-written row whose every field is legal passes CHECK" T-LABEL
   RUN-OK
   s" a stored kind outside the vocabulary is refused" T-LABEL
   [: RUN-KIND ;] E-NTAPE-KIND TTHROWSQ
   s" a stored parser mode outside the vocabulary is refused" T-LABEL
   [: RUN-MODE ;] E-NTAPE-MODE TTHROWSQ
   s" a stored span crossing its source's end is refused" T-LABEL
   [: RUN-SPAN ;] E-IR-SRC-SPAN TTHROWSQ
   s" a stored span naming an unregistered source is refused" T-LABEL
   [: RUN-SRC ;] E-IR-SRC-BOUND TTHROWSQ
   s" a stored spelling the interner never minted is refused" T-LABEL
   [: RUN-SYM ;] E-IR-SYM-BOUND TTHROWSQ
   s" a stored literal on a kind that has none is refused" T-LABEL
   [: RUN-LIT ;] E-NTAPE-LITERAL TTHROWSQ
   s" a stored origin that is not below its own row is refused" T-LABEL
   [: RUN-ORG ;] E-NTAPE-STATE TTHROWSQ ;

\ ---- the shared digest -------------------------------------------------------
\ A three-token tape, built to order, so a caller can vary exactly one field.
\ mode2 and lit2 belong to the second token; ord decides whether the second and
\ third tokens are appended in order or swapped.
: BUILD ( IR-CTX:ctx NTAPE:mode n n n -- IR-ARENA:view )
   {: c:IR-CTX:ctx m2:NTAPE:mode l2:n st3:n ord:n :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sp sy key s" SQUARE" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c sp sy key s" DUP" IR-SYM:INTERN {: n1:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:INTERPRETING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   ord 0 = if
      c tp sr sy
         sr s0 2 3 IR-SOURCE:SPAN n1 m2 l2 NTAPE:INT-TOKEN
         NTAPE:PUSH drop
      c tp sr sy
         sr s0 st3 2 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
         NTAPE:PUSH drop
   else
      c tp sr sy
         sr s0 st3 2 IR-SOURCE:SPAN n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
         NTAPE:PUSH drop
      c tp sr sy
         sr s0 2 3 IR-SOURCE:SPAN n1 m2 l2 NTAPE:INT-TOKEN
         NTAPE:PUSH drop
   then
   tp NTAPE:SEAL ;

: BASE ( IR-CTX:ctx -- CDIGEST:digest )
   NTAPE-MODE:COMPILING 7 5 0 BUILD NTAPE:DIGEST ;

\ The same three tokens again, with the knobs BUILD does not have: the third
\ token's kind, spelling, expansion origin, source, and span length. Every
\ selector at zero builds exactly BUILD's base tape, which is one of the
\ clauses below - so a preimage slot that stopped being read would show up as
\ two tapes that differ agreeing, not as a fixture drifting apart.
: BUILD-B ( IR-CTX:ctx n n n n n -- IR-ARENA:view )
   {: c:IR-CTX:ctx k3:n p3:n o3:n r3:n l3:n :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   c sr key SRC {: s0:IR-ID:ir-source-id :}
   c sr key TEXT IR-SOURCE:REGISTER {: s1:IR-ID:ir-source-id :}
   c sp sy key s" SQUARE" IR-SYM:INTERN {: n0:IR-ID:ir-symbol-id :}
   c sp sy key s" DUP" IR-SYM:INTERN {: n1:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN n0 NTAPE-MODE:INTERPRETING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 2 3 IR-SOURCE:SPAN n1 NTAPE-MODE:COMPILING 7 NTAPE:INT-TOKEN
      NTAPE:PUSH drop
   r3 0 = if s0 else s1 then {: src:IR-ID:ir-source-id :}
   p3 0 = if n0 else n1 then {: spl:IR-ID:ir-symbol-id :}
   l3 0 = if 2 else 3 then {: ln:n :}
   \ name and string-literal carry the same spelling, span and empty literal,
   \ so this knob moves the stored kind and nothing else
   c tp sr sy
   k3 0 = if
      sr src 5 ln IR-SOURCE:SPAN spl NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
   else
      sr src 5 ln IR-SOURCE:SPAN spl NTAPE-MODE:COMPILING NTAPE:STRING-TOKEN
   then
   o3 0 = if NTAPE:PUSH else 0 NTAPE:PUSH-FROM then
   drop
   tp NTAPE:SEAL ;

\ Two structurally identical tapes, in two different modules of one context,
\ digest identically: the module serial is allocated per process and is
\ deliberately outside the preimage, or no cached result could outlive a run.
: DG-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c BASE CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   c BASE  w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c NTAPE-MODE:INTERPRETING 7 5 0 BUILD NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c NTAPE-MODE:COMPILING 8 5 0 BUILD NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c NTAPE-MODE:COMPILING 7 6 0 BUILD NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c NTAPE-MODE:COMPILING 7 5 1 BUILD NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c BASE  w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ ;

: DG-CASE ( -- )
   s" the digest is the tape and only the tape" T-LABEL
   BND [: DG-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE TFALSE TFALSE TFALSE TTRUE ;

\ The remaining preimage fields, one clause each: the third token's kind,
\ spelling, expansion origin, source, and span length all move the digest, and
\ the all-zero build is the same tape BUILD's base is.
: DGB-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c 0 0 0 0 0 BUILD-B NTAPE:DIGEST
   CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   c BASE  w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c 1 0 0 0 0 BUILD-B NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c 0 1 0 0 0 BUILD-B NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c 0 0 1 0 0 BUILD-B NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c 0 0 0 1 0 BUILD-B NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c 0 0 0 0 1 BUILD-B NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ ;

: DGB-CASE ( -- )
   s" kind, spelling, origin, source and length each move the digest" T-LABEL
   BND [: DGB-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE TFALSE TFALSE TFALSE TTRUE ;

\ An empty tape still has a digest, and it is not any non-empty tape's.
: DGE-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 4 NTAPE:NEW NTAPE:SEAL NTAPE:DIGEST
   CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   c IR-CTX:NEW-MODULE drop {: k2:IR-ID:ir-module-key :}
   c k2 4 NTAPE:NEW NTAPE:SEAL NTAPE:DIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   c BASE  w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ ;

: DGE-CASE ( -- )
   s" an empty tape has its own stable digest" T-LABEL
   BND [: DGE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

\ VERIFY is the contract the stages share: a stage records the digest of the
\ tape it read and hands it back. The tape it was given must be that tape.
: VF-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c NTAPE-MODE:COMPILING 7 5 0 BUILD {: v:IR-ARENA:view :}
   v v NTAPE:DIGEST NTAPE:VERIFY ;

: VF-CASE ( -- )
   s" a stage that hands back the digest it recorded is accepted" T-LABEL
   BND [: VF-BODY ;] IR-CTX:WITH-CONTEXT ;

: VFX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c BASE CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   c NTAPE-MODE:INTERPRETING 7 5 0 BUILD
   w0 w1 w2 w3 CDIGEST-DIGEST:MAKE NTAPE:VERIFY ;

: VFX ( -- )
   BND [: VFX-BODY ;] IR-CTX:WITH-CONTEXT ;

: VFX-CASE ( -- )
   s" a digest from another tape is refused" T-LABEL
   [: VFX ;] E-NTAPE-DIGEST TTHROWSQ ;

\ ---- a tape of a module that is still being built -----------------------------
\ src/compiler/ir/build.f holds a module's source registry and symbol interner
\ privately, so a tape of such a module cannot be handed them. It asks the
\ builder the same two questions instead. This is the join the elaborator needs:
\ one module whose source tape and whose IR are built together rather than two
\ modules that merely agree, and the proof is that the tape passes its own
\ structural CHECK against that module's frozen registries afterwards - the same
\ CHECK a tape built the other way passes.
: BPLAN ( -- )
   IR-BUILD:PLAN-BEGIN
   16 256 IR-BUILD:PLAN-SYMBOLS
   8 64 IR-BUILD:PLAN-TYPES
   8 64 IR-BUILD:PLAN-ATTRS
   4 IR-BUILD:PLAN-SOURCES
   8 64 IR-BUILD:PLAN-SCHEMAS
   8 8 64 IR-BUILD:PLAN-OPS
   4 4 64 IR-BUILD:PLAN-FUNS ;

: BMOD ( IR-CTX:ctx n -- IR-BUILD:builder IR-ARENA:arena )
   {: c:IR-CTX:ctx cap:n :}
   BPLAN
   c s" hir" 1 0 IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   b  c b IR-BUILD:MODULE-KEY cap NTAPE:NEW ;

: BJ-BODY ( IR-CTX:ctx -- n n bool bool )
   {: c:IR-CTX:ctx :}
   c 8 BMOD {: b:IR-BUILD:builder tp:IR-ARENA:arena :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b TEXT IR-BUILD:ADD-SOURCE {: s0:IR-ID:ir-source-id :}
   c b s" SQUARE" IR-BUILD:INTERN-SYMBOL {: n0:IR-ID:ir-symbol-id :}
   c b s" 2" IR-BUILD:INTERN-SYMBOL {: n1:IR-ID:ir-symbol-id :}
   c b tp
      b s0 2 6 IR-BUILD:ADD-SPAN n0 NTAPE-MODE:INTERPRETING NTAPE:NAME-TOKEN
      NTAPE:PUSH-INTO {: t0:n :}
   c b tp
      b s0 9 1 IR-BUILD:ADD-SPAN n1 NTAPE-MODE:COMPILING 2 NTAPE:INT-TOKEN
      NTAPE:PUSH-INTO drop
   c b tp
      b s0 9 1 IR-BUILD:ADD-SPAN n1 NTAPE-MODE:COMPILING 2 NTAPE:INT-TOKEN
      t0 NTAPE:PUSH-INTO-FROM drop
   tp NTAPE:SEAL {: v:IR-ARENA:view :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   v key m IR-BUILD:FSOURCES m IR-BUILD:FSYM-ROWS NTAPE:CHECK
   v NTAPE:TOKENS
   v 2 NTAPE:ORIGIN@
   v key 1 NTAPE:SPELL@ IR-ID:SYMBOL-LOCAL n1 IR-ID:SYMBOL-LOCAL =
   v 1 NTAPE:KIND@ NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ ;

: BJ-CASE ( -- )
   s" a tape built through a live builder checks against that module" T-LABEL
   BND [: BJ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 3 T= ;

\ The span is assembled through the open generated constructor, which is the
\ only way to name bytes outside a registered source.
: BJ-SPAN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 BMOD {: b:IR-BUILD:builder tp:IR-ARENA:arena :}
   c b TEXT IR-BUILD:ADD-SOURCE {: s0:IR-ID:ir-source-id :}
   c b s" X" IR-BUILD:INTERN-SYMBOL {: n0:IR-ID:ir-symbol-id :}
   c b tp
      s0 5 99 IR--SOURCE-SPAN:MAKE n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH-INTO drop ;

: BJ-SPAN ( -- )
   BND [: BJ-SPAN-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A spelling of this module whose ordinal its interner never minted. It carries
\ the right owning module, so only the interner itself can refuse it.
: BJ-GHOST-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 BMOD {: b:IR-BUILD:builder tp:IR-ARENA:arena :}
   c b TEXT IR-BUILD:ADD-SOURCE {: s0:IR-ID:ir-source-id :}
   c b tp
      b s0 0 1 IR-BUILD:ADD-SPAN
      b IR-BUILD:MODULE-KEY b IR-BUILD:SYMBOLS IR-ID:PACK-SYMBOL
      NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH-INTO drop ;

: BJ-GHOST ( -- )
   BND [: BJ-GHOST-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Once the module is published its tape cannot grow behind it.
: BJ-FROZEN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 BMOD {: b:IR-BUILD:builder tp:IR-ARENA:arena :}
   c b TEXT IR-BUILD:ADD-SOURCE {: s0:IR-ID:ir-source-id :}
   c b s" X" IR-BUILD:INTERN-SYMBOL {: n0:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE drop
   c b tp
      s0 0 1 IR--SOURCE-SPAN:MAKE n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH-INTO drop ;

: BJ-FROZEN ( -- )
   BND [: BJ-FROZEN-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Appending to the tape of a live module is a use of that module, so it proves
\ the caller owns the compilation exactly as an append to the module does.
: BJ-XC-INNER ( IR-BUILD:builder IR-ARENA:arena IR-ID:ir-source-id IR-ID:ir-symbol-id IR-CTX:ctx -- )
   {: b:IR-BUILD:builder tp:IR-ARENA:arena s0:IR-ID:ir-source-id
      n0:IR-ID:ir-symbol-id c2:IR-CTX:ctx :}
   c2 b tp
      s0 0 1 IR--SOURCE-SPAN:MAKE n0 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH-INTO drop ;

: BJ-XC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 4 BMOD {: b:IR-BUILD:builder tp:IR-ARENA:arena :}
   c b TEXT IR-BUILD:ADD-SOURCE {: s0:IR-ID:ir-source-id :}
   c b s" X" IR-BUILD:INTERN-SYMBOL {: n0:IR-ID:ir-symbol-id :}
   b tp s0 n0
   BND [: BJ-XC-INNER ;] IR-CTX:WITH-CONTEXT ;

: BJ-XC ( -- )
   BND [: BJ-XC-BODY ;] IR-CTX:WITH-CONTEXT ;

: BJ-REFUSE-CASES-A ( -- )
   s" a span outside its source cannot enter a live module's tape" T-LABEL
   [: BJ-SPAN ;] E-IR-SRC-SPAN TTHROWSQ
   s" a spelling the module's interner never minted is refused" T-LABEL
   [: BJ-GHOST ;] E-IR-SYM-BOUND TTHROWSQ ;

: BJ-REFUSE-CASES-B ( -- )
   s" a published module's tape cannot be appended to" T-LABEL
   [: BJ-FROZEN ;] E-IR-BUILD-FROZEN TTHROWSQ
   s" appending with a foreign live context rejects" T-LABEL
   [: BJ-XC ;] E-IR-BUILD-OWNER TTHROWSQ ;

\ ---- teardown ----------------------------------------------------------------
: TD-BODY ( IR-CTX:ctx -- IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c 8 MOD-NEW
   {: key:IR-ID:ir-module-key sr:IR-ARENA:arena
      sp:IR-ARENA:arena sy:IR-ARENA:arena tp:IR-ARENA:arena :}
   tp ;

: TD ( -- )
   BND [: TD-BODY ;] IR-CTX:WITH-CONTEXT NTAPE:PUSHED drop ;

: TD-CASE ( -- )
   s" a tape is dead after its context ends" T-LABEL
   [: TD ;] E-IR-ARENA-STALE TTHROWSQ ;

\ ---- the checker keeps the identities and the API sealed ---------------------
: CHECKER-CASES ( -- )
   \ positive control: a well-typed candidate over the same surface certifies,
   \ so the rejections below fail for their stated reason, not a harness typo
   s" NTPOS ( IR-ARENA:view -- n ) NTAPE:TOKENS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" NTKIND-FORGE ( n -- NTAPE:kind )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NTMODE-FORGE ( n -- NTAPE:mode )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NTTOK-CELL ( n n n n n -- NTAPE:token ) NTAPE-TOKEN:MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NTPUSH-CTXLESS ( IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena NTAPE:token -- n ) NTAPE:PUSH"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NTLIT-BARE ( IR-ARENA:view -- n ) 0 NTAPE:LIT@"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" NTSEAL-LIVE ( IR-ARENA:view -- IR-ARENA:view ) NTAPE:SEAL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NTINTO-CTXLESS ( IR-BUILD:builder IR-ARENA:arena NTAPE:token -- n ) NTAPE:PUSH-INTO"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NTINTO-FROZEN ( IR-CTX:ctx IR-BUILD:module IR-ARENA:arena NTAPE:token -- n ) NTAPE:PUSH-INTO"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" NTINTO-ARENAS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena NTAPE:token -- n ) NTAPE:PUSH-INTO"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Throw-through fixtures run inside a harness context, so a context aborted by
\ a throw is reclaimed when that harness exits rather than lingering for the
\ rest of the process. A module here owns four arenas - source registry, symbol
\ pool, symbol rows, tape - against a sixty-four slot registry, so the cases
\ run in several harness contexts instead of one: each harness exit lets the
\ next arena the suite creates sweep the aborted contexts of the group before
\ it. One harness for everything runs the registry out of slots.
: GROUP-RECORD ( IR-CTX:ctx -- )
   drop
   RB-CASE
   TK-CASE
   LIT-CASES ;

: GROUP-OWNER ( IR-CTX:ctx -- )
   drop
   SPAN-CASES
   OWNER-CASES ;

: GROUP-LIFECYCLE ( IR-CTX:ctx -- )
   drop
   OG-CASE
   OG-REJECT-CASES
   BOUND-CASES
   SEAL-CASE
   STATE-CASES ;

: GROUP-CHECK ( IR-CTX:ctx -- )
   drop
   CHECK-CASES ;

: GROUP-DIGEST ( IR-CTX:ctx -- )
   drop
   DG-CASE
   DGE-CASE ;

: GROUP-DIGEST-FIELDS ( IR-CTX:ctx -- )
   drop
   DGB-CASE ;

: GROUP-VERIFY ( IR-CTX:ctx -- )
   drop
   VF-CASE
   VFX-CASE ;

: GROUP-BUILT ( IR-CTX:ctx -- )
   drop
   BJ-CASE ;

: GROUP-BUILT-REFUSE-A ( IR-CTX:ctx -- )
   drop
   BJ-REFUSE-CASES-A ;

: GROUP-BUILT-REFUSE-B ( IR-CTX:ctx -- )
   drop
   BJ-REFUSE-CASES-B ;

public

: RUN ( -- )
   T-RESET
   BND [: GROUP-RECORD ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-OWNER ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-LIFECYCLE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-CHECK ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-DIGEST ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-DIGEST-FIELDS ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-VERIFY ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-BUILT ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-BUILT-REFUSE-A ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-BUILT-REFUSE-B ;] IR-CTX:WITH-CONTEXT
   TORN-CASE
   TD-CASE
   CHECKER-CASES
   T-REPORT ;

;package

NTAPE-TEST:RUN
