\ certify-does-definer.f - certification knows what a `does>` definer publishes.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f \
\   src/habu/verify-source.f test/certify-does-definer.f
\
\ A `create ... does>` definition is a DEFINER: every word it creates carries
\ the clause's declared effect, and the engine publishes exactly that row at
\ creation time (src/habu/habu2.f DOESPATCH:EMIT hands the parsed clause
\ signature to LASTC-TRUST:PUBLISH, which registers it through the checker's
\ `trust-raw`). The source pre-verifier never executes a definer, so it has to
\ learn the same row from the text, and until it did, a created word was
\ E-UNDEFINED at its first typed use - measured: `bin/hb tools/check.f
\ lib/queue.f` refused NG-BUFFER (lib/type/deftype.f:60).
\
\ Sections 1-3 measure the learned row through the pre-pass; section 4 runs the
\ same definer for real, so the row the engine publishes and the row the scanner
\ learns are pinned against each other rather than against one authority.
\
\ WHAT IS DELIBERATELY NOT LEARNED. A definer call the body reaches only
\ conditionally, twice, or inside a quotation says nothing about what the
\ enclosing word creates, so nothing is recorded and the created word stays
\ unresolvable. Those negatives are section 3, and they are what keeps the
\ wrapper rule from being a prefix match on "calls something that creates".

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/verify-source.f

\ The resident package definer of section 5, compiled by the ENGINE: its two
\ spellings - qualified, and bare under `using` - are what a source can name it
\ by, and both have to reach the one row.
package CDD-RESP
public
: CDD-RP-D ( n -- ) create , does> ( -- ptr n ) ;
;package

\ An export of that definer: the same xt under another tail, and the tail is a
\ symbol of its own, so what the definer creates has to be copied to it
\ (checker.f EXPORT-META-COPY) or the exported spelling knows nothing.
package CDD-RESX
public
EXPORT CDD-RESP:CDD-RP-D
;package

package CERTIFY-DOES-DEFINER

\ The rest of section 5's fixtures, also compiled by the engine here: this
\ file's scanner never reads their text, so what they create is known only
\ through the store the checker wrote when it certified their clause.
: CDD-RES-D ( n -- ) create , does> ( -- ptr n ) ;
: CDD-RES-W ( -- ) 3 CDD-RES-D ;          \ a RESIDENT wrapper: still unknown, pinned below
: CDD-RES-P ( n -- n ) 1 + ;              \ the plain definition right after a definer

-1 constant ACCEPTED
0 constant REFUSED
1 constant UNRESOLVED

: CDD-VERDICT ( ptr u8 n -- n )
   CHECK-QUIET-CANDIDATE! ;

\ ---- 1. the definer the scanner read, and the word it creates ---------------
: CDD-SECTION-DEFINER ( -- )
   s\" : CDD-D ( n -- ) create , does> ( -- ptr n ) ;\n8 CDD-D CDD-ONE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" the created word certifies as the clause effect" T-LABEL
   s" C1 ( -- ptr n ) CDD-ONE" CDD-VERDICT ACCEPTED T=
   s" the same word is refused against a bare cell" T-LABEL
   s" C2 ( -- n ) CDD-ONE" CDD-VERDICT REFUSED T=
   s" a byte pointee is refused too" T-LABEL
   s" C3 ( -- ptr u8 ) CDD-ONE" CDD-VERDICT REFUSED T=
   s" a name the definer never created stays unresolvable" T-LABEL
   s" C4 ( -- ptr n ) CDD-TWO" CDD-VERDICT UNRESOLVED T=
   s" the definer itself keeps its own declared effect" T-LABEL
   s" C5 ( n -- ) CDD-D" CDD-VERDICT ACCEPTED T= ;

\ ---- 2. a package definer, its straight-line wrapper, and both spellings ----
\ `: BUFFER ( n -- ) E-CG-CAP E-CG-VALUE BUFFER-E ;` (lib/codegen.f) is the
\ production shape of the wrapper: a body whose tokens hold exactly one definer
\ call and no control flow creates whatever that definer creates.
: CDD-SECTION-WRAPPER ( -- )
   s\" package CDDP\npublic\n: CDD-PD ( n n n -- ) create , , , does> ( -- ptr n ) ;\n: CDD-PW ( n -- ) 3 4 CDD-PD ;\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s\" 8 CDDP:CDD-PD CDD-QUAL\n9 CDDP:CDD-PW CDD-WRAP\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s\" using CDDP\n7 CDD-PW CDD-BARE\n;using\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" the qualified definer's created word certifies" T-LABEL
   s" C6 ( -- ptr n ) CDD-QUAL" CDD-VERDICT ACCEPTED T=
   s" the wrapper's created word certifies the same effect" T-LABEL
   s" C7 ( -- ptr n ) CDD-WRAP" CDD-VERDICT ACCEPTED T=
   s" a bare wrapper call under `using` records the same row" T-LABEL
   s" C8 ( -- ptr n ) CDD-BARE" CDD-VERDICT ACCEPTED T=
   s" the wrapper's created word is refused against a bare cell" T-LABEL
   s" C9 ( -- n ) CDD-WRAP" CDD-VERDICT REFUSED T= ;

\ ---- 3. what a body has to be before it counts as a wrapper ----------------
: CDD-SECTION-NOT-A-WRAPPER ( -- )
   s\" : CDD-CD ( n -- ) create , does> ( -- ptr n ) ;\n: CDD-IFW ( n -- ) dup 0= if drop 1 then CDD-CD ;\n2 CDD-IFW CDD-COND\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer behind a conditional records nothing" T-LABEL
   s" C10 ( -- ptr n ) CDD-COND" CDD-VERDICT UNRESOLVED T=
   s\" : CDD-TWOW ( n n -- ) CDD-CD CDD-CD ;\n3 4 CDD-TWOW CDD-TWICE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" two definer calls in one body record nothing" T-LABEL
   s" C11 ( -- ptr n ) CDD-TWICE" CDD-VERDICT UNRESOLVED T=
   s\" : CDD-QW ( n -- ) drop [: 5 CDD-CD ;] drop ;\n6 CDD-QW CDD-QUOT\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer inside a quotation records nothing" T-LABEL
   s" C12 ( -- ptr n ) CDD-QUOT" CDD-VERDICT UNRESOLVED T= ;

\ ---- 4. the learned row is the row the engine publishes ---------------------
\ The same definition, compiled and run by the engine: CDD-LIVE-ONE's effect
\ here comes from DOESPATCH:EMIT, not from the scanner, and the two agree on
\ both the acceptance and the refusal.
: CDD-LIVE-D ( n -- ) create , does> ( -- ptr n ) ;
8 CDD-LIVE-D CDD-LIVE-ONE

: CDD-LIVE-READ ( -- n ) CDD-LIVE-ONE @ ;

: CDD-SECTION-LIVE ( -- )
   s" the engine's own row certifies the clause effect" T-LABEL
   s" C13 ( -- ptr n ) CDD-LIVE-ONE" CDD-VERDICT ACCEPTED T=
   s" and refuses the bare cell the scanner refuses" T-LABEL
   s" C14 ( -- n ) CDD-LIVE-ONE" CDD-VERDICT REFUSED T=
   s" the created word holds what the definer stored" T-LABEL
   CDD-LIVE-READ 8 T= ;

\ ---- 5. a definer the scanner never read ------------------------------------
\ CDD-RES-D and CDD-RESP:CDD-RP-D above were compiled by the engine in this
\ process: no text of theirs ever reached the scanner, and before the checker
\ kept what a certified clause creates, a source using one of them left its
\ created word E-UNDEFINED at the first typed use - measured, `bin/hb
\ tools/check.f lib/queue.f` refused NG-BUFFER, created at lib/type/deftype.f:61
\ by the resident CODEGEN:BUFFER-E.
\
\ THE LAST ROWS ARE THE LATCH'S LIFETIME, and they are here because the store is
\ written between two checker entry points rather than by one of them. The
\ engine checks a clause and then the definer's own body; the scanner checks the
\ body and then the clause. Either order must leave a plain definition with
\ nothing, and a refused clause must leave nothing at all. The refusal row makes
\ the engine print its own `does> at <file>:1` line on stderr before it throws:
\ that line IS the refusal being measured, not a test failure.
TRUSTED: CDD-EVAL ( ptr u8 n -- ) evaluate ;

: CDD-SECTION-RESIDENT ( -- )
   s\" 8 CDD-RES-D CDD-RES-ONE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident definer's created word certifies as the clause effect" T-LABEL
   s" C15 ( -- ptr n ) CDD-RES-ONE" CDD-VERDICT ACCEPTED T=
   s" and is refused against a bare cell" T-LABEL
   s" C16 ( -- n ) CDD-RES-ONE" CDD-VERDICT REFUSED T=
   s\" 8 CDD-RESP:CDD-RP-D CDD-RP-QUAL\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s\" using CDD-RESP\n7 CDD-RP-D CDD-RP-BARE\n;using\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" the qualified spelling of a resident package definer certifies" T-LABEL
   s" C17 ( -- ptr n ) CDD-RP-QUAL" CDD-VERDICT ACCEPTED T=
   s" the bare spelling under `using` names the same definer" T-LABEL
   s" C18 ( -- ptr n ) CDD-RP-BARE" CDD-VERDICT ACCEPTED T=
   s\" 9 CDD-RESX:CDD-RP-D CDD-RP-EXPORTED\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" an exported definer creates what the definer creates" T-LABEL
   s" C24 ( -- ptr n ) CDD-RP-EXPORTED" CDD-VERDICT ACCEPTED T=
   s\" : CDD-RES-WRAP ( -- ) 3 CDD-RES-D ;\nCDD-RES-WRAP CDD-RES-WRAPMADE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a source-read wrapper of a resident definer creates what it creates" T-LABEL
   s" C19 ( -- ptr n ) CDD-RES-WRAPMADE" CDD-VERDICT ACCEPTED T=
   s\" CDD-RES-W CDD-RES-WMADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a RESIDENT wrapper is still unknown (the next worker's row)" T-LABEL
   s" C20 ( -- ptr n ) CDD-RES-WMADE" CDD-VERDICT UNRESOLVED T= ;

: CDD-SECTION-LATCH ( -- )
   s\" 5 CDD-RES-P CDD-PL-MADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" engine order: the definition after a definer creates nothing" T-LABEL
   s" C21 ( -- ptr n ) CDD-PL-MADE" CDD-VERDICT UNRESOLVED T=
   s\" : CDD-SO-D ( n -- ) create , does> ( -- ptr n ) ;\n: CDD-SO-P ( n -- n ) 1 + ;\n5 CDD-SO-P CDD-SO-MADE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" source order: the definition after a definer creates nothing" T-LABEL
   s" C22 ( -- ptr n ) CDD-SO-MADE" CDD-VERDICT UNRESOLVED T=
   s" a clause its body contradicts is refused" T-LABEL
   [: s\" : CDD-RES-BAD ( n -- ) create , does> ( -- n ) ;\n" CDD-EVAL ;] 70 TTHROWSQ
   s\" : CDD-RES-AFTER ( n -- n ) 2 * ;\n" CDD-EVAL
   s\" 6 CDD-RES-AFTER CDD-AFTER-MADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" and leaves nothing for the next definition to inherit" T-LABEL
   s" C23 ( -- ptr n ) CDD-AFTER-MADE" CDD-VERDICT UNRESOLVED T= ;

: MAIN ( -- )
   T-RESET
   CDD-SECTION-DEFINER
   CDD-SECTION-WRAPPER
   CDD-SECTION-NOT-A-WRAPPER
   CDD-SECTION-LIVE
   CDD-SECTION-RESIDENT
   CDD-SECTION-LATCH
   T-REPORT ;

MAIN

;package
