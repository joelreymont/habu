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

package CERTIFY-DOES-DEFINER

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

: MAIN ( -- )
   T-RESET
   CDD-SECTION-DEFINER
   CDD-SECTION-WRAPPER
   CDD-SECTION-NOT-A-WRAPPER
   CDD-SECTION-LIVE
   T-REPORT ;

MAIN

;package
