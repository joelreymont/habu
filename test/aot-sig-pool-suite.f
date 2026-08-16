\ aot-sig-pool-suite.f - the signature pool's two DATA cells: where they sit,
\ and that every mirrored copy of an engine-layout offset still agrees.
\
\ WHY A MIRROR EXISTS AT ALL. src/core/checker.f loads BEFORE src/habu/layout.f
\ in every host that has both - the engine's own cold prefix (src/habu/habu2.f
\ PFX-PATH-CHECKER-FILES ahead of PFX-PATH-CORE-FILES) and the metabuild host
\ (tools/build-fixpoint.f BF-APPEND-RUN-PRELUDE ahead of BF-APPEND-COMMON) -
\ while layout.f and habu2.f also compile in the gforth recovery host, where
\ checker.f never loads. So neither file can be the sole owner of an offset both
\ need, and the checker restates the number with a comment naming layout's
\ constant. There were two such pairs, both prose-only; the signature pool adds a
\ third and a fourth, and prose is not a check.
\
\ WHAT THIS FILE ASSERTS, AND WHY IT IS NOT A NAME SEARCH. The four CK-*-OFF
\ constants are NOT reachable from checked code - checker.f's own definitions
\ compile with the hook off, so a checked caller has no record to resolve against
\ and only the words carrying a PRIM: axiom can be named. Adding four axioms so a
\ test could read four literals would grow the checker's public surface for a
\ test's convenience. So the checker's side is read where it is DEFINED: the
\ source, through tools/lint/source-lex.f - the same lexer package-diff-lint and
\ schedule-lint read Habu with - and only a `<number> constant <NAME>` whose
\ three tokens are all real code tokens counts. The layout side is read from the
\ LIVE ENGINE, by naming the constants, which is the authority a compiled
\ reference would use.
\
\ THE FIXTURES ARE BUILT TO FOOL A TEXT MATCHER. The lexer drops `\` line
\ comments, `( ... )` bodies and string payloads, so a synthetic source carrying
\ the right spelling in a comment, in a string, in the wrong role (the name where
\ the number goes), or defining a decoy `constant` of the same name with a
\ different number must NOT satisfy the reader. Each of those is a case below.
\ Without them "the file contains $47D0" would pass for "the checker mirrors
\ $47D0", which is the thing this file exists to refuse.
\
\ THE SECOND HALF IS THE BAND. Two cells were taken out of the unclaimed run
\ above the evaluator frames, and their claim is only sound while they stay in
\ it, stay a cell apart, stay below the ceiling a `DATA <off> LDR` can address
\ and below DATA-START, and collide with no other cell named in that band. Every
\ one of those is a statement about src/habu/layout.f that an ordinary edit
\ falsifies, and every name is live in a booted engine.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/aot-sig-pool-suite.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/layout.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package AOT-SIG-POOL-TEST
private

: CHECKER$ ( -- ptr u8 n ) s" src/core/checker.f" ;

\ The ceiling src/habu/layout.f measures for a cell a compiled routine names
\ directly: `DATA <off> LDR` is a 12-bit immediate scaled by eight.
$7FF8 constant LDR-CEILING

\ ---- reading a `<number> constant <NAME>` out of a source, structurally ------
\ ANSWERS THE FIRST ONE and refuses a second: two definitions of one constant is
\ two authorities, and taking either would hide the other.

variable HITS                      \ how many definitions the walk saw
variable VALUE                     \ the number the first one carried

: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: TOK= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= if LINT-FALSE exit then
   k LINT-LEX:TOKEN a u STR= ;

\ The engine's own number reader, so a spelling this test accepts is a spelling
\ the compiler accepts. It answers the value, whether that value is a DOUBLE's
\ bits, and whether the bytes were a number at all - in that order, which is
\ worth writing down: reading the middle flag as validity makes every literal
\ answer "not a number" and the whole scan silently return nothing.
: NUM ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u num-parse {: v:n dbl:bool ok:bool :}
   ok 0= if 0 LINT-FALSE exit then
   dbl if 0 LINT-FALSE exit then      \ a double's bits are not a cell offset
   v LINT-TRUE ;

: SCAN-FOR ( ptr u8 n -- ) {: na:ptr nu:n :}
   0 HITS !  0 VALUE !
   LINT-LEX:COUNT 2 - 0 ?do
      i 1 + s" constant" TOK= if
         i 2 + na nu TOK= if
            i WORD? if
               i LINT-LEX:TOKEN NUM {: v:n ok:bool :}
               ok if
                  HITS @ 0= if v VALUE ! then
                  HITS @ 1 + HITS !
               then
            then
         then
      then
   loop ;

\ The one question the mirror asks, and the three ways it can fail.
: ?MIRROR ( ptr u8 n n -- ) {: na:ptr nu:n want:n :}
   na nu SCAN-FOR
   s" the checker defines the mirrored constant exactly once" T-LABEL
   HITS @ 1 T=
   s" ... and its number is the layout constant it names" T-LABEL
   VALUE @ want T= ;

: LOAD-CHECKER ( -- )
   CHECKER$ LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   s" the checker source lexes without a diagnostic" T-LABEL
   LINT-LEX:ERROR? 0= TTRUE ;

\ ---- case one: every mirrored pair agrees ------------------------------------
\ Two pairs are the signature pool's; two were already in the tree carrying only
\ a comment, and they are here for the same reason - the drift risk is the same
\ risk, and a check that covered only the new pairs would leave the older ones
\ exactly as unverified as they were.

: MIRROR-CASE ( -- )
   LOAD-CHECKER
   s" CK-AOT-SIG-POOL-OFF" AOT-SIG:POOL-CELL ?MIRROR
   s" CK-AOT-SIG-LEN-OFF"  AOT-SIG:LEN-CELL  ?MIRROR
   s" CK-SEAL-LATCH-OFF"   FRIEND-LATCH-CELL ?MIRROR
   s" CK-USE-DEPTH-OFF"    USE-DEPTH-CELL    ?MIRROR ;

\ ---- case two: the reader cannot be fooled -----------------------------------
\ Synthetic sources through the SAME entry points the live read uses, each built
\ so that a text search would answer and a structural read must not.

\ Every fixture asserts the lexer's own health before its count is believed. A
\ source that does not lex is not a fixture that says anything, and - measured -
\ a source that leaves the lexer holding UNTERMINATED-QUOTE changes what the NEXT
\ source counts, which is how a poisoned neighbour looks like a scanner bug.
: FIXTURE ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n na:ptr nu:n :}
   sa su LINT-LEX:SOURCE
   s" the fixture lexes without a diagnostic" T-LABEL
   LINT-LEX:ERROR? 0= TTRUE
   na nu SCAN-FOR ;

: DECOY-CASE ( -- )
   s\" \\ $1111 constant CK-DECOY-OFF\n: X ( $2222 constant CK-DECOY-OFF ) ;\n"
   s" CK-DECOY-OFF" FIXTURE
   s" a definition inside a line comment or a paren body is not a definition" T-LABEL
   HITS @ 0 T=

   s\" : Y ( -- ptr u8 n ) s\" $3333 constant CK-DECOY-OFF\" ;\n"
   s" CK-DECOY-OFF" FIXTURE
   s" ... and neither is one inside a string literal" T-LABEL
   HITS @ 0 T=

   s\" CK-DECOY-OFF constant $4444\n"
   s" CK-DECOY-OFF" FIXTURE
   s" the name and the number in the wrong roles do not define it" T-LABEL
   HITS @ 0 T=

   s\" CK-DECOY-OFF constant CK-DECOY-OFF\n"
   s" CK-DECOY-OFF" FIXTURE
   s" a non-numeric token in the value position does not define it" T-LABEL
   HITS @ 0 T=

   s\" $47D0 constant CK-DECOY-OFF\n"
   s" CK-DECOY-OFF" FIXTURE
   s" a real definition IS read, and reads as its own number" T-LABEL
   HITS @ 1 T=
   VALUE @ $47D0 T=

   s\" $47D0 constant CK-DECOY-OFF\n$5000 constant CK-DECOY-OFF\n"
   s" CK-DECOY-OFF" FIXTURE
   s" two definitions of one constant are two authorities and are refused" T-LABEL
   HITS @ 2 T=

   s\" $47D0 constant CK-DECOY-OFFSET\n"
   s" CK-DECOY-OFF" FIXTURE
   s" a longer name that merely starts with the wanted one is a different name" T-LABEL
   HITS @ 0 T= ;

\ ---- case three: the band the two cells were taken out of --------------------
\ PROT:RHI and PROT:CF took the first two cells of the unclaimed run between the
\ evaluator frames' end and the lowering transaction state; these are the next
\ two, and each clause below is why taking them was legal.

: BAND-CASE ( -- )
   s" the pool cell and the length cell are distinct" T-LABEL
   AOT-SIG:POOL-CELL AOT-SIG:LEN-CELL <> TTRUE

   s" ... and adjacent, one cell apart, in that order" T-LABEL
   AOT-SIG:LEN-CELL AOT-SIG:POOL-CELL - CELL T=

   s" they sit above the two cells PROT already took from this run" T-LABEL
   AOT-SIG:POOL-CELL PROT:CF > TTRUE
   PROT:CF PROT:RHI > TTRUE

   s" ... and below the lowering transaction state that ends the run" T-LABEL
   AOT-SIG:LEN-CELL CELL + TXN-STATE-OFF <= TTRUE

   s" both are addressable by the `DATA <off> LDR` form the band bodies use" T-LABEL
   AOT-SIG:LEN-CELL LDR-CEILING < TTRUE

   s" both are below DATA-START, so no compiled source can reach them" T-LABEL
   AOT-SIG:LEN-CELL DATA-START < TTRUE

   s" neither collides with another cell this file names" T-LABEL
   AOT-SIG:POOL-CELL PROT:RHI <> TTRUE
   AOT-SIG:POOL-CELL PROT:CF <> TTRUE
   AOT-SIG:POOL-CELL AOT-WINDOW:T0-CELL <> TTRUE
   AOT-SIG:POOL-CELL AOT-WINDOW:D0-CELL <> TTRUE
   AOT-SIG:POOL-CELL AOT-WINDOW:B0-CELL <> TTRUE
   AOT-SIG:POOL-CELL EVALREC-CELL <> TTRUE
   AOT-SIG:POOL-CELL AOT-SEED-DONE-CELL <> TTRUE
   AOT-SIG:LEN-CELL PROT:RHI <> TTRUE
   AOT-SIG:LEN-CELL PROT:CF <> TTRUE
   AOT-SIG:LEN-CELL AOT-WINDOW:T0-CELL <> TTRUE
   AOT-SIG:LEN-CELL AOT-WINDOW:D0-CELL <> TTRUE
   AOT-SIG:LEN-CELL AOT-WINDOW:B0-CELL <> TTRUE
   AOT-SIG:LEN-CELL EVALREC-CELL <> TTRUE
   AOT-SIG:LEN-CELL AOT-SEED-DONE-CELL <> TTRUE

   s" the two cells are cell-aligned, as an atomic read of one requires" T-LABEL
   AOT-SIG:POOL-CELL CELL mod 0 T=
   AOT-SIG:LEN-CELL CELL mod 0 T= ;

public

: RUN ( -- )
   DECOY-CASE
   MIRROR-CASE
   BAND-CASE
   T-REPORT
   s" aot-sig-pool: ok" type cr ;

;package

AOT-SIG-POOL-TEST:RUN
