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
\ learns are pinned against each other rather than against one authority. Section
\ 8 asks both of a `TRUSTED:` definer, whose body is asserted but whose clause is
\ still the declaration both paths record.
\
\ Sections 9-12 are the definers no clause describes: one that writes its word
\ as text and loads it through INCLUDE-EVALUATE (lib/process-command.f COMMAND,
\ lib/task.f +USER), and lib/ffi-abi.f FUNCTION:, whose word comes from its
\ declaration group. A `generates: D ( effect )` row states what D makes.
\ Section 9 reads a row from source and from a resident package and pins it
\ against the word the engine really generates; section 10 reads FUNCTION:'s
\ group; section 11 holds a row to the checker scope that recorded it; section
\ 12 is the engine refusing a row. A refused row prints its E-GENERATES-ROW line
\ on stderr before it throws, as section 5's refused clause prints its own.
\
\ WHAT IS DELIBERATELY NOT LEARNED. A definer call the body reaches only
\ conditionally, twice, or inside a quotation says nothing about what the
\ enclosing word creates, so nothing is recorded and the created word is never
\ accepted. Those negatives are section 3 and the last three rows of section 5,
\ and they are what keeps the wrapper rule from being a prefix match on "calls
\ something that creates". Such a word still calls `create`, so a top-level
\ statement that runs it marks the wordlist it runs in: the word it creates, and
\ every later name nothing in that wordlist resolves, is left to the run
\ (DEFERRED, src/core/checker.f UNSEEN-COVERS?) rather than UNRESOLVED. MAIN
\ runs the latch and TRUSTED sections, which pin names no definer created as
\ UNRESOLVED, before section 3 marks this file's wordlist.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/codegen.f
require lib/ffi-abi.f
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

\ The text-writing definer of sections 9-12, the shape of lib/process-command.f
\ COMMAND: MAKE builds `: NAME ( -- ptr n ) data-base OFF + ;` for the next name
\ and evaluates it, so its word has no `does>` clause for a checker to read.
package CDD-GEN
$60 constant TEXT-CAP
TEXT-CAP CODEGEN:BUFFER TEXT
public
: MAKE ( n -- )
   {: off:n :}
   parse-name
   {: name:ptr nameu:n :}
   TEXT CODEGEN:RESET
   s" : " TEXT CODEGEN:APPEND-STRING
   name nameu TEXT CODEGEN:APPEND-STRING
   s"  ( -- ptr n ) data-base " TEXT CODEGEN:APPEND-STRING
   off TEXT CODEGEN:APPEND-DECIMAL
   s"  + ;" TEXT CODEGEN:APPEND-STRING
   TEXT CODEGEN:CONTENTS INCLUDE-EVALUATE ;
;package

\ The same definer compiled by the ENGINE with its row, private definer and
\ public wrapper as section 9's sources write them: section 9 reads a source
\ that only calls it, and runs it for real.
package CDD-GQ4
: CDD-GD4 ( n -- ) CDD-GEN:MAKE ;
generates: CDD-GD4 ( -- ptr n )
public
: CDD-GD4 ( n -- ) CDD-GD4 ;
;package

package CERTIFY-DOES-DEFINER

\ The rest of section 5's fixtures, also compiled by the engine here: this
\ file's scanner never reads their text, so what they create is known only
\ through the store the checker wrote when it certified their clause.
: CDD-RES-D ( n -- ) create , does> ( -- ptr n ) ;
: CDD-RES-W ( -- ) 3 CDD-RES-D ;          \ a RESIDENT wrapper: known through the checker's walk
: CDD-RES-P ( n -- n ) 1 + ;              \ the plain definition right after a definer

\ The resident twins of C10-C12: the walk counts definer calls and notes any
\ bend, so these three are wrappers of nothing at all.
: CDD-RES-IFW ( n -- ) dup 0= if drop 1 then CDD-RES-D ;
: CDD-RES-TWOW ( n n -- ) CDD-RES-D CDD-RES-D ;
: CDD-RES-QW ( n -- ) drop [: 5 CDD-RES-D ;] drop ;

\ A resident definer whose CLAUSE calls a definer: its own clause is what it
\ creates, and the clause walk leaves nothing for the next record.
: CDD-RES-CD ( n -- ) create , does> ( -- ) drop 3 CDD-RES-D ;

\ Section 8's resident fixtures: a TRUSTED definer, its straight-line wrapper and
\ the plain definition right after it. A trusted definition's body is asserted,
\ so it has no body check of its own between the clause and the publication that
\ records what it creates - lib/task.f TASK and its public wrapper `: TASK TASK ;`
\ are the production shape.
TRUSTED: CDD-TRES-D ( n -- ) create , does> ( -- ptr n ) ;
: CDD-TRES-W ( -- ) 3 CDD-TRES-D ;
: CDD-TRES-P ( n -- n ) 1 + ;

-1 constant ACCEPTED
0 constant REFUSED
1 constant UNRESOLVED
2 constant DEFERRED            \ a mark leaves the name to the run

\ Two authorities, asked apart. A row the scanner learned is a fact of the
\ certify path: the scan compiles nothing, so the engine holds no record of the
\ word it names, and the probe asks that path (CDD-VERDICT). A word the engine
\ created - sections 4 and 8's live rows - is asked as compiled code asks, by
\ the engine's own lookup (CDD-LIVE-VERDICT).
: CDD-VERDICT ( ptr u8 n -- n )
   VERIFY:CANDIDATE-IN-SCOPE ;

: CDD-LIVE-VERDICT ( ptr u8 n -- n )
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
   s" C5 ( n -- ) CDD-D" CDD-VERDICT ACCEPTED T=
   s\" : CDD-DU ( n -- ) create , DoEs> ( -- ptr n ) ;\n8 CDD-DU CDD-ONE-U\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a mixed-case `DoEs>` opens the clause, as the engine reads it" T-LABEL
   s" C1U ( -- ptr n ) CDD-ONE-U" CDD-VERDICT ACCEPTED T= ;

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
   s" C10 ( -- ptr n ) CDD-COND" CDD-VERDICT DEFERRED T=
   s\" : CDD-IFWU ( n -- ) dup 0= IF drop 1 THEN CDD-CD ;\n2 CDD-IFWU CDD-CONDU\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer behind an uppercase conditional records nothing" T-LABEL
   s" C10U ( -- ptr n ) CDD-CONDU" CDD-VERDICT DEFERRED T=
   s\" : CDD-TWOW ( n n -- ) CDD-CD CDD-CD ;\n3 4 CDD-TWOW CDD-TWICE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" two definer calls in one body record nothing" T-LABEL
   s" C11 ( -- ptr n ) CDD-TWICE" CDD-VERDICT DEFERRED T=
   s\" : CDD-QW ( n -- ) drop [: 5 CDD-CD ;] drop ;\n6 CDD-QW CDD-QUOT\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer inside a quotation records nothing" T-LABEL
   s" C12 ( -- ptr n ) CDD-QUOT" CDD-VERDICT DEFERRED T= ;

\ ---- 4. the learned row is the row the engine publishes ---------------------
\ The same definition, compiled and run by the engine: CDD-LIVE-ONE's effect
\ here comes from DOESPATCH:EMIT, not from the scanner, and the two agree on
\ both the acceptance and the refusal.
: CDD-LIVE-D ( n -- ) create , does> ( -- ptr n ) ;
8 CDD-LIVE-D CDD-LIVE-ONE

: CDD-LIVE-READ ( -- n ) CDD-LIVE-ONE @ ;

: CDD-SECTION-LIVE ( -- )
   s" the engine's own row certifies the clause effect" T-LABEL
   s" C13 ( -- ptr n ) CDD-LIVE-ONE" CDD-LIVE-VERDICT ACCEPTED T=
   s" and refuses the bare cell the scanner refuses" T-LABEL
   s" C14 ( -- n ) CDD-LIVE-ONE" CDD-LIVE-VERDICT REFUSED T=
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
   s" a RESIDENT wrapper creates what its definer creates (the walk's row)" T-LABEL
   s" C20 ( -- ptr n ) CDD-RES-WMADE" CDD-VERDICT ACCEPTED T=
   s\" 2 CDD-RES-IFW CDD-RES-CONDMADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident definer behind a conditional records nothing" T-LABEL
   s" C25 ( -- ptr n ) CDD-RES-CONDMADE" CDD-VERDICT DEFERRED T=
   s\" 3 4 CDD-RES-TWOW CDD-RES-TWICEMADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" two resident definer calls in one body record nothing" T-LABEL
   s" C26 ( -- ptr n ) CDD-RES-TWICEMADE" CDD-VERDICT DEFERRED T=
   s\" 6 CDD-RES-QW CDD-RES-QUOTMADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident definer inside a quotation records nothing" T-LABEL
   s" C27 ( -- ptr n ) CDD-RES-QUOTMADE" CDD-VERDICT DEFERRED T= ;

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

\ ---- 7. the wrapper latch's lifetime ----------------------------------------
\ The wrapper fact is learned by the body WALK, and three walks are not a
\ definition's own body: a candidate, a `does>` clause, and the definer body
\ whose own clause already says what it creates. Each row below is a record
\ published right after one of those walks, and each must be untouched by it.
: CDD-SECTION-WRAP-LATCH ( -- )
   s" a candidate body may be a wrapper shape and still teach nothing" T-LABEL
   s" C28 ( n -- ) CDD-RES-D" CDD-LIVE-VERDICT ACCEPTED T=
   s\" 7 CDD-RES-D CDD-CAND-ONE\nCDD-CAND-ONE CDD-CAND-TWO\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" the next created word certifies as the clause effect" T-LABEL
   s" C29 ( -- ptr n ) CDD-CAND-ONE" CDD-VERDICT ACCEPTED T=
   s" and is no definer itself: the candidate walk left nothing to inherit" T-LABEL
   s" C30 ( -- ptr n ) CDD-CAND-TWO" CDD-VERDICT UNRESOLVED T=
   s\" 5 CDD-RES-CD CDD-CD-MADE\nCDD-CD-MADE CDD-CD-X\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer whose clause calls a definer creates its OWN clause" T-LABEL
   s" C31 ( -- ) CDD-CD-MADE" CDD-VERDICT ACCEPTED T=
   s" and not the row the definer that clause calls creates" T-LABEL
   s" C32 ( -- ptr n ) CDD-CD-MADE" CDD-VERDICT REFUSED T=
   s" what that clause created is no definer either" T-LABEL
   s" C33 ( -- ptr n ) CDD-CD-X" CDD-VERDICT UNRESOLVED T=
   \ The same clause in SOURCE order, which is the order that could inherit: the
   \ pre-pass checks the definer's body first and the clause last, so the next
   \ record published is the created word's (verify-source CREATED-TRUST-NEXT?).
   s\" : CDD-SRC-CD ( n -- ) create , does> ( -- ) drop 3 CDD-RES-D ;\n5 CDD-SRC-CD CDD-SRC-MADE\nCDD-SRC-MADE CDD-SRC-X\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a clause read from source arms nothing the created word takes" T-LABEL
   s" C34 ( -- ptr n ) CDD-SRC-X" CDD-VERDICT UNRESOLVED T= ;

\ ---- 8. a TRUSTED definer, on both paths ------------------------------------
\ `TRUSTED:` changes nothing about what a `does>` clause declares: the engine
\ runs CHECK-DOES! at the `;` of a trusted definition too (habu2.f
\ EM-COMPILE-PUBLISH-TRUSTED, ahead of DEF-TRUST:REGISTER), and the created word
\ gets the clause's row at creation time. What used to be lost was the fact that
\ the DEFINER creates it: the scanner skipped a trusted body blind, and the
\ engine's latch was cleared unread at a trusted publication because such a
\ definition has no body check to step it. Measured before the fix, `tools/check.f`
\ on `TASK:MIN-STACK TASK:TASK T1  : F ( -- ptr n ) T1 ;` refused T1 as
\ E-UNDEFINED with the require (read) and without it (resident).
: CDD-SECTION-TRUSTED ( -- )
   s\" TRUSTED: CDD-TD ( n -- ) create , does> ( -- ptr n ) ;\n5 CDD-TD CDD-TD-ONE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a read trusted definer's created word certifies as the clause effect" T-LABEL
   s" C35 ( -- ptr n ) CDD-TD-ONE" CDD-VERDICT ACCEPTED T=
   s" and is refused against a bare cell" T-LABEL
   s" C36 ( -- n ) CDD-TD-ONE" CDD-VERDICT REFUSED T=
   s\" TRUSTED: CDD-TDU ( n -- ) create , DOES> ( -- ptr n ) ;\n5 CDD-TDU CDD-TDU-ONE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" an uppercase `DOES>` declares a trusted definer's clause too" T-LABEL
   s" C35U ( -- ptr n ) CDD-TDU-ONE" CDD-VERDICT ACCEPTED T=
   s\" : CDD-TDW ( n -- ) CDD-TD ;\n5 CDD-TDW CDD-TD-WRAPPED\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a checked wrapper of a read trusted definer creates the same" T-LABEL
   s" C37 ( -- ptr n ) CDD-TD-WRAPPED" CDD-VERDICT ACCEPTED T=
   s\" : CDD-TD-P ( n -- n ) 1 + ;\n5 CDD-TD-P CDD-TD-PMADE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" the definition after a trusted definer creates nothing" T-LABEL
   s" C38 ( -- ptr n ) CDD-TD-PMADE" CDD-VERDICT UNRESOLVED T=
   s\" 8 CDD-TRES-D CDD-TRES-ONE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident trusted definer's created word certifies the same way" T-LABEL
   s" C39 ( -- ptr n ) CDD-TRES-ONE" CDD-VERDICT ACCEPTED T=
   s" and is refused against a bare cell" T-LABEL
   s" C40 ( -- n ) CDD-TRES-ONE" CDD-VERDICT REFUSED T=
   s\" CDD-TRES-W CDD-TRES-WMADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident wrapper of a trusted definer creates what it creates" T-LABEL
   s" C41 ( -- ptr n ) CDD-TRES-WMADE" CDD-VERDICT ACCEPTED T=
   s\" 5 CDD-TRES-P CDD-TRES-PMADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" and the definition published after it inherits nothing" T-LABEL
   s" C42 ( -- ptr n ) CDD-TRES-PMADE" CDD-VERDICT UNRESOLVED T= ;

\ The same trusted definer run for real, as section 4 runs the checked one: the
\ row here is the engine's own, published at creation time.
8 CDD-TRES-D CDD-TRES-LIVE

: CDD-TRES-READ ( -- n ) CDD-TRES-LIVE @ ;

: CDD-SECTION-TRUSTED-LIVE ( -- )
   s" the trusted definer's created word holds what it stored" T-LABEL
   CDD-TRES-READ 8 T=
   s" and the engine's own row certifies the clause effect" T-LABEL
   s" C43 ( -- ptr n ) CDD-TRES-LIVE" CDD-LIVE-VERDICT ACCEPTED T=
   s" and refuses the bare cell" T-LABEL
   s" C44 ( -- n ) CDD-TRES-LIVE" CDD-LIVE-VERDICT REFUSED T= ;

\ ---- 9. a definer that writes its word as text ------------------------------
\ CDD-GEN:MAKE leaves nothing a scanner can read: its word exists only once the
\ text it builds is evaluated, which the pre-pass never does. The row states the
\ effect on the private definer, and the public wrapper inherits it as section
\ 2's wrapper does. The row may sit before or after `public`: either way the
\ private definer is the only CDD-GD* defined when the row is read.
: CDD-SECTION-GENERATES ( -- )
   s\" package CDD-GQ1\n: CDD-GD1 ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-GD1 ( -- ptr n )\npublic\n: CDD-GD1 ( n -- ) CDD-GD1 ;\n;package\n5 CDD-GQ1:CDD-GD1 CDD-GD1-ONE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a generated word certifies as its row's effect" T-LABEL
   s" C45 ( -- ptr n ) CDD-GD1-ONE" CDD-VERDICT ACCEPTED T=
   s" and is refused against a bare cell" T-LABEL
   s" C46 ( -- n ) CDD-GD1-ONE" CDD-VERDICT REFUSED T=
   s\" package CDD-GQ2\n: CDD-GD2 ( n -- ) CDD-GEN:MAKE ;\npublic\ngenerates: CDD-GD2 ( -- ptr n )\n: CDD-GD2 ( n -- ) CDD-GD2 ;\n;package\n5 CDD-GQ2:CDD-GD2 CDD-GD2-ONE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a row after `public` names the private definer too" T-LABEL
   s" C47 ( -- ptr n ) CDD-GD2-ONE" CDD-VERDICT ACCEPTED T=
   s" C48 ( -- n ) CDD-GD2-ONE" CDD-VERDICT REFUSED T=
   s\" 5 CDD-GQ4:CDD-GD4 CDD-GD4-MADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident row reaches a source that only calls its wrapper" T-LABEL
   s" C49 ( -- ptr n ) CDD-GD4-MADE" CDD-VERDICT ACCEPTED T=
   s" C50 ( -- n ) CDD-GD4-MADE" CDD-VERDICT REFUSED T= ;

\ The resident definer run for real: CDD-GD4-LIVE is the word the evaluated text
\ defines, checked against the effect that text declares, so the row is pinned
\ against the generated word rather than against itself.
8 CDD-GQ4:CDD-GD4 CDD-GD4-LIVE

: CDD-SECTION-GENERATES-LIVE ( -- )
   s" the generated word certifies as the effect its row states" T-LABEL
   s" C51 ( -- ptr n ) CDD-GD4-LIVE" CDD-VERDICT ACCEPTED T=
   s" and refuses the bare cell the row refuses" T-LABEL
   s" C52 ( -- n ) CDD-GD4-LIVE" CDD-VERDICT REFUSED T= ;

\ ---- 10. FUNCTION: ------------------------------------------------------------
\ lib/ffi-abi.f FUNCTION: makes its word from the declaration group, so the
\ pre-pass reads the group as the word's effect, with the one rewrite the
\ declarer applies: an `i32` result is a cell. The live declaration is the pin.
PROCESS-SYMBOLS
FUNCTION: CDD-FN getpid ( -- i32 ) ;FUNCTION

: CDD-SECTION-FUNCTION ( -- )
   s" a declared function certifies as its group, the result a cell" T-LABEL
   s" C53 ( -- n ) CDD-FN" CDD-VERDICT ACCEPTED T=
   s" C54 ( -- ptr n ) CDD-FN" CDD-VERDICT REFUSED T=
   s\" FUNCTION: CDD-FN-READ getpid ( -- i32 ) ;FUNCTION\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" and the pre-pass reads the same effect from the declaration" T-LABEL
   s" C55 ( -- n ) CDD-FN-READ" CDD-VERDICT ACCEPTED T=
   s" C56 ( -- ptr n ) CDD-FN-READ" CDD-VERDICT REFUSED T= ;

\ ---- 11. a row lives as long as the scope that recorded it -------------------
\ A row names its definer by the checker's symbol id, and a scope that pops
\ hands its ids out again: a row that outlived its scope would make the next
\ word given that id a definer (the stale pair), and a source read in two
\ scopes would meet its own row in the second (the replay pair). Both pairs
\ read in neutral scopes, as tools/check.f reads a source. A scope that
\ FINALIZES keeps its ids and its rows with them, and a sibling scope popping
\ afterwards must not take them back. Those three reads stay in this package:
\ FINALIZE drops the frame without the package restore a pop runs, so a
\ finalized neutral scope would leave this file at top level, and the last
\ read must see the word the kept scope defined here. Each read runs in a
\ scope of its own and answers its throw code; the stale pair's E-UNDEFINED
\ line on stderr is the refusal being measured. MAIN runs this section before
\ section 9: its reads, in this file's own scope, are rendering statements, and
\ the mark each leaves on the global wordlist would cover the stale pair's
\ unresolved name for the rest of the process, leaving it to a run instead of
\ refusing it (src/core/checker.f UNSEEN-COVERS?).
TYPED-VARIABLE CDD-SRC-A ptr u8
variable CDD-SRC-U

\ A quotation cannot read the enclosing word's locals, so the span travels to
\ the caught read through these two cells.
: CDD-READ ( -- )
   CDD-SRC-A @ CDD-SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

: CDD-SPAN! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a CDD-SRC-A !
   u CDD-SRC-U ! ;

\ A neutral scope that pops, as tools/check.f reads a source.
: CDD-SCOPED ( ptr u8 n -- n )
   CDD-SPAN!
   CHECKER-SCOPE-START-NEUTRAL
   [: CDD-READ ;] catch
   CHECKER-SCOPE-DONE ;

\ A scope in this package that pops.
: CDD-OWN ( ptr u8 n -- n )
   CDD-SPAN!
   CHECKER-SCOPE-START
   [: CDD-READ ;] catch
   CHECKER-SCOPE-DONE ;

\ A scope in this package that keeps what it read.
: CDD-KEPT ( ptr u8 n -- n )
   CDD-SPAN!
   CHECKER-SCOPE-START
   [: CDD-READ ;] catch
   {: rc:n :}
   rc 0= IF CHECKER-SCOPE-FINALIZE ELSE CHECKER-SCOPE-DONE THEN
   rc ;

: CDD-REPLAY$ ( -- ptr u8 n )
   s\" : CDD-SG ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-SG ( -- ptr n )\n5 CDD-SG CDD-SGM\n: CDD-SGU ( -- ptr n ) CDD-SGM ;\n" ;

: CDD-SECTION-SCOPES ( -- )
   s" a source read in two scopes takes its row once in each" T-LABEL
   CDD-REPLAY$ CDD-SCOPED 0 T=
   CDD-REPLAY$ CDD-SCOPED 0 T=
   s" the stale pair's definer reads in a scope that pops" T-LABEL
   s\" : CDD-SD ( n -- ) create , does> ( -- ptr n ) ;\n" CDD-SCOPED 0 T=
   s" and its row does not outlive the ids that scope gave out" T-LABEL
   s\" : CDD-SE ( -- n ) 5 ;\nCDD-SE CDD-SF\n: CDD-SU ( -- ptr n ) CDD-SF ;\n" CDD-SCOPED 70 T=
   s" a finalized scope keeps its definer's row" T-LABEL
   s\" : CDD-SK ( n -- ) create , does> ( -- ptr n ) ;\n" CDD-KEPT 0 T=
   s\" : CDD-SX ( -- n ) 1 ;\n" CDD-OWN 0 T=
   s" through a sibling scope's pop" T-LABEL
   s\" 5 CDD-SK CDD-SKMADE\n: CDD-SKU ( -- ptr n ) CDD-SKMADE ;\n" CDD-OWN 0 T= ;

\ ---- 12. the engine refuses a row it cannot keep ----------------------------
\ The checker's own error constant, read at load time under a local name, the
\ way test/checker-replay-pkg-state.f reads E-USING-UNBALANCED.
E-GENERATES-ROW constant E-GEN-ROW

: CDD-SECTION-GENERATES-REFUSED ( -- )
   s" a row read before its definer is defined is refused" T-LABEL
   [: s\" generates: CDD-GL ( -- ptr n )\n: CDD-GL ( n -- ) CDD-GEN:MAKE ;\n" CDD-EVAL ;] E-GEN-ROW TTHROWSQ
   s" a row on a definer whose clause states its word is refused" T-LABEL
   [: s\" generates: CDD-RES-D ( -- ptr n )\n" CDD-EVAL ;] E-GEN-ROW TTHROWSQ
   s\" : CDD-G2 ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-G2 ( -- ptr n )\n" CDD-EVAL
   s" a second row on one definer is refused" T-LABEL
   [: s\" generates: CDD-G2 ( -- ptr n )\n" CDD-EVAL ;] E-GEN-ROW TTHROWSQ
   s" an effect the checker cannot parse is refused" T-LABEL
   [: s\" : CDD-GI ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-GI ( -- i32 )\n" CDD-EVAL ;] E-GEN-ROW TTHROWSQ ;

: MAIN ( -- )
   T-RESET
   CDD-SECTION-DEFINER
   CDD-SECTION-WRAPPER
   CDD-SECTION-LATCH
   CDD-SECTION-WRAP-LATCH
   CDD-SECTION-TRUSTED
   CDD-SECTION-TRUSTED-LIVE
   CDD-SECTION-NOT-A-WRAPPER
   CDD-SECTION-LIVE
   CDD-SECTION-RESIDENT
   CDD-SECTION-SCOPES
   CDD-SECTION-GENERATES
   CDD-SECTION-GENERATES-LIVE
   CDD-SECTION-FUNCTION
   CDD-SECTION-GENERATES-REFUSED
   T-REPORT ;

MAIN

;package
