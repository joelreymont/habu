\ certify-does-definer.f - certification knows what a `does>` definer publishes.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f \
\   src/habu/verify-source.f test/certify-does-definer.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/codegen.f
require src/lib/ffi-abi.f
require src/habu/verify-source.f

\ The resident package definer of section 5, compiled by the engine: its two
\ spellings, qualified and bare under `using`, are what a source can name it by,
\ and both reach one row.
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

\ The text-writing definer of sections 9-12, the shape of src/lib/task.f
\ +USER: MAKE builds `: NAME ( -- ptr n ) data-base OFF + ;` for the next name
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

\ The same definer compiled by the engine with its row, private definer and
\ public wrapper as section 9's sources write them: section 9 reads a source
\ that only calls it, and runs it for real.
package CDD-GQ4
: CDD-GD4 ( n -- ) CDD-GEN:MAKE ;
generates: CDD-GD4 ( -- ptr n )
public
: CDD-GD4 ( n -- ) CDD-GD4 ;
;package

\ Section 15's packaged word, patched at once by a global does> word defined
\ after `;package`: the patching word's spelling resolves outside CDD-PKC.
package CDD-PKC
public
variable CDD-PKV
;package
: CDD-PKB ( -- ) does> ( -- n ) drop 9 ;
CDD-PKB

\ Section 16's definer, defined from outside its package: its publication gets
\ the tail bool>n, which names hb's sealed global word, not this definer.
package CDD-QQ
public
;package
: CDD-QQ:bool>n ( n -- ) create , does> ( -- n n ) @ dup ;
7 CDD-QQ:bool>n CDD-QF

package CERTIFY-DOES-DEFINER

\ The rest of section 5's fixtures, also compiled by the engine here: this
\ file's scanner never reads their text, so what they create is known only
\ through the store the checker wrote when it certified their clause.
: CDD-RES-D ( n -- ) create , does> ( -- ptr n ) ;
: CDD-RES-W ( -- ) 3 CDD-RES-D ;          \ a resident wrapper: known through the checker's walk
: CDD-RES-P ( n -- n ) 1 + ;              \ the plain definition right after a definer

\ The resident twins of C10-C12: the walk counts definer calls and notes any
\ bend, so these three are wrappers of nothing at all.
: CDD-RES-IFW ( n -- ) dup 0= if drop 1 then CDD-RES-D ;
: CDD-RES-TWOW ( n n -- ) CDD-RES-D CDD-RES-D ;
\ The native compiler needs a typed consumer for a quotation it never calls.
: CDD-DROP-Q ( [ -- ] -- ) drop ;
: CDD-RES-QW ( n -- ) drop [: 5 CDD-RES-D ;] CDD-DROP-Q ;

\ A resident definer whose clause calls a definer: its own clause is what it
\ creates, and the clause walk leaves nothing for the next record.
: CDD-RES-CD ( n -- ) create , does> ( -- ) drop 3 CDD-RES-D ;

-1 constant ACCEPTED
0 constant REFUSED
0 constant UNRESOLVED          \ a name nothing resolves is refused, E-UNDEFINED
2 constant DEFERRED            \ a mark leaves the name to the run

\ The engine publishes a does> definer's clause row at creation (habu2.f
\ DOESPATCH:EMIT); the pre-verifier, which runs no definer, reads it from text.

\ A scanned word binds through the checker overlay only while the neutral scope
\ holding its scan is open, so its probes ask the certify path inside it.
: CDD-VERDICT ( ptr u8 n -- n )
   VERIFY:CANDIDATE-IN-SCOPE ;

\ A word the engine created (section 4) is asked by the engine's lookup.
: CDD-LIVE-VERDICT ( ptr u8 n -- n )
   CHECK-QUIET-CANDIDATE! ;

\ Section 1: the definer the scanner read and the word it creates.
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

\ Section 2: a package definer, its straight-line wrapper and both spellings.
\ `: BUFFER ( n -- ) E-CG-CAP E-CG-VALUE BUFFER-E ;` (src/lib/codegen.f) is the
\ production shape of the wrapper: a body whose tokens hold exactly one definer
\ call and no control flow creates whatever that definer creates.
: CDD-SECTION-WRAPPER ( -- )
   CHECKER-SCOPE-START-NEUTRAL
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
   s" C9 ( -- n ) CDD-WRAP" CDD-VERDICT REFUSED T=
   CHECKER-SCOPE-DONE ;

\ Section 3: what a body has to be before it counts as a wrapper. A definer call
\ reached only conditionally, twice or inside a quotation says nothing about
\ what the word creates, so nothing is recorded (here and the last three rows of
\ section 5): the wrapper rule is not a prefix match on "calls something that
\ creates". Such a word still calls `create`, so a top-level statement running
\ it marks its wordlist: a later name nothing there resolves is left to the run
\ (DEFERRED, src/core/checker.f UNSEEN-COVERS?), not refused as UNRESOLVED. The
\ run takes the latch sections, which refuse names as UNRESOLVED, before this one
\ marks this file's wordlist.
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

\ Section 4: the learned row is the row the engine publishes.
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

\ Section 5: a definer the scanner never read. CDD-RES-D and CDD-RESP:CDD-RP-D
\ were compiled by the engine here; what they create is known only from the
\ store the checker wrote when it certified their clause. Without that store,
\ `bin/hb tools/check.f src/lib/queue.f` refused NG-BUFFER, created in
\ src/lib/type/deftype.f by the resident CODEGEN:BUFFER-E, as E-UNDEFINED. The
\ last rows are the latch's lifetime: the store is written between two checker
\ entry points. The engine checks a clause and then the definer's body; the
\ scanner checks the body and then the clause. Either order must leave a plain
\ definition with nothing, and a refused clause nothing at all. The refusal
\ row's `E-DOES-TWICE habu: <file>:1: does>` line on stderr is the refusal being measured, not a
\ test failure.
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
   s" C22 ( -- ptr n ) CDD-SO-MADE" CDD-VERDICT UNRESOLVED T= ;

\ Section 6: the refused clause and the definition after it are live
\ definitions, so they run between two scopes.
: CDD-SECTION-LATCH-LIVE ( -- )
   s" a clause its body contradicts is refused" T-LABEL
   [: s\" : CDD-RES-BAD ( n -- ) create , does> ( -- n ) ;\n" evaluate-closed ;]
      70 TTHROWSQ
   s\" : CDD-RES-AFTER ( n -- n ) 2 * ;\n" evaluate-closed ;

: CDD-SECTION-LATCH-AFTER ( -- )
   s\" 6 CDD-RES-AFTER CDD-AFTER-MADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" and leaves nothing for the next definition to inherit" T-LABEL
   s" C23 ( -- ptr n ) CDD-AFTER-MADE" CDD-VERDICT UNRESOLVED T= ;

\ Section 7: the wrapper latch's lifetime. The wrapper fact is learned by the
\ body walk, and three walks are not a definition's own body: a candidate, a
\ `does>` clause, and the definer body whose own clause already says what it
\ creates. Each row below is a record published right after one of those walks,
\ and each must be untouched by it.
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
   \ The same clause in source order, which is the order that could inherit: the
   \ pre-pass checks the definer's body first and the clause last, so the next
   \ record published is the created word's (verify-source CREATED-TRUST-NEXT?).
   s\" : CDD-SRC-CD ( n -- ) create , does> ( -- ) drop 3 CDD-RES-D ;\n5 CDD-SRC-CD CDD-SRC-MADE\nCDD-SRC-MADE CDD-SRC-X\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a clause read from source arms nothing the created word takes" T-LABEL
   s" C34 ( -- ptr n ) CDD-SRC-X" CDD-VERDICT UNRESOLVED T= ;

\ Section 9: a definer that writes its word as text; a `generates: D ( effect )`
\ row states what D makes. Section 10 reads FUNCTION:'s group, section 11 holds
\ a row to the scope that recorded it, section 12 is the engine refusing a row.
\ CDD-GEN:MAKE leaves nothing a scanner can read: its word exists only once the
\ text it builds is evaluated, which the pre-pass never does. The row states the
\ effect on the private definer, and the public wrapper inherits it as section
\ 2's wrapper does. The row may sit before or after `public`: either way the
\ private definer is the only CDD-GD* defined when the row is read.
: CDD-SECTION-GENERATES ( -- )
   CHECKER-SCOPE-START-NEUTRAL
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
   s" C50 ( -- n ) CDD-GD4-MADE" CDD-VERDICT REFUSED T=
   CHECKER-SCOPE-DONE ;

\ The resident definer run for real: CDD-GD4-LIVE is the word the evaluated text
\ defines, checked against the effect that text declares, so the row is pinned
\ against the generated word rather than against itself.
8 CDD-GQ4:CDD-GD4 CDD-GD4-LIVE

: CDD-SECTION-GENERATES-LIVE ( -- )
   s" the generated word certifies as the effect its row states" T-LABEL
   s" C51 ( -- ptr n ) CDD-GD4-LIVE" CDD-VERDICT ACCEPTED T=
   s" and refuses the bare cell the row refuses" T-LABEL
   s" C52 ( -- n ) CDD-GD4-LIVE" CDD-VERDICT REFUSED T= ;

\ Section 10: FUNCTION:.
\ src/lib/ffi-abi.f FUNCTION: makes its word from the declaration group, so the
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

\ Section 11: a row lives as long as its scope. A popped scope hands its symbol
\ ids out again, so a row outliving it would make that id's next word a definer.
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

\ The stale pair's E-UNDEFINED line on stderr is the refusal being measured.
: CDD-SECTION-SCOPES ( -- )
   s" a source read in two scopes takes its row once in each" T-LABEL
   CDD-REPLAY$ CDD-SCOPED 0 T=
   CDD-REPLAY$ CDD-SCOPED 0 T=
   s" the stale pair's definer reads in a scope that pops" T-LABEL
   s\" : CDD-SD ( n -- ) create , does> ( -- ptr n ) ;\n" CDD-SCOPED 0 T=
   s" and its row does not outlive the ids that scope gave out" T-LABEL
   s\" : CDD-SE ( -- n ) 5 ;\nCDD-SE CDD-SF\n: CDD-SU ( -- ptr n ) CDD-SF ;\n" CDD-SCOPED 70 T= ;

: CDD-SECTION-KEPT ( -- )
   s" a finalized scope keeps its definer's row" T-LABEL
   s\" : CDD-SK ( n -- ) create , does> ( -- ptr n ) ;\n" CDD-KEPT 0 T=
   s\" : CDD-SX ( -- n ) 1 ;\n" CDD-OWN 0 T=
   s" through a sibling scope's pop" T-LABEL
   s\" 5 CDD-SK CDD-SKMADE\n: CDD-SKU ( -- ptr n ) CDD-SKMADE ;\n" CDD-OWN 0 T= ;

\ Section 12: the engine refuses a row it cannot keep. A refused row prints its
\ E-GENERATES-ROW line on stderr before it throws, as section 5's refused clause
\ does. A row on a word that reads no name after it, `evaluate` or a deferred
\ word is refused too: kept, it makes the pre-pass take the token after a call
\ as a made name the load never makes. The checker's own error constant, read
\ at load time under a local name.
E-GENERATES-ROW constant E-GEN-ROW

\ Values of 2 to 128 cells, for a row of 256 input cells short enough to state.
STRUCTURE cdd-c2 FIELD a n FIELD b n ;STRUCTURE
STRUCTURE cdd-c4 FIELD a cdd-c2 FIELD b cdd-c2 ;STRUCTURE
STRUCTURE cdd-c8 FIELD a cdd-c4 FIELD b cdd-c4 ;STRUCTURE
STRUCTURE cdd-c16 FIELD a cdd-c8 FIELD b cdd-c8 ;STRUCTURE
STRUCTURE cdd-c32 FIELD a cdd-c16 FIELD b cdd-c16 ;STRUCTURE
STRUCTURE cdd-c64 FIELD a cdd-c32 FIELD b cdd-c32 ;STRUCTURE
STRUCTURE cdd-c128 FIELD a cdd-c64 FIELD b cdd-c64 ;STRUCTURE

: CDD-SECTION-GENERATES-REFUSED ( -- )
   s" a row read before its definer is defined is refused" T-LABEL
   [: s\" generates: CDD-GL ( -- ptr n )\n: CDD-GL ( n -- ) CDD-GEN:MAKE ;\n" evaluate-closed ;] E-GEN-ROW TTHROWSQ
   s" a row on a definer whose clause states its word is refused" T-LABEL
   [: s\" generates: CDD-RES-D ( -- ptr n )\n" evaluate-closed ;] E-GEN-ROW TTHROWSQ
   s\" : CDD-G2 ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-G2 ( -- ptr n )\n" evaluate-closed
   s" a second row on one definer is refused" T-LABEL
   [: s\" generates: CDD-G2 ( -- ptr n )\n" evaluate-closed ;] E-GEN-ROW TTHROWSQ
   s" an effect the checker cannot parse is refused" T-LABEL
   [: s\" : CDD-GI ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-GI ( -- i32 )\n" evaluate-closed ;] E-GEN-ROW TTHROWSQ
   s" an effect too wide to record is refused" T-LABEL
   [: s\" : CDD-GW ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-GW ( cdd-c128 cdd-c64 cdd-c32 cdd-c16 cdd-c8 cdd-c4 cdd-c2 n n -- )\n" evaluate-closed ;]
      E-GEN-ROW TTHROWSQ
   s" a row on a word that reads no name after it is refused" T-LABEL
   [: s\" : CDD-GN ( n -- ) drop ;\ngenerates: CDD-GN ( -- ptr n )\n" evaluate-closed ;] E-GEN-ROW TTHROWSQ
   s" a row on a deferred word is refused" T-LABEL
   [: s\" defer CDD-GF ( n -- )\ngenerates: CDD-GF ( -- ptr n )\n" evaluate-closed ;] E-GEN-ROW TTHROWSQ
   s" a row on evaluate, which reads no name, is refused" T-LABEL
   [: s\" generates: evaluate ( -- )\n" evaluate-closed ;] E-GEN-ROW TTHROWSQ ;

\ Section 13: a clause's body ends as its colon twin's does (checker.f
\ CHECK-DOES-RUN); refused clauses' diagnostics go to CDD-DIAG, not stderr.
defer CDD-VSAME ( mut-view<p,q,a,u8> mut-view<p,q,a,u8> -- )
create CDD-DIAG 4096 allot

: CDD-NOT-CERTIFIED ( ptr u8 n ptr u8 n -- )
   {: sa:ptr su:n wa:ptr wu:n :}
   CDD-DIAG 4096 DIAG-BUFFER!
   sa su TEST-EVAL:RC  70 T=
   DIAG-BUFFER$ wa wu CONTAINS? TTRUE
   DIAG-BUFFER-OFF ;

: CDD-NP-REFUSED ( ptr u8 n -- )
   s" E-NONPARAMETRIC-EFFECT" CDD-NOT-CERTIFIED ;

\ A clause declaring 256 input cells, whose body takes 257 with the data field.
$600 constant CDD-WIDE-CAP
CDD-WIDE-CAP CODEGEN:BUFFER CDD-WIDE
: CDD-WIDE-ROW ( -- ) 256 0 do s" n " CDD-WIDE CODEGEN:APPEND-STRING loop ;
: CDD-WIDE$ ( -- ptr u8 n )
   CDD-WIDE CODEGEN:RESET
   s" : CDD-W1 ( n -- ) create , does> ( " CDD-WIDE CODEGEN:APPEND-STRING
   CDD-WIDE-ROW  s" -- " CDD-WIDE CODEGEN:APPEND-STRING
   CDD-WIDE-ROW  s" ) drop ;" CDD-WIDE CODEGEN:APPEND-STRING
   CDD-WIDE CODEGEN:CONTENTS ;

\ A clause declaring 255 input cells, whose body takes 256 with the data field;
\ its packet declares the signature as written.
$800 constant CDD-FIT-CAP
CDD-FIT-CAP CODEGEN:BUFFER CDD-FIT
CDD-FIT-CAP CODEGEN:BUFFER CDD-FIT-WANT
: CDD-FIT-ROW ( ptr n -- )
   {: d:ptr :}
   255 0 do s" n " d CODEGEN:APPEND-STRING loop ;
: CDD-FIT$ ( -- ptr u8 n )
   CDD-FIT CODEGEN:RESET
   s" -1 JSON-DIAGS ! : CDD-F1 ( n -- ) create , does> ( " CDD-FIT CODEGEN:APPEND-STRING
   CDD-FIT CDD-FIT-ROW  s" -- n ) " CDD-FIT CODEGEN:APPEND-STRING
   255 0 do s" drop " CDD-FIT CODEGEN:APPEND-STRING loop
   s" ;" CDD-FIT CODEGEN:APPEND-STRING
   CDD-FIT CODEGEN:CONTENTS ;

\ The packet field named by its key text, holding the 255-input row then the tail.
: CDD-FIT-FIELD ( ptr u8 n ptr u8 n -- )
   {: ka:ptr ku:n ta:ptr tu:n :}
   CDD-FIT-WANT CODEGEN:RESET
   ka ku CDD-FIT-WANT CODEGEN:APPEND-STRING
   CDD-FIT-WANT CDD-FIT-ROW
   ta tu CDD-FIT-WANT CODEGEN:APPEND-STRING
   DIAG-BUFFER$ CDD-FIT-WANT CODEGEN:CONTENTS CONTAINS? TTRUE ;

: CDD-FIT-DECLARED ( -- )
   CDD-DIAG 4096 DIAG-BUFFER!
   CDD-FIT$ TEST-EVAL:RC  70 T=
   DIAG-BUFFER$ s" input row too wide to record (256 cells, at most 255)" CONTAINS? TTRUE
   s\" \"declared_effect\":\"" s\" -- n \"" CDD-FIT-FIELD
   s\" \"declared_effect_source\":\"" s\" -- n\"" CDD-FIT-FIELD
   DIAG-BUFFER-OFF
   s" 0 JSON-DIAGS !" TEST-EVAL:RC 0 T= ;

: CDD-SECTION-CLAUSE-TAIL ( -- )
   s" a clause that specializes a declared quantifier is refused" T-LABEL
   s" : CDD-NP1 ( n -- ) create , does> ( a -- n ) drop ;" CDD-NP-REFUSED
   s" a clause that unifies two declared quantifiers is refused" T-LABEL
   s" : CDD-NP2 ( n -- ) create , does> ( mut-view<p,q,a,u8> mut-view<p,q,c,u8> -- ) drop CDD-VSAME ;"
      CDD-NP-REFUSED
   s" a clause that unifies two declared rows is refused" T-LABEL
   s" : CDD-NP3 ( n -- ) create , does> ( R -- S ) drop ;" CDD-NP-REFUSED
   s" a clause that claims a quotation's output row without running it is refused" T-LABEL
   s" : CDD-NP4 ( n -- ) create , does> ( R [ R -- S ] -- S ) drop drop ;" CDD-NP-REFUSED
   \ The return-stack seal's, not the parametric pass's (checker.f RS-BORROWS?).
   s" a clause that reads below its declared return row is refused at the read" T-LABEL
   s" : CDD-RB1 ( n -- ) create , does> ( | R -- | R ) drop r> drop 5 throw ;"
      s" at 'r>' return stack: this reads below the return-stack cells pushed here" CDD-NOT-CERTIFIED
   s" a clause too wide to record is uncheckable, and the load goes on" T-LABEL
   CDD-WIDE$ s" input row too wide to record (257 cells, at most 255)" CDD-NOT-CERTIFIED
   s" a clause whose signature fits a record declares it, though its body is too wide" T-LABEL
   CDD-FIT-DECLARED
   s" a clause's unknown signature type is reported at that type, as its colon twin's is" T-LABEL
   s" : CDD-UX ( n -- ) create , does> ( cdd-zz -- cdd-zz ) drop drop 5 ;"
      s" unknown type 'cdd-zz'" CDD-NOT-CERTIFIED
   s" compiled natively, the clause's unknown type is placed at that type in its text" T-LABEL
   s" -1 JSON-DIAGS ! : CDD-UXJ ( n -- ) create , does> ( cdd-zz -- cdd-zz ) drop drop 5 ;"
      S\" \"token\":\"cdd-zz\",\"token_index\":0,\"file\":\"<input>\",\"line\":1,\"column\":4,\"byte_start\":3,\"byte_end\":9,"
      CDD-NOT-CERTIFIED
   s" 0 JSON-DIAGS !" TEST-EVAL:RC 0 T=
   s" a clause that never returns owes no output" T-LABEL
   s" : CDD-NORET ( n -- ) create , does> ( n -- ptr u8 ) drop 5 throw ;" TEST-EVAL:RC 0 T= ;

\ Section 14: a clause naming a type a reached renderer may declare is deferred
\ unwalked at that type, as its colon twin is (verify-source.f NOMINAL-MARK).
: CDD-QUIET$ ( -- ptr u8 n )
   s\" package CDD-QP\n: CDD-QREG ( -- ) s\" NEWTYPE cdd-qt\" INCLUDE-EVALUATE ;\nCDD-QREG\n: CDD-QB ( n -- ) create , does> ( cdd-qt -- cdd-qt ) drop drop 5 ;\n;package\n" ;

: CDD-SECTION-QUIET-CLAUSE ( -- )
   s" a clause naming a type a renderer may declare is deferred at that type" T-LABEL
   true VERIFY:REPORT-DEFERRALS
   CDD-DIAG 4096 DIAG-BUFFER!
   CHECKER-SCOPE-START-NEUTRAL
   [: CDD-QUIET$ s" cdd-quiet.f" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE
   0 T=
   DIAG-BUFFER$ s\" \"verdict\":\"deferred\",\"token\":\"cdd-qt\"" CONTAINS? TTRUE
   DIAG-BUFFER-OFF
   VERIFY:CENSUS 1 T= drop ;

\ Section 15: a does> word patches the latest created word, which takes the
\ clause effect while the patching word keeps its own. Each text creates its
\ own word and stores what that word gives in CDD-GOT.
variable CDD-GOT

: CDD-GOT-RC ( ptr u8 n -- n n )
   0 CDD-GOT !  TEST-EVAL:RC  CDD-GOT @ ;

: CDD-SECTION-PATCH ( -- )
   s" a word patched by a does> word defined after it gives the clause's cell" T-LABEL
   s" variable CDD-PV1 : CDD-PB1 ( -- ) does> ( -- n ) drop 9 ; CDD-PB1 CDD-PV1 CDD-GOT !"
      CDD-GOT-RC 9 T= 0 T=
   s" the patching word keeps its own effect" T-LABEL
   s" variable CDD-PV3 : CDD-PB3 ( -- ) does> ( -- n ) drop 9 ; CDD-PB3 CDD-PV3 drop"
      TEST-EVAL:RC 0 T=
   s" : CDD-PT3 ( -- n ) CDD-PB3 ;" TEST-EVAL:RC  70 T=
   s" a does> word defined before the created word patches it the same way" T-LABEL
   s" : CDD-PB4 ( -- ) does> ( -- n ) drop 9 ; variable CDD-PV4 CDD-PB4 CDD-PV4 CDD-GOT !"
      CDD-GOT-RC 9 T= 0 T=
   s" a packaged word patched by a global does> word gives the clause's cell" T-LABEL
   s" CDD-PKC:CDD-PKV CDD-GOT !" CDD-GOT-RC 9 T= 0 T=
   s" a word created over a forgotten definition's ordinal starts unlinked" T-LABEL
   s" : CDD-FD ( -- n ) 5 ; ndict@ 1- ndict! create CDD-FX" TEST-EVAL:RC 0 T=
   s" : CDD-FG ( -- ) CDD-FX ;" TEST-EVAL:RC  70 T=
   \ Last: a word still typed as its cell runs `@` on the clause's 9 and dies.
   s" the patched word's cell is gone, so `@` on it is refused" T-LABEL
   s" variable CDD-PV2 : CDD-PB2 ( -- ) does> ( -- n ) drop 9 ; CDD-PB2 CDD-PV2 @ drop"
      TEST-EVAL:RC 70 T= ;

\ Section 16: the qualified definer's created word gives the clause's cells.
: CDD-SECTION-QUALIFIED ( -- )
   s" a definer named like an hb word from outside its package runs" T-LABEL
   CDD-QF + 14 T= ;

\ Each scan runs with its probes in one neutral scope; sections 2 and 9 open
\ theirs outside this package, as a scan begun inside one refuses `package`.
T-RESET
CHECKER-SCOPE-START-NEUTRAL package CERTIFY-DOES-DEFINER CDD-SECTION-DEFINER ;package CHECKER-SCOPE-DONE
CDD-SECTION-WRAPPER
CHECKER-SCOPE-START-NEUTRAL package CERTIFY-DOES-DEFINER CDD-SECTION-LATCH ;package CHECKER-SCOPE-DONE
\ A section that defines live words runs outside a scope, which refuses a
\ definition (ENGINE-ERROR:OVERLAY-OPEN).
CDD-SECTION-LATCH-LIVE
CHECKER-SCOPE-START-NEUTRAL package CERTIFY-DOES-DEFINER CDD-SECTION-LATCH-AFTER ;package CHECKER-SCOPE-DONE
CHECKER-SCOPE-START-NEUTRAL package CERTIFY-DOES-DEFINER CDD-SECTION-WRAP-LATCH ;package CHECKER-SCOPE-DONE
CHECKER-SCOPE-START-NEUTRAL package CERTIFY-DOES-DEFINER CDD-SECTION-NOT-A-WRAPPER ;package CHECKER-SCOPE-DONE
CDD-SECTION-LIVE
CHECKER-SCOPE-START-NEUTRAL package CERTIFY-DOES-DEFINER CDD-SECTION-RESIDENT ;package CHECKER-SCOPE-DONE
\ Section 11 runs before section 9, whose statements mark the global wordlist
\ and would leave the stale pair's name to a run (checker.f UNSEEN-COVERS?).
CDD-SECTION-SCOPES
CHECKER-SCOPE-START-NEUTRAL package CERTIFY-DOES-DEFINER CDD-SECTION-KEPT ;package CHECKER-SCOPE-DONE
CDD-SECTION-GENERATES
CDD-SECTION-GENERATES-LIVE
CHECKER-SCOPE-START-NEUTRAL package CERTIFY-DOES-DEFINER CDD-SECTION-FUNCTION ;package CHECKER-SCOPE-DONE
CDD-SECTION-GENERATES-REFUSED
CDD-SECTION-CLAUSE-TAIL
CDD-SECTION-QUIET-CLAUSE
CDD-SECTION-PATCH
CDD-SECTION-QUALIFIED
T-REPORT

;package
