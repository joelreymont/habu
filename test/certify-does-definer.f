\ certify-does-definer.f - certification knows what a `does>` definer publishes.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f \
\   src/habu/verify-source.f test/certify-does-definer.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/eval.f
require src/lib/codegen.f
require src/lib/ffi-abi.f
require src/habu/verify-source.f

\ The resident package definer of section 5, compiled by the engine: its two
\ spellings, qualified and bare under `using`, are what a source can name it by,
\ and both reach one row.
package CDD-RESP
public
: CDD-RP-D ( n -- ) create , does> ( -- ^n ) ;
;package

\ An export of that definer: the same xt under another tail, and the tail is a
\ symbol of its own, so what the definer creates has to be copied to it
\ (checker.f EXPORT-META-COPY) or the exported spelling knows nothing.
package CDD-RESX
public
EXPORT CDD-RESP:CDD-RP-D
;package

\ The text-writing definer of sections 9-12: MAKE builds `: NAME ( -- ^n )
\ CDD-GEN:AREA OFF + ;` for the next name and evaluates it, so its word has no
\ `does>` clause for a checker to read.
package CDD-GEN
CODEGEN:BUFFER TEXT
public
create AREA 2 cells allot
: MAKE ( n -- )
   {: off:n :}
   parse-name
   {: name:str :}
   TEXT CODEGEN:RESET
   s" : " TEXT CODEGEN:APPEND-STRING
   name TEXT CODEGEN:APPEND-STRING
   s"  ( -- ^n ) CDD-GEN:AREA " TEXT CODEGEN:APPEND-STRING
   off TEXT CODEGEN:APPEND-DECIMAL
   s"  + ;" TEXT CODEGEN:APPEND-STRING
   TEXT CODEGEN:CONTENTS evaluate-closed ;
;package

\ The same definer compiled by the engine with its row, private definer and
\ public wrapper as section 9's sources write them: section 9 reads a source
\ that only calls it, and runs it for real.
package CDD-GQ4
: CDD-GD4 ( n -- ) CDD-GEN:MAKE ;
generates: CDD-GD4 ( -- ^n )
public
: CDD-GD4 ( n -- ) CDD-GD4 ;
;package

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
: CDD-RES-D ( n -- ) create , does> ( -- ^n ) ;
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
0 constant REJECTED
0 constant UNRESOLVED          \ a name nothing resolves is rejected, E-UNDEFINED
2 constant DEFERRED            \ a mark leaves the name to the run

\ The engine publishes a does> definer's clause row at creation (habu2.f
\ DOESPATCH:EMIT); the pre-verifier, which runs no definer, reads it from text.

\ A scanned word binds through the checker overlay only while the neutral scope
\ holding its scan is open, so its probes ask the certify path inside it.
: CDD-VERDICT ( str -- n )
   VERIFY:CANDIDATE-IN-SCOPE ;

\ A word the engine created (section 4) is asked by the engine's lookup.
: CDD-LIVE-VERDICT ( str -- n )
   CHECK-QUIET-CANDIDATE! ;

\ Section 1: the definer the scanner read and the word it creates.
: CDD-SECTION-DEFINER ( -- )
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-D ( n -- ) create , does> ( -- ^n ) ;\n8 CDD-D CDD-ONE\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" the created word certifies as the clause effect" T-LABEL
   s" C1 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-ONE" CDD-VERDICT ACCEPTED T=
   s" the same word is rejected against a bare cell" T-LABEL
   s" C2 ( -- n ) CERTIFY-DOES-DEFINER:CDD-ONE" CDD-VERDICT REJECTED T=
   s" a byte pointee is rejected too" T-LABEL
   s" C3 ( -- ^u8 ) CERTIFY-DOES-DEFINER:CDD-ONE" CDD-VERDICT REJECTED T=
   s" a name the definer never created stays unresolvable" T-LABEL
   s" C4 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-TWO" CDD-VERDICT UNRESOLVED T=
   s" the definer itself keeps its own declared effect" T-LABEL
   s" C5 ( n -- ) CERTIFY-DOES-DEFINER:CDD-D" CDD-VERDICT ACCEPTED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-DU ( n -- ) create , DoEs> ( -- ^n ) ;\n8 CDD-DU CDD-ONE-U\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a mixed-case `DoEs>` opens the clause, as the engine reads it" T-LABEL
   s" C1U ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-ONE-U" CDD-VERDICT ACCEPTED T= ;

\ Section 2: a package definer, its straight-line wrapper and both spellings.
\ A wrapper body whose tokens hold exactly one definer call and no control flow
\ creates whatever that definer creates.
: CDD-SECTION-WRAPPER ( -- ) [:
   s\" package CDDP\npublic\n: CDD-PD ( n n n -- ) create , , , does> ( -- ^n ) ;\n: CDD-PW ( n -- ) 3 4 CDD-PD ;\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s\" 8 CDDP:CDD-PD CDD-QUAL\n9 CDDP:CDD-PW CDD-WRAP\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s\" using CDDP\n7 CDD-PW CDD-BARE\n;using\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" the qualified definer's created word certifies" T-LABEL
   s" C6 ( -- ^n ) CDD-QUAL" CDD-VERDICT ACCEPTED T=
   s" the wrapper's created word certifies the same effect" T-LABEL
   s" C7 ( -- ^n ) CDD-WRAP" CDD-VERDICT ACCEPTED T=
   s" a bare wrapper call under `using` records the same row" T-LABEL
   s" C8 ( -- ^n ) CDD-BARE" CDD-VERDICT ACCEPTED T=
   s" the wrapper's created word is rejected against a bare cell" T-LABEL
   s" C9 ( -- n ) CDD-WRAP" CDD-VERDICT REJECTED T=
   ;] CHECK:SCOPED ;

\ Section 3: what a body has to be before it counts as a wrapper. A definer call
\ reached only conditionally, twice or inside a quotation says nothing about
\ what the word creates, so nothing is recorded (here and the last three rows of
\ section 5): the wrapper rule is not a prefix match on "calls something that
\ creates". Such a word still calls `create`, so a top-level statement running
\ it marks its wordlist: a later name nothing there resolves is left to the run
\ (DEFERRED, src/core/checker.f UNSEEN-COVERS?), not rejected as UNRESOLVED. The
\ run takes the latch sections, which reject names as UNRESOLVED, before this one
\ marks this file's wordlist.
: CDD-SECTION-NOT-A-WRAPPER ( -- )
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-CD ( n -- ) create , does> ( -- ^n ) ;\n: CDD-IFW ( n -- ) dup 0= if drop 1 then CDD-CD ;\n2 CDD-IFW CDD-COND\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer behind a conditional records nothing" T-LABEL
   s" C10 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-COND" CDD-VERDICT DEFERRED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-IFWU ( n -- ) dup 0= IF drop 1 THEN CDD-CD ;\n2 CDD-IFWU CDD-CONDU\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer behind an uppercase conditional records nothing" T-LABEL
   s" C10U ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-CONDU" CDD-VERDICT DEFERRED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-TWOW ( n n -- ) CDD-CD CDD-CD ;\n3 4 CDD-TWOW CDD-TWICE\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" two definer calls in one body record nothing" T-LABEL
   s" C11 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-TWICE" CDD-VERDICT DEFERRED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-QW ( n -- ) drop [: 5 CDD-CD ;] drop ;\n6 CDD-QW CDD-QUOT\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer inside a quotation records nothing" T-LABEL
   s" C12 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-QUOT" CDD-VERDICT DEFERRED T= ;

\ Section 4: the learned row is the row the engine publishes.
\ The same definition, compiled and run by the engine: CDD-LIVE-ONE's effect
\ here comes from DOESPATCH:EMIT, not from the scanner, and the two agree on
\ both the acceptance and the rejection.
: CDD-LIVE-D ( n -- ) create , does> ( -- ^n ) ;
8 CDD-LIVE-D CDD-LIVE-ONE

: CDD-LIVE-READ ( -- n ) CDD-LIVE-ONE @ ;

: CDD-SECTION-LIVE ( -- )
   s" the engine's own row certifies the clause effect" T-LABEL
   s" C13 ( -- ^n ) CDD-LIVE-ONE" CDD-LIVE-VERDICT ACCEPTED T=
   s" and rejects the bare cell the scanner rejects" T-LABEL
   s" C14 ( -- n ) CDD-LIVE-ONE" CDD-LIVE-VERDICT REJECTED T=
   s" the created word holds what the definer stored" T-LABEL
   CDD-LIVE-READ 8 T= ;

\ Section 5: a definer the scanner never read. CDD-RES-D and CDD-RESP:CDD-RP-D
\ were compiled by the engine here; what they create is known only from the
\ store the checker wrote when it certified their clause. Without that store,
\ tools/check.f rejects as E-UNDEFINED a word a resident definer creates, as
\ the resident CODEGEN:BUFFER creates src/lib/ffi-callback.f's NAME. The
\ last rows are the latch's lifetime: the store is written between two checker
\ entry points. The engine checks a clause and then the definer's body; the
\ scanner checks the body and then the clause. Either order must leave a plain
\ definition with nothing, and a rejected clause nothing at all. The
\ rejection row's `error[E-DOES-TWICE]` record on stderr is the rejection
\ being measured, not a test failure.
: CDD-SECTION-RESIDENT ( -- )
   s\" package CERTIFY-DOES-DEFINER\npublic\n8 CDD-RES-D CDD-RES-ONE\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident definer's created word certifies as the clause effect" T-LABEL
   s" C15 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RES-ONE" CDD-VERDICT ACCEPTED T=
   s" and is rejected against a bare cell" T-LABEL
   s" C16 ( -- n ) CERTIFY-DOES-DEFINER:CDD-RES-ONE" CDD-VERDICT REJECTED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n8 CDD-RESP:CDD-RP-D CDD-RP-QUAL\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s\" package CERTIFY-DOES-DEFINER\npublic\nusing CDD-RESP\n7 CDD-RP-D CDD-RP-BARE\n;using\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" the qualified spelling of a resident package definer certifies" T-LABEL
   s" C17 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RP-QUAL" CDD-VERDICT ACCEPTED T=
   s" the bare spelling under `using` names the same definer" T-LABEL
   s" C18 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RP-BARE" CDD-VERDICT ACCEPTED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n9 CDD-RESX:CDD-RP-D CDD-RP-EXPORTED\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" an exported definer creates what the definer creates" T-LABEL
   s" C24 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RP-EXPORTED" CDD-VERDICT ACCEPTED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-RES-WRAP ( -- ) 3 CDD-RES-D ;\nCDD-RES-WRAP CDD-RES-WRAPMADE\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a source-read wrapper of a resident definer creates what it creates" T-LABEL
   s" C19 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RES-WRAPMADE" CDD-VERDICT ACCEPTED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\nCDD-RES-W CDD-RES-WMADE\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a RESIDENT wrapper creates what its definer creates (the walk's row)" T-LABEL
   s" C20 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RES-WMADE" CDD-VERDICT ACCEPTED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n2 CDD-RES-IFW CDD-RES-CONDMADE\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident definer behind a conditional records nothing" T-LABEL
   s" C25 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RES-CONDMADE" CDD-VERDICT DEFERRED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n3 4 CDD-RES-TWOW CDD-RES-TWICEMADE\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" two resident definer calls in one body record nothing" T-LABEL
   s" C26 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RES-TWICEMADE" CDD-VERDICT DEFERRED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n6 CDD-RES-QW CDD-RES-QUOTMADE\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident definer inside a quotation records nothing" T-LABEL
   s" C27 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-RES-QUOTMADE" CDD-VERDICT DEFERRED T= ;

: CDD-SECTION-LATCH ( -- )
   s\" package CERTIFY-DOES-DEFINER\npublic\n5 CDD-RES-P CDD-PL-MADE\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" engine order: the definition after a definer creates nothing" T-LABEL
   s" C21 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-PL-MADE" CDD-VERDICT UNRESOLVED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-SO-D ( n -- ) create , does> ( -- ^n ) ;\n: CDD-SO-P ( n -- n ) 1 + ;\n5 CDD-SO-P CDD-SO-MADE\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" source order: the definition after a definer creates nothing" T-LABEL
   s" C22 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-SO-MADE" CDD-VERDICT UNRESOLVED T= ;

\ Section 6: the rejected clause and the definition after it are live
\ definitions, so they run between two scopes.
: CDD-SECTION-LATCH-LIVE ( -- )
   s" a clause its body contradicts is rejected" T-LABEL
   [: s\" : CDD-RES-BAD ( n -- ) create , does> ( -- n ) ;\n" evaluate-closed ;]
      70 TTHROWSQ
   s\" : CDD-RES-AFTER ( n -- n ) 2 * ;\n" evaluate-closed ;

: CDD-SECTION-LATCH-AFTER ( -- )
   s\" package CERTIFY-DOES-DEFINER\npublic\n6 CDD-RES-AFTER CDD-AFTER-MADE\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" and leaves nothing for the next definition to inherit" T-LABEL
   s" C23 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-AFTER-MADE" CDD-VERDICT UNRESOLVED T= ;

\ Section 7: the wrapper latch's lifetime. The wrapper fact is learned by the
\ body walk, and three walks are not a definition's own body: a candidate, a
\ `does>` clause, and the definer body whose own clause already says what it
\ creates. Each row below is a record published right after one of those walks,
\ and each must be untouched by it.
: CDD-SECTION-WRAP-LATCH ( -- )
   s" a candidate body may be a wrapper shape and still teach nothing" T-LABEL
   s" C28 ( n -- ) CDD-RES-D" CDD-LIVE-VERDICT ACCEPTED T=
   \ Opening the scope keeps the walk's latch, so the scan meets it first.
   [:
   s\" package CERTIFY-DOES-DEFINER\npublic\n7 CDD-RES-D CDD-CAND-ONE\nCDD-CAND-ONE CDD-CAND-TWO\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" the next created word certifies as the clause effect" T-LABEL
   s" C29 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-CAND-ONE" CDD-VERDICT ACCEPTED T=
   s" and is no definer itself: the candidate walk left nothing to inherit" T-LABEL
   s" C30 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-CAND-TWO" CDD-VERDICT UNRESOLVED T=
   s\" package CERTIFY-DOES-DEFINER\npublic\n5 CDD-RES-CD CDD-CD-MADE\nCDD-CD-MADE CDD-CD-X\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a definer whose clause calls a definer creates its OWN clause" T-LABEL
   s" C31 ( -- ) CERTIFY-DOES-DEFINER:CDD-CD-MADE" CDD-VERDICT ACCEPTED T=
   s" and not the row the definer that clause calls creates" T-LABEL
   s" C32 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-CD-MADE" CDD-VERDICT REJECTED T=
   s" what that clause created is no definer either" T-LABEL
   s" C33 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-CD-X" CDD-VERDICT UNRESOLVED T=
   \ The same clause in source order, which is the order that could inherit: the
   \ pre-pass checks the definer's body first and the clause last, so the next
   \ record published is the created word's (verify-source CREATED-TRUST-NEXT?).
   s\" package CERTIFY-DOES-DEFINER\npublic\n: CDD-SRC-CD ( n -- ) create , does> ( -- ) drop 3 CDD-RES-D ;\n5 CDD-SRC-CD CDD-SRC-MADE\nCDD-SRC-MADE CDD-SRC-X\n;package\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a clause read from source arms nothing the created word takes" T-LABEL
   s" C34 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-SRC-X" CDD-VERDICT UNRESOLVED T=
   ;] CHECK:SCOPED ;

\ Section 9: a definer that writes its word as text; a `generates: D ( effect )`
\ row states what D makes. Section 10 reads FUNCTION:'s group, section 11 holds
\ a row to the scope that recorded it, section 12 is the engine rejecting a row.
\ CDD-GEN:MAKE leaves nothing a scanner can read: its word exists only once the
\ text it builds is evaluated, which the pre-pass never does. The row states the
\ effect on the private definer, and the public wrapper inherits it as section
\ 2's wrapper does. The row may sit before or after `public`: either way the
\ private definer is the only CDD-GD* defined when the row is read.
: CDD-SECTION-GENERATES ( -- ) [:
   s\" package CDD-GQ1\n: CDD-GD1 ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-GD1 ( -- ^n )\npublic\n: CDD-GD1 ( n -- ) CDD-GD1 ;\n;package\n5 CDD-GQ1:CDD-GD1 CDD-GD1-ONE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a generated word certifies as its row's effect" T-LABEL
   s" C45 ( -- ^n ) CDD-GD1-ONE" CDD-VERDICT ACCEPTED T=
   s" and is rejected against a bare cell" T-LABEL
   s" C46 ( -- n ) CDD-GD1-ONE" CDD-VERDICT REJECTED T=
   s\" package CDD-GQ2\n: CDD-GD2 ( n -- ) CDD-GEN:MAKE ;\npublic\ngenerates: CDD-GD2 ( -- ^n )\n: CDD-GD2 ( n -- ) CDD-GD2 ;\n;package\n5 CDD-GQ2:CDD-GD2 CDD-GD2-ONE\n"
      VERIFY:SOURCE-BUF-IN-SCOPE
   s" a row after `public` names the private definer too" T-LABEL
   s" C47 ( -- ^n ) CDD-GD2-ONE" CDD-VERDICT ACCEPTED T=
   s" C48 ( -- n ) CDD-GD2-ONE" CDD-VERDICT REJECTED T=
   s\" 5 CDD-GQ4:CDD-GD4 CDD-GD4-MADE\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a resident row reaches a source that only calls its wrapper" T-LABEL
   s" C49 ( -- ^n ) CDD-GD4-MADE" CDD-VERDICT ACCEPTED T=
   s" C50 ( -- n ) CDD-GD4-MADE" CDD-VERDICT REJECTED T=
   ;] CHECK:SCOPED ;

\ The resident definer run for real: CDD-GD4-LIVE is the word the evaluated text
\ defines, checked against the effect that text declares, so the row is pinned
\ against the generated word rather than against itself.
8 CDD-GQ4:CDD-GD4 CDD-GD4-LIVE

: CDD-SECTION-GENERATES-LIVE ( -- )
   s" the generated word certifies as the effect its row states" T-LABEL
   s" C51 ( -- ^n ) CDD-GD4-LIVE" CDD-VERDICT ACCEPTED T=
   s" and rejects the bare cell the row rejects" T-LABEL
   s" C52 ( -- n ) CDD-GD4-LIVE" CDD-VERDICT REJECTED T= ;

\ Section 10: FUNCTION:.
\ src/lib/ffi-abi.f FUNCTION: makes its word from the declaration group, so the
\ pre-pass reads the group as the word's effect, with the one rewrite the
\ declarer applies: an `i32` result is a cell. The live declaration is the pin.
PROCESS-SYMBOLS
FUNCTION: CDD-FN getpid ( -- i32 ) ;FUNCTION

: CDD-SECTION-FUNCTION ( -- )
   s" a declared function certifies as its group, the result a cell" T-LABEL
   s" C53 ( -- n ) CDD-FN" CDD-VERDICT ACCEPTED T=
   s" C54 ( -- ^n ) CDD-FN" CDD-VERDICT REJECTED T=
   [:
   s\" package CERTIFY-DOES-DEFINER\npublic\nFUNCTION: CDD-FN-READ getpid ( -- i32 ) ;FUNCTION\n;package\n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" and the pre-pass reads the same effect from the declaration" T-LABEL
   s" C55 ( -- n ) CERTIFY-DOES-DEFINER:CDD-FN-READ" CDD-VERDICT ACCEPTED T=
   s" C56 ( -- ^n ) CERTIFY-DOES-DEFINER:CDD-FN-READ" CDD-VERDICT REJECTED T=
   ;] CHECK:SCOPED ;

\ Section 11: a row lives as long as its scope. A popped scope hands its symbol
\ ids out again, so a row outliving it would make that id's next word a definer.
TYPED-VARIABLE CDD-SRC-A ^u8
variable CDD-SRC-U

\ x86-64 rejects a quotation that reads a local, so the span travels to the
\ caught read through these two cells.
: CDD-READ ( -- )
   CDD-SRC-A @ CDD-SRC-U @ >str VERIFY:SOURCE-BUF-IN-SCOPE ;

: CDD-SPAN! ( str -- )
   {: s:str :}
   s STR:DATA CDD-SRC-A !
   s STR:LEN CDD-SRC-U ! ;

\ A neutral scope that pops, as tools/check.f reads a source.
: CDD-SCOPED ( str -- n )
   CDD-SPAN!
   [: [: CDD-READ ;] CHECK:SCOPED ;] catch ;

: CDD-REPLAY$ ( -- str )
   s\" : CDD-SG ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-SG ( -- ^n )\n5 CDD-SG CDD-SGM\n: CDD-SGU ( -- ^n ) CDD-SGM ;\n" ;

\ The stale pair's E-UNDEFINED line on stderr is the rejection being measured.
: CDD-SECTION-SCOPES ( -- )
   s" a source read in two scopes takes its row once in each" T-LABEL
   CDD-REPLAY$ CDD-SCOPED 0 T=
   CDD-REPLAY$ CDD-SCOPED 0 T=
   s" the stale pair's definer reads in a scope that pops" T-LABEL
   s\" : CDD-SD ( n -- ) create , does> ( -- ^n ) ;\n" CDD-SCOPED 0 T=
   s" and its row does not outlive the ids that scope gave out" T-LABEL
   s\" : CDD-SE ( -- n ) 5 ;\nCDD-SE CDD-SF\n: CDD-SU ( -- ^n ) CDD-SF ;\n" CDD-SCOPED 70 T= ;

\ Section 12: the engine rejects a row it cannot keep with an E-GENERATES-ROW
\ record that names the row's word and why. A row on a word that reads no name
\ after it, `evaluate` or a deferred word is rejected too: kept, it makes the
\ pre-pass take the token after a call as a made name the load never makes.
: CDD-ROW-REJECTED ( [ -- ] str -- )
   {: ws:str :}
   REJECTION:CAPTURE
   70 TTHROWSQ
   REJECTION:BUFFER$ s" error[E-GENERATES-ROW]" STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ ws STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF ;

: CDD-SECTION-GENERATES-REJECTED ( -- )
   s" a row read before its definer is defined is rejected" T-LABEL
   [: s\" generates: CDD-GL ( -- ^n )\n: CDD-GL ( n -- ) CDD-GEN:MAKE ;\n" evaluate-closed ;]
      s" `CDD-GL`, which is not defined here" CDD-ROW-REJECTED
   s" a row on a definer whose clause states its word is rejected" T-LABEL
   [: s\" generates: CDD-RES-D ( -- ^n )\n" evaluate-closed ;]
      s" `CDD-RES-D`, which already states what it makes" CDD-ROW-REJECTED
   s\" : CDD-G2 ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-G2 ( -- ^n )\n" evaluate-closed
   s" a second row on one definer is rejected" T-LABEL
   [: s\" generates: CDD-G2 ( -- ^n )\n" evaluate-closed ;]
      s" `CDD-G2`, which already states what it makes" CDD-ROW-REJECTED
   s" an effect the checker cannot parse is rejected" T-LABEL
   [: s\" : CDD-GI ( n -- ) CDD-GEN:MAKE ;\ngenerates: CDD-GI ( -- i32 )\n" evaluate-closed ;]
      s" `CDD-GI` with an effect that cannot be recorded" CDD-ROW-REJECTED
   s" a row on a word that reads no name after it is rejected" T-LABEL
   [: s\" : CDD-GN ( n -- ) drop ;\ngenerates: CDD-GN ( -- ^n )\n" evaluate-closed ;]
      s" `CDD-GN`, which is deferred or reads no name" CDD-ROW-REJECTED
   s" a row on a deferred word is rejected" T-LABEL
   [: s\" defer CDD-GF ( n -- )\ngenerates: CDD-GF ( -- ^n )\n" evaluate-closed ;]
      s" `CDD-GF`, which is deferred or reads no name" CDD-ROW-REJECTED
   s" a row on evaluate, which reads no name, is rejected" T-LABEL
   [: s\" generates: evaluate ( -- )\n" evaluate-closed ;]
      s" `evaluate`, which is deferred or reads no name" CDD-ROW-REJECTED ;

\ Section 13: a clause's body ends as its colon twin's does (checker.f
\ CHECK-DOES-RUN); rejected clauses' diagnostics are captured, not printed.
defer CDD-VSAME ( mut-view<p,q,a,u8> mut-view<p,q,a,u8> -- )

: CDD-NOT-CERTIFIED ( str str -- )
   {: ss:str ws:str :}
   REJECTION:CAPTURE
   ss TEST-EVAL:RC  70 T=
   REJECTION:BUFFER$ ws STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF ;

: CDD-NP-REJECTED ( str -- )
   s" E-NONPARAMETRIC-EFFECT" CDD-NOT-CERTIFIED ;

: CDD-SECTION-CLAUSE-TAIL ( -- )
   s" a clause that specializes a declared quantifier is rejected" T-LABEL
   s" : CDD-NP1 ( n -- ) create , does> ( a -- n ) drop ;" CDD-NP-REJECTED
   s" a clause that unifies two declared quantifiers is rejected" T-LABEL
   s" : CDD-NP2 ( n -- ) create , does> ( mut-view<p,q,a,u8> mut-view<p,q,c,u8> -- ) drop CDD-VSAME ;"
      CDD-NP-REJECTED
   s" a clause that unifies two declared rows is rejected" T-LABEL
   s" : CDD-NP3 ( n -- ) create , does> ( R -- S ) drop ;" CDD-NP-REJECTED
   s" a clause that claims a quotation's output row without running it is rejected" T-LABEL
   s" : CDD-NP4 ( n -- ) create , does> ( R [ R -- S ] -- S ) drop drop ;" CDD-NP-REJECTED
   \ The return-stack seal's, not the parametric pass's (checker.f RS-BORROWS?).
   s" a clause that reads below its declared return row is rejected at the read" T-LABEL
   s" : CDD-RB1 ( n -- ) create , does> ( | R -- | R ) drop r> drop 5 throw ;"
      s" error[E-RETURN-STACK]: return stack: this reads below the return-stack cells pushed here" CDD-NOT-CERTIFIED
   s" a clause's unknown signature type is reported at that type, as its colon twin's is" T-LABEL
   s" : CDD-UX ( n -- ) create , does> ( cdd-zz -- cdd-zz ) drop drop 5 ;"
      s" error[E-UNKNOWN-SIGNATURE-TYPE]: unknown type `cdd-zz`" CDD-NOT-CERTIFIED
   s" compiled natively, the clause's unknown type is placed at that type in its text" T-LABEL
   REJECTION:CAPTURE
   s" -1 REJECTION:JSON ! : CDD-UXJ ( n -- ) create , does> ( cdd-zz -- cdd-zz ) drop drop 5 ;"
      TEST-EVAL:RC  70 T=
   REJECTION:BUFFER$ s\" \"code\":\"E-UNKNOWN-SIGNATURE-TYPE\"" STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ s\" \"label\":\"unknown type\"" STR:CONTAINS? TTRUE
   \ The `}` closes the evaluated object: the token is the text's, not the file's.
   REJECTION:BUFFER$ s\" \"token\":\"cdd-zz\"}" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF
   s" 0 REJECTION:JSON !" TEST-EVAL:RC 0 T=
   s" a clause that never returns owes no output" T-LABEL
   s" : CDD-NORET ( n -- ) create , does> ( n -- ^u8 ) drop 5 throw ;" TEST-EVAL:RC 0 T= ;

\ Section 14: a clause naming a type a reached renderer may declare is deferred
\ unwalked at that type, as its colon twin is (verify-source.f NOMINAL-MARK).
: CDD-QUIET$ ( -- str )
   s\" package CDD-QP\n: CDD-QREG ( -- ) s\" NEWTYPE cdd-qt\" evaluate-closed ;\nCDD-QREG\n: CDD-QB ( n -- ) create , does> ( cdd-qt -- cdd-qt ) drop drop 5 ;\n;package\n" ;

: CDD-SECTION-QUIET-CLAUSE ( -- )
   s" a clause naming a type a renderer may declare is deferred at that type" T-LABEL
   true VERIFY:REPORT-DEFERRALS
   REJECTION:CAPTURE
   [: [: CDD-QUIET$ s" cdd-quiet.f" s" cdd-quiet.f" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] CHECK:SCOPED ;] catch
   0 T=
   REJECTION:BUFFER$ s" warning[W-CHECK-DEFERRED]: the check cannot follow the source from `cdd-qt`" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF
   VERIFY:CENSUS 1 T= drop ;

\ Section 15: a does> word acts for the word it creates, so one that creates
\ none is rejected at its does>.
: CDD-SECTION-PATCH ( -- )
   s" a does> word that creates no word is rejected at its does>" T-LABEL
   s" : CDD-PB1 ( -- ) does> ( -- n ) drop 9 ;"
      s" `does>` needs a word this definition creates" CDD-NOT-CERTIFIED
   s" a head that bends before a word's first call is still rejected" T-LABEL
   s" : CDD-PB5 ( -- ) false if then depth drop create 5 , does> ( -- n ) @ ;"
      s" `does>` needs a word this definition creates" CDD-NOT-CERTIFIED ;

\ Section 16: the qualified definer's created word gives the clause's cells.
: CDD-SECTION-QUALIFIED ( -- )
   s" a definer named like an hb word from outside its package runs" T-LABEL
   CDD-QF + 14 T= ;

\ Each scan runs with its probes in one scope that opens no package: a source
\ naming this file's private fixtures opens the package and is probed qualified.
T-RESET
' CDD-SECTION-DEFINER CHECK:SCOPED
CDD-SECTION-WRAPPER
' CDD-SECTION-LATCH CHECK:SCOPED
\ A section that defines live words runs outside a scope, which rejects a
\ definition (E-OVERLAY-OPEN).
CDD-SECTION-LATCH-LIVE
' CDD-SECTION-LATCH-AFTER CHECK:SCOPED
CDD-SECTION-WRAP-LATCH
' CDD-SECTION-NOT-A-WRAPPER CHECK:SCOPED
CDD-SECTION-LIVE
' CDD-SECTION-RESIDENT CHECK:SCOPED
\ Section 11 runs before section 9, whose statements mark the global wordlist
\ and would leave the stale pair's name to a run (checker.f UNSEEN-COVERS?).
CDD-SECTION-SCOPES
CDD-SECTION-GENERATES
CDD-SECTION-GENERATES-LIVE
CDD-SECTION-FUNCTION
CDD-SECTION-GENERATES-REJECTED
CDD-SECTION-CLAUSE-TAIL
CDD-SECTION-QUIET-CLAUSE
CDD-SECTION-PATCH
CDD-SECTION-QUALIFIED
T-REPORT

;package
