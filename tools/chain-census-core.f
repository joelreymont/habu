\ chain-census-core.f - how much of the real tree the native chain can already
\ compile, measured rather than read. One concern: driving every checked colon
\ definition of a file through src/compiler/native/migrate.f and counting what
\ came back.
\
\ WHY A CENSUS AT ALL. The native chain models a SUBSET of Habu - the words
\ src/compiler/native/hir-word.f REGISTER-WORDS declares and nothing else - and
\ the question that decides what gets built next is which missing shape blocks
\ the most real definitions. That is a measurement and not a reading: a reader
\ counts the words a body mentions, and the chain refuses for reasons a reader
\ cannot see - register pressure, a control structure the elaborator cannot
\ close, a body that does not leave the declared outputs. So every verdict here
\ is the chain's own, obtained by running it, and every reason is the error the
\ refusing stage threw.
\
\ THE INSTRUMENT IS NMIGRATE:MEASURE-HELD. It hands one definition's source to
\ the engine, which compiles and CERTIFIES it and publishes nothing; the chain
\ then compiles the tape the checker's own reader filled while it certified that
\ body, and the publication seam makes every refusal it can make against the
\ finished emission. A refusal anywhere throws the refusing stage's own code and
\ leaves nothing behind. That is why the census can ask about a definition
\ without changing what the definition means.
\
\ AND IT KEEPS NOTHING WHEN THE ANSWER IS YES EITHER, WHICH IS WHAT MAKES THE
\ COMPILED COUNT A MEASUREMENT. A migration that COMMITS its emission spends a
\ code slot, a row of the clobber record and a row of the replacement log per
\ definition, and neither record may drop a row to make space - a row is the
\ whole of what a caller compiled against it. So a census that committed ran out
\ of table long before it ran out of tree, and reported the size of the table it
\ filled first: the 2026-08-07 run compiled EXACTLY 128 definitions of lib/ and
\ refused 1275 more with the clobber record's E-NCLOB-CAP, every one of them
\ after selection, allocation, verification and emission had all accepted it.
\ Measuring stops one step short of those writes, so what the count reports is
\ what the chain can compile.
\
\ THE SUBJECT IS THE BODY, NOT THE NAME, AND THAT IS FORCED. A tail may be
\ defined once in a wordlist (src/habu/habu2.f C-REJECT-DUP-DEF), and every
\ definition the census measures is already in the dictionary by the time it is
\ measured - the census LOADS the file first, precisely so the checker can answer
\ about it. So a definition cannot be re-driven under its own name, and the
\ census drives its body under a fresh name of its own. Production already does
\ exactly this: src/compiler/native/reach.f's header says a migrated word "is
\ published as a record of its own, in a package of its own" for this same
\ reason.
\
\ WHAT THE RENAME COSTS, PLAINLY. The census measures a BODY under a name nobody
\ calls. It therefore cannot see anything that depends on the definition's own
\ identity: recursion written by name rather than by RECURSE reaches the OLD
\ record and is compiled as an ordinary call to it, and no caller-side effect of
\ the real name is exercised at all. Those are the two blind spots and there are
\ no others - every other token the chain elaborates is the token the checker
\ certified for this body.
\
\ AND THE PACKAGE IS REOPENED AROUND THE RUN, ONCE PER SECTION. A definition's
\ callees are usually private to its package, so a body driven at top level dies
\ at the ENGINE with an unresolved name before the chain ever sees it - which
\ would score a perfectly modellable definition as a refusal. Reopening is a
\ supported mechanism and tools/codegen-compare-migrated.f uses it for the same
\ reason. It is done once per contiguous package section rather than once per
\ definition, because `;package` does real work and a file with two hundred
\ definitions in one package must not open and close it two hundred times.
\
\ THE DECLARED ARITY COMES FROM THE CHECKER. Counting terms in the `( ... )`
\ signature comment is WRONG - `( ptr a len idx -- a )` is THREE inputs, because
\ `ptr a` is one value - and the checker already knows, because it parsed the
\ effect during the scan that filled the tape. The query is made INSIDE the
\ reopened package with the BARE name, which is the one form that answers for
\ both public and private words: asked at top level a private word answers
\ nothing, and asked qualified inside its own package it answers nothing either.
\ That is why the query lives in the same package window the migration runs in
\ rather than beside the walk.
\
\ WHAT THE CENSUS CANNOT SEE, WRITTEN DOWN SO THE NEXT READER DOES NOT
\ REDISCOVER IT.
\   - A word whose name the seal has stripped (src/habu/treeshake.f and
\     src/habu/habu1.f are the visible cases) holds no queryable effect, so it
\     lands in the no-certified-effect population. That is a limit of the
\     instrument, not a refusal by the chain, and it is never counted as one.
\   - `TRUSTED:` and the other colon-family definers are unchecked or are not the
\     form the production entry compiles, so they fill no tape and cannot be
\     held-compiled at all. They are a counted population line, never a silent
\     drop.
\   - Every definition of a package the engine will not let anything reopen, which
\     is two separate seals. The wordlists of every substrate file are protected at
\     the foot of the file; and seven package names are reserved outright by a
\     baked table in the compiler, protected or not. Both refusals END THE PROCESS
\     rather than throwing, so both are detected before the attempt - the first
\     through the engine's own protected-wordlist bitmap, the second through a
\     mirror of its table that the suite holds against the engine's source - and
\     counted rather than tried. That is a large part of src/ and it is the
\     instrument's biggest blind spot; measuring it needs a way to compile a
\     definition in a package's scope without publishing into it.
\   - A body whose callee was made visible by a `using` at the top of its file.
\     `using` lasts for the file that wrote it and no longer (measured: the bare
\     name stops resolving the moment the file finishes loading), and the census
\     reopens the definition's PACKAGE, not its file's imports - so such a body
\     is refused by the ENGINE with an unresolved name and lands in the census's
\     own self-check count, where it is visible rather than mistaken for a gap.
\   - The two blind spots of the rename, above.
\
\ ONE MEASURED FINDING ABOUT THE ERROR TABLE, recorded here because it otherwise
\ looks like a missing bucket. E-HIR-KIND names a source-tape token kind the
\ subset does not model, and the tape HAS kinds for a character and a string
\ literal - but nothing fills them: src/compiler/native/feed.f APPEND writes only
\ the reader's three classes, name, integer and real. So a body containing a
\ string literal is refused E-HIR-UNMODELED at the token spelled `s"`, not
\ E-HIR-KIND. The bucket is kept because the kind exists and a later reader may
\ record it; today it counts zero, and that zero is a fact rather than an
\ oversight.
\
\ RUN IT FROM THE REPOSITORY ROOT, WITH NO PACKAGE OPEN. Every path it is given is
\ handed to `required` and to the file reader unchanged, and the tree's own
\ require paths are relative to the root. The second half is a requirement and not
\ a style: this tool LOADS the files it measures, and a file that opens a package
\ inside an already-open one is source the engine refuses outright - it exits on
\ the nested opener, before anything catchable happens. There is no way to ask the
\ engine whether a package is open (the checker's depth cell is name-stripped at
\ seal), so this cannot be a refusal here; it is a contract, and both the CLI and
\ the suite keep it by calling in after `;package`.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/sort.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/def.f
require tools/prot-wid-probe.f
require src/compiler/native/migrate.f

package CHAIN-CENSUS
private

\ ---- the register budget ------------------------------------------------------
\ Not a guess and not a taste: eighteen is the WHOLE pool the ABI can describe.
\ src/compiler/native/abi.f POOL hands the allocator `n` general registers from
\ x0, and x18 is platform-reserved, so a contract asking for nineteen cannot be
\ built at all - src/compiler/a64-effect.f refuses it with E-A64EFF-GPR. Measured
\ on a counted loop: budgets four through eighteen behave, nineteen and up throw
\ that refusal. Choosing the maximum is what makes a register-pressure refusal a
\ fact about the program instead of a fact about how stingy the census was.
18 constant REGS

\ ---- capacities ---------------------------------------------------------------
\ Each is a ceiling this tool refuses at rather than overruns, and each is sized
\ against the tree it is pointed at: on the order of ten thousand colon
\ definitions across some fifteen hundred files. A run that outgrows one says
\ WHICH one and refuses, rather than writing past a buffer.
\
\ THE SPELLING CEILING IS THE SURPRISING ONE and was measured rather than guessed.
\ A call to any ordinary word is refused with that word's own spelling, so the
\ sub-histogram's key space is not "the words the dialect is missing" - a few
\ dozen - but every word the censused bodies name, which is most of the tree's
\ vocabulary. Two hundred and fifty-six was exhausted by tools/lint alone.
16384 constant DEFS-MAX               \ definitions one census run records
2048 constant FILES-MAX               \ files one census run visits
$100000 constant POOL-CAP             \ bytes of names, paths and spellings kept
8192 constant SPELLS-MAX              \ distinct refused spellings one run holds
64 constant BUCKETS-MAX               \ distinct refusal codes one run holds
20 constant DENSE-SHOWN               \ files listed in the density table
64 constant NAME-CAP                  \ the longest package or subject name held

\ The staging buffer for one renamed definition. The number it has to clear is
\ the RECORDER's own ceiling, which is the engine's body capture
\ (src/compiler/native/migrate.f TEXT-CAP, BODYBUF-CAP, 8000 bytes): a source
\ shorter than that is one the recorder will take, so the staging buffer has to
\ hold it or the census would report its own capacity as a refusal.
\
\ AND THE TWO LENGTHS ARE NOT THE SAME LENGTH, which is the thing to know before
\ trusting this number. A definition's SOURCE is what the census stages, and it
\ can be far longer than the body the engine CAPTURES from it: the capture leaves
\ out backslash comments, the indentation and the line breaks, so a definition
\ that loads perfectly well may still stage many kilobytes. Twice the recorder's
\ ceiling is the headroom, the longest definition in src or lib stages 2948 bytes
\ (measured 2026-08-14), and a source past this buffer stops the run by name
\ rather than being truncated into a measurement.
$4000 constant SRC-CAP

\ ---- per-file status ----------------------------------------------------------
\ Why a file produced no definitions, when it produced none. Each is a counted
\ line of the report; none is ever folded into a refusal.
\ They are public because FILE-STATUS answers one of them and a caller reading
\ that answer needs the names, not the numbers.
public
0 constant ST-OK
1 constant ST-NOT-SOURCE              \ not a Habu source path
2 constant ST-LOAD-REFUSED            \ `required` threw: the file will not load
3 constant ST-LEX-ERROR               \ the shared lexer refused the text
4 constant ST-STRUCTURE               \ package or definition nesting the census cannot replay
5 constant ST-MISSING                 \ no file at that path
6 constant ST-SUITE                   \ a suite rather than a source
private

\ ---- the classes a refusal falls into -----------------------------------------
\ A refusal is not one kind of news. A dialect gap is work for the chain; register
\ pressure is a fact about the program and the frame; a recorder ceiling is a fact
\ about the instrument; and an engine-level refusal means the CENSUS is broken,
\ not the chain. Folding them into one histogram is how a stingy budget or a
\ mis-resolved name comes to be read as a missing language feature, so they are
\ kept apart and subtotalled apart. They are public because the taxonomy is a
\ claim about every code the chain can throw, and a claim has to be checkable from
\ outside: CLASS-OF below answers for any code, named or not.
public
0 constant CL-DIALECT
1 constant CL-PRESSURE
2 constant CL-INSTRUMENT
3 constant CL-SELF-CHECK
private

\ The engine's own refusal statuses, as `catch` reports them. A name the reopened
\ package did not resolve comes back as 70 and a colliding rename as 78; both mean
\ the census's own scaffolding failed for that definition, so a nonzero count in
\ either is the census reporting on ITSELF and must never be read as a dialect
\ gap.
public
70 constant RC-UNDEFINED
78 constant RC-DUPLICATE
private

\ ---- the two boundaries this file needs ---------------------------------------
\ `evaluate` is the metaprogramming boundary the checker does not model, and it is
\ the only way to open and close a package scope from inside a word: `package` and
\ `;package` are interpreted words that parse from the input stream.
\ src/compiler/native/migrate.f wraps the same primitive the same way.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

\ The checker's effect-read export is name-stripped past the seal, so it is
\ reachable only from an unchecked boundary - the shape src/compiler/native/reach.f
\ and test/effect-read-api-test.f both use. One boundary, both halves of the
\ answer, and a name the checker holds no effect for answers a pair nothing can be
\ mistaken for.
\
\ IT ASKS FOR CELLS, because that is what the number is used AS. The census hands
\ this pair to NELAB:COLON as the definition's declared arity, and the elaborator
\ checks the body's compile-time value vector against it - a count of CELLS. The
\ TERM count is a different number whenever a signature carries a term wider than
\ one cell, and handing the term count over states an arity the body cannot match,
\ which the elaborator then refuses as E-NELAB-ARITY: the census reporting a
\ dialect verdict on its own arithmetic. Dot habu-export-the-checker-2bbc831c
\ published the widths; this reads them.
TRUSTED: EFFECT ( ptr u8 n -- n n )
   EFFECT-QUERY if EFFECT-DIN-CELLS EFFECT-DOUT-CELLS else -1 -1 then ;

-1 constant EFFECT-NONE

\ ---- the one seam onto the elaborator's refused token --------------------------
\ WHICH WORD the dialect could not compile is the ELABORATOR's answer and nobody
\ else's: it is the token its walk was standing on when it threw. The census must
\ not re-derive it. Re-lexing the body and guessing which spelling the model lacks
\ would silently disagree with the chain the moment the two readers differ, which
\ is exactly the failure this whole tool exists to prevent. So the spelling comes
\ from the refusal itself, and it arrives through exactly one word here, so the
\ census depends on the elaborator's published surface in one place rather than
\ three.
: REFUSED-SPELL$ ( -- ptr u8 n )
   NELAB:REFUSED$ ;

\ The same seam's other half. The spelling above is EMPTY in two different states
\ - the refusal stood on no body token at all, and the token's spelling was longer
\ than the record holds - and the two have to be told apart, or a refusal with no
\ nameable spelling would simply vanish from the sub-histogram and the counts
\ underneath the named-token buckets would stop adding up to them. The row is
\ what separates them.
: REFUSED-AT ( -- n )
   NELAB:REFUSED-ROW ;

\ And its clear. The elaborator empties the record when its walk is ENTERED, so a
\ definition refused BEFORE that - the engine declining the source while it is
\ still resolving names - leaves the previous definition's word sitting there.
\ Only the driver knows an attempt was made, so the driver is what clears it, once
\ per attempt. With this, "the record describes the definition just offered" is a
\ property of the loop rather than an argument about who throws which code.
: REFUSED-CLEAR ( -- )
   NELAB:REFUSED-RESET ;

\ ---- the byte pool ------------------------------------------------------------
\ Names, paths and spellings are spans of buffers that get reused - the shared
\ source slab is overwritten by the next file, and every lexer token points into
\ it - so every string the census keeps is copied here first and referred to by
\ offset and length afterwards.
\ A ceiling reached says which ceiling it was. A bare bounds throw out of a tool
\ that holds six of them sends the reader to the wrong constant, and this one is
\ meant to be re-run over a growing tree, so outgrowing a table is an expected
\ event rather than a defect.
create CAP-NL 1 allot

: CAP-FULL ( ptr u8 n -- ) {: a:ptr u:n :}
   10 CAP-NL c!
   2 s" chain-census: ran out of " LINT-FD-WRITE
   2 a u LINT-FD-WRITE
   2 CAP-NL 1 LINT-FD-WRITE
   E-TBL-BOUNDS throw ;

create POOL POOL-CAP allot
variable POOL-U

: POOL-RESET ( -- )
   0 POOL-U ! ;

: POOL+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   POOL-U @ u + POOL-CAP > if s" POOL-CAP" CAP-FULL then
   POOL-U @ {: off:n :}
   a POOL off + u BYTE-COPY
   off u + POOL-U !
   off ;

: POOL$ ( n n -- ptr u8 n ) {: off:n u:n :}
   POOL off + u ;

\ ---- reading and writing one cell of a parallel table --------------------------
: T@ ( ptr a n -- n ) {: t:ptr k:n :}
   k cells t + @ ;

: T! ( n ptr a n -- ) {: v:n t:ptr k:n :}
   v k cells t + ! ;

: T+ ( n ptr a -- ) {: k:n t:ptr :}
   t k T@ 1+ t k T! ;

\ ---- what happened to one definition ------------------------------------------
\ One row per definition the census offered to the chain. The code is the chain's
\ own: zero means it compiled, anything else is the refusing stage's error. The
\ spelling is filled in only where the refusal names one.
\
\ THE STAGED LENGTH IS RECORDED BECAUSE ONE REFUSAL CLASS IS ABOUT IT. The
\ recorder refuses a source longer than its text buffer, and a report that says
\ only "refused for length" cannot say by how much or whether a bigger buffer
\ would clear the row. The number kept here is the exact byte count handed to
\ NMIGRATE:MEASURE-HELD - not a length re-derived from the file - so a reader
\ comparing it against the recorder's ceiling is comparing the two quantities
\ that were actually compared.
create D-FILE DEFS-MAX cells allot
create D-NAME-OFF DEFS-MAX cells allot
create D-NAME-U DEFS-MAX cells allot
create D-CODE DEFS-MAX cells allot
create D-SPELL-OFF DEFS-MAX cells allot
create D-SPELL-U DEFS-MAX cells allot
create D-IN DEFS-MAX cells allot
create D-OUT DEFS-MAX cells allot
create D-SRC-U DEFS-MAX cells allot
variable D-N

: D-CK ( n -- n ) {: k:n :}
   k 0 < k D-N @ >= or if E-TBL-BOUNDS throw then
   k ;

: DEF+ ( n n n n n n n -- )
   {: file:n noff:n nu:n code:n in:n out:n srcu:n :}
   D-N @ DEFS-MAX >= if s" DEFS-MAX" CAP-FULL then
   D-N @ {: k:n :}
   file D-FILE k T!
   noff D-NAME-OFF k T!
   nu D-NAME-U k T!
   code D-CODE k T!
   0 D-SPELL-OFF k T!
   0 D-SPELL-U k T!
   in D-IN k T!
   out D-OUT k T!
   srcu D-SRC-U k T!
   k 1+ D-N ! ;

: DEF-SPELL! ( n n -- ) {: off:n u:n :}
   D-N @ 1- {: k:n :}
   off D-SPELL-OFF k T!
   u D-SPELL-U k T! ;

\ ---- what happened to one file -------------------------------------------------
create F-PATH-OFF FILES-MAX cells allot
create F-PATH-U FILES-MAX cells allot
create F-STATUS FILES-MAX cells allot
create F-COLON FILES-MAX cells allot        \ plain `:` definitions found
create F-NOTCOLON FILES-MAX cells allot     \ definers that are not a plain `:`
create F-NOEFFECT FILES-MAX cells allot     \ no certified effect to declare
create F-CLOSED FILES-MAX cells allot       \ in a package the census may not reopen
create F-COMPILED FILES-MAX cells allot
create F-REFUSED FILES-MAX cells allot
variable F-N

: F-CK ( n -- n ) {: k:n :}
   k 0 < k F-N @ >= or if E-TBL-BOUNDS throw then
   k ;

: FILE+ ( n n -- ) {: off:n u:n :}
   F-N @ FILES-MAX >= if s" FILES-MAX" CAP-FULL then
   F-N @ {: k:n :}
   off F-PATH-OFF k T!
   u F-PATH-U k T!
   ST-OK F-STATUS k T!
   0 F-COLON k T!  0 F-NOTCOLON k T!  0 F-NOEFFECT k T!
   0 F-CLOSED k T!  0 F-COMPILED k T!  0 F-REFUSED k T!
   k 1+ F-N ! ;

: CUR-FILE ( -- n )
   F-N @ 1- ;

: F$ ( n -- ptr u8 n ) {: k:n :}
   F-PATH-OFF k T@  F-PATH-U k T@ POOL$ ;

: F-EXAMINED@ ( n -- n ) {: k:n :}
   F-COMPILED k T@  F-REFUSED k T@ + ;

: STATUS! ( n -- ) {: st:n :}
   st F-STATUS CUR-FILE T! ;

\ ---- the paths this run was pointed at -----------------------------------------
\ Collected first and SORTED before anything is censused. The repository's
\ directory walk answers in the host's own order, so a census whose report order
\ came from the walk would produce a different report on a different machine - and
\ this tool's whole job is to be re-run and compared against its previous self,
\ where a reordering reads exactly like real movement in the numbers.
create P-OFF FILES-MAX cells allot
create P-U FILES-MAX cells allot
variable P-N

create P-ORD VEC-HEADER-CELLS cells allot
create BK-ORD VEC-HEADER-CELLS cells allot
create SP-ORD VEC-HEADER-CELLS cells allot
create DN-ORD VEC-HEADER-CELLS cells allot
variable TABLES-READY

: TABLES-INIT ( -- )
   TABLES-READY @ if exit then
   P-ORD FILES-MAX VEC-COUNT VEC-INIT
   BK-ORD BUCKETS-MAX VEC-COUNT VEC-INIT
   SP-ORD SPELLS-MAX VEC-COUNT VEC-INIT
   DN-ORD FILES-MAX VEC-COUNT VEC-INIT
   true TABLES-READY ! ;

: P$ ( n -- ptr u8 n ) {: k:n :}
   P-OFF k T@  P-U k T@ POOL$ ;

: P-BEFORE? ( n n -- bool ) {: l:n r:n :}
   l P$ r P$ LINT-ORDER:CMP-CI {: c:n :}
   c 0 <> if c 0 < exit then
   l r < ;

: PATH-KEEP ( ptr u8 n -- ) {: a:ptr u:n :}
   P-N @ FILES-MAX >= if s" FILES-MAX" CAP-FULL then
   a u POOL+ P-OFF P-N @ T!
   u P-U P-N @ T!
   P-N @ 1+ P-N ! ;

\ ---- the report tables, derived at the end -------------------------------------
create BK-CODE BUCKETS-MAX cells allot
create BK-COUNT BUCKETS-MAX cells allot
variable BK-N

create SP-OFF SPELLS-MAX cells allot
create SP-U SPELLS-MAX cells allot
create SP-COUNT SPELLS-MAX cells allot
variable SP-N

\ ---- the named codes ------------------------------------------------------------
\ Every code the chain is known to refuse with, its class, and the words a reader
\ needs beside the number. A code that is NOT here is not swallowed: the report
\ prints it raw, on its own line, because the census discovering a reason nobody
\ predicted is a result.
24 constant CODE#

: CODE-AT ( n -- n )
   case
      0 of E-HIR-UNMODELED endof
      1 of E-HIR-KIND endof
      2 of E-NELAB-CTRL endof
      3 of E-NELAB-JOIN endof
      4 of E-NELAB-LOCAL endof
      5 of E-NELAB-LOCAL-CAP endof
      6 of E-NELAB-BLOCK endof
      7 of E-NELAB-BUNDLE endof
      8 of E-NELAB-QUOT endof
      9 of E-A64RA-SPILL endof
      10 of E-A64RA-POOL endof
      11 of E-A64RA-EDGE endof
      12 of E-A64RA-PRESSURE endof
      13 of E-NELAB-ARITY endof
      14 of E-NMIGRATE-TEXT endof
      15 of E-NMIGRATE-STATE endof
      16 of E-NMIGRATE-NAME endof
      17 of E-NMIGRATE-VERDICT endof
      18 of E-NFEED-STATE endof
      19 of E-NFEED-TEXT endof
      20 of E-NCLOB-CAP endof
      21 of E-NPUB-CAP endof
      22 of RC-UNDEFINED endof
      23 of RC-DUPLICATE endof
      E-TBL-BOUNDS throw
   endcase ;

: CODE-NAME$ ( n -- ptr u8 n )
   case
      0 of s" E-HIR-UNMODELED" endof
      1 of s" E-HIR-KIND" endof
      2 of s" E-NELAB-CTRL" endof
      3 of s" E-NELAB-JOIN" endof
      4 of s" E-NELAB-LOCAL" endof
      5 of s" E-NELAB-LOCAL-CAP" endof
      6 of s" E-NELAB-BLOCK" endof
      7 of s" E-NELAB-BUNDLE" endof
      8 of s" E-NELAB-QUOT" endof
      9 of s" E-A64RA-SPILL" endof
      10 of s" E-A64RA-POOL" endof
      11 of s" E-A64RA-EDGE" endof
      12 of s" E-A64RA-PRESSURE" endof
      13 of s" E-NELAB-ARITY" endof
      14 of s" E-NMIGRATE-TEXT" endof
      15 of s" E-NMIGRATE-STATE" endof
      16 of s" E-NMIGRATE-NAME" endof
      17 of s" E-NMIGRATE-VERDICT" endof
      18 of s" E-NFEED-STATE" endof
      19 of s" E-NFEED-TEXT" endof
      20 of s" E-NCLOB-CAP" endof
      21 of s" E-NPUB-CAP" endof
      22 of s" engine refused the name" endof
      23 of s" engine refused a duplicate name" endof
      E-TBL-BOUNDS throw
   endcase ;

\ The first nine rows are the dialect's own refusals, the next four are register
\ pressure, then the instrument's, then the engine's - the last two being the
\ census reporting on itself.
\
\ THE TWO PUBLICATION CEILINGS ARE THE INSTRUMENT'S TOO, and they are the reason
\ this table gained a row rather than a comment. E-NCLOB-CAP is the clobber
\ record's table full and E-NPUB-CAP is the replacement log's; both are raised
\ AFTER selection, allocation, register-allocation validation and emission have
\ all accepted the definition, so a definition they refuse is one the chain
\ COMPILED and could not find a table row for. Unlisted, they printed as a raw
\ number in the dialect bucket - which is where a reader looks for work the chain
\ still needs - and the 2026-08-07 whole-tree run put 1669 of them there, three
\ times the biggest real dialect gap over the same tree. A capacity reached is
\ not a capability missing.
\
\ AND A CENSUS SHOULD NEVER SEE EITHER OF THEM AGAIN. Both records are written by
\ a publication, and a census does not publish: it measures through
\ NMIGRATE:MEASURE-HELD, which makes every refusal a publication can make and
\ then keeps none of the rows. A nonzero count on either line means something
\ started committing what it was only supposed to ask about.
\
\ E-NELAB-ARITY IS THE INSTRUMENT'S AND NOT THE DIALECT'S, and it is the one
\ classification here worth arguing. The elaborator throws it when the declared
\ input or output count does not match what the body does - and the census is what
\ declared those counts, from the checker's effect store, for a body it renamed. So
\ an arity refusal says the census mis-stated its own subject; the chain lacks no
\ capability. Left in the dialect bucket it would inflate exactly the number the
\ work order is read off. TOTALS. prints it on its own line for the same reason a
\ self-check count is printed: it is a number to watch, not a gap to close.
: ROW-CLASS ( n -- n ) {: i:n :}
   i 9 < if CL-DIALECT exit then
   i 13 < if CL-PRESSURE exit then
   i 22 < if CL-INSTRUMENT exit then
   CL-SELF-CHECK ;

\ Which named row a code is, or -1 for a code no row names.
: CODE-ROW ( n -- n ) {: code:n :}
   0 begin dup CODE# < while
      dup CODE-AT code = if exit then
      1+
   repeat drop -1 ;

\ A code nothing names is reported as a dialect refusal, because that is the
\ conservative reading: it is a refusal the chain made that this table did not
\ predict, and the report prints its raw number so nobody has to guess.
: CODE-CLASS ( n -- n ) {: code:n :}
   code CODE-ROW {: row:n :}
   row 0 < if CL-DIALECT exit then
   row ROW-CLASS ;

\ ---- the fresh name every subject is driven under ------------------------------
\ It has to be a name nothing in the tree carries and nothing a later run will
\ carry either, because the definition is CERTIFIED under it while it is being
\ measured, and a second certified definition of one name is refused before the
\ measurement can retract the first. The counter is monotonic for the life of the
\ process and is deliberately NOT cleared by RESET: two censuses in one process
\ must not offer the same name twice.
variable SUBJECT-N
create SUBJECT-BUF NAME-CAP allot
variable SUBJECT-U

: SUBJECT$ ( -- ptr u8 n )
   SUBJECT-BUF SUBJECT-U @ ;

: SUBJECT-MINT ( -- )
   SUBJECT-N @ 1+ SUBJECT-N !
   SB-RESET
   s" CHAIN-CENSUS-SUBJECT-" SB-APPEND
   SUBJECT-N @ FMT:SB-U
   SB$ {: a:ptr u:n :}
   u NAME-CAP > if s" NAME-CAP" CAP-FULL then
   a SUBJECT-BUF u BYTE-COPY
   u SUBJECT-U ! ;

\ ---- the staged source ---------------------------------------------------------
create SRC-BUF SRC-CAP allot
variable SRC-U
variable RUN-IN
variable RUN-OUT

: SRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SRC-U @ u + SRC-CAP > if s" SRC-CAP" CAP-FULL then
   a SRC-BUF SRC-U @ + u BYTE-COPY
   SRC-U @ u + SRC-U ! ;

\ Parked, because a quotation cannot read the enclosing word's locals and the run
\ has to happen inside one so its refusal can be caught.
: MIGRATE ( -- )
   SRC-BUF SRC-U @ RUN-IN @ RUN-OUT @ REGS NMIGRATE:MEASURE-HELD ;

\ ---- the package scope a section runs in ---------------------------------------
create PKG-BUF NAME-CAP allot
variable PKG-U
variable PKG-ON
variable SKIP-ON                      \ this section is one the census may not reopen

: PKG$ ( -- ptr u8 n )
   PKG-BUF PKG-U @ ;

\ `public` is what a reopen has to name, and it is enough: a package's private
\ words are visible from inside the package however the section was opened, which
\ is the whole reason the reopen exists. The subject words this publishes land in
\ the package's public wordlist under names nothing calls.
: SECTION-OPEN ( -- )
   SB-RESET
   s" package " SB-APPEND PKG$ SB-APPEND s"  public" SB-APPEND
   SB$ EV
   true PKG-ON ! ;

: SECTION-CLOSE ( -- )
   PKG-ON @ 0= if exit then
   false PKG-ON !
   s" ;package" EV ;

\ ---- the walk over one file's tokens -------------------------------------------
variable LEX-I
variable DEF-ON
variable DEF-KIND
variable DEF-DEFINER-I
variable DEF-NAME-I
variable BAD-STRUCTURE

: TOK$ ( n -- ptr u8 n )
   LINT-LEX:TOKEN ;

: TOK-LEN ( n -- n ) {: k:n :}
   k TOK$ {: a:ptr u:n :}
   u ;

: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: TOK=CI ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= if false exit then
   k TOK$ a u LINT-STR=CI ;

\ Everything of the definition after its name, as bytes of the loaded source: from
\ the byte just past the name token through the last byte of the closer. The
\ rename replaces exactly one token, so every other byte the checker will read is
\ the file's own text.
: TAIL$ ( n -- ptr u8 n ) {: close:n :}
   DEF-NAME-I @ {: ni:n :}
   ni LINT-LEX:BYTE@ ni TOK-LEN + {: from:n :}
   close LINT-LEX:BYTE@ close TOK-LEN + {: to:n :}
   LINT-SOURCE:TEXT {: sa:ptr su:n :}
   sa from +  to from - ;

: STAGE-SOURCE ( n -- ) {: close:n :}
   close TAIL$ {: ta:ptr tu:n :}
   0 SRC-U !
   DEF-DEFINER-I @ TOK$ SRC+
   s"  " SRC+
   SUBJECT$ SRC+
   ta tu SRC+ ;

\ ---- one definition ------------------------------------------------------------
\ Two spellings no Habu token can carry, because both hold a space and a token is
\ delimited by one. They stand for the two states in which the elaborator has a
\ refusal but no word to name it by, and they are entries of the sub-histogram
\ like any other, so its lines always add up to the buckets above them.

\ WHICH REFUSALS CARRY A SPELLING WORTH COUNTING. A refusal that stands on a body
\ TOKEN can name it, and the sub-histogram of those names is what says which
\ source shape blocks the most definitions. Two codes do: the word the dialect has
\ never heard of, and the quotation it HAS heard of and cannot shape yet. Keeping
\ the second out would have made this histogram lie the moment `[:` stopped being
\ unmodelled - the spelling would have left the table while every definition it
\ blocks went on being blocked, which is the one way a census can shrink without
\ anything improving.
: SPELLED-CODE? ( n -- bool )
   {: rc:n :}
   rc E-HIR-UNMODELED = if 0 0= exit then
   rc E-NELAB-QUOT = ;

: NO-TOKEN$ ( -- ptr u8 n )
   s" (refused no body token)" ;

: LONG-SPELL$ ( -- ptr u8 n )
   s" (spelling too long to record)" ;

: REFUSAL-SPELL$ ( -- ptr u8 n )
   REFUSED-AT 0 < if NO-TOKEN$ exit then
   REFUSED-SPELL$ {: sa:ptr su:n :}
   su 0= if LONG-SPELL$ exit then
   sa su ;

\ ---- the census checking the clear it depends on ---------------------------------
\ An outcome the elaborator cannot have recorded, arriving with a record anyway.
\ Two shapes qualify and both are definitional rather than argued: a definition
\ that COMPILED left no refusal behind, and a definition the ENGINE refused - an
\ unresolved name, a duplicate - never reached the elaborator at all. Either one
\ holding a record means REFUSED-CLEAR did not happen, and the damage would be a
\ spelling credited to a definition that never wrote it: a real-looking number in
\ the one histogram that decides what gets built next. So it is counted and
\ printed rather than trusted.
variable STALE-N

: STALE-CK ( n -- ) {: rc:n :}
   REFUSED-AT 0 < if exit then
   rc 0=  rc RC-UNDEFINED =  or  rc RC-DUPLICATE =  or 0= if exit then
   STALE-N @ 1+ STALE-N ! ;

: RECORD-REFUSAL ( n -- ) {: rc:n :}
   CUR-FILE F-REFUSED T+
   rc SPELLED-CODE? 0= if exit then
   REFUSAL-SPELL$ {: sa:ptr su:n :}
   sa su POOL+ su DEF-SPELL! ;

\ The order matters and each step earns its place. The arity is asked first,
\ because a definition the checker holds no effect for is one the census cannot
\ declare and therefore never offers - counting it as a refusal would blame the
\ chain for the seal. Then the rename is staged, the elaborator's refusal record
\ is cleared, and the chain is run; whatever it throws is recorded verbatim.
: RUN-ONE ( n -- ) {: close:n :}
   CUR-FILE F-COLON T+
   DEF-NAME-I @ TOK$ {: na:ptr nu:n :}
   na nu EFFECT {: in:n out:n :}
   in EFFECT-NONE = if CUR-FILE F-NOEFFECT T+ exit then
   na nu POOL+ {: noff:n :}
   SUBJECT-MINT
   close STAGE-SOURCE
   in RUN-IN !  out RUN-OUT !
   REFUSED-CLEAR
   [: MIGRATE ;] catch {: rc:n :}
   CUR-FILE noff nu rc in out SRC-U @ DEF+
   rc STALE-CK
   rc 0= if CUR-FILE F-COMPILED T+ exit then
   rc RECORD-REFUSAL ;

\ ---- which definer this is ------------------------------------------------------
\ ONLY THE PLAIN `:` IS CENSUSED, and the argument is short. A held migration
\ compiles the tape the CHECKER's reader filled, so it exists only for a
\ definition the checker certified. `TRUSTED:` is unchecked by construction and
\ fills no tape; `CHECKED:`, `KERNEL:`, `CAST:`, `MODEL:` and `+:` are the
\ engine's other colon-family definers and none is the form the production entry
\ compiles. Offering any of them would produce a refusal that says nothing about
\ the dialect. So they are COUNTED, under one line that names them as a population
\ rather than a verdict, and never dropped in silence. The test is on the definer
\ TOKEN, not on the text of the line, so a `TRUSTED:` written inside a comment or
\ a string is not a definer here and neither is anything else the lexer read as
\ inert.
: PLAIN-COLON? ( -- bool )
   DEF-DEFINER-I @ TOK$ s" :" LINT-STR= ;

: FINISH-DEF ( n -- ) {: close:n :}
   false DEF-ON !
   PLAIN-COLON? 0= if CUR-FILE F-NOTCOLON T+ exit then
   SKIP-ON @ if CUR-FILE F-CLOSED T+ exit then
   close RUN-ONE ;

: START-DEF ( n n -- ) {: k:n kind:n :}
   k LINT-DEF:NAME-I
   MATCH option
      none OF true BAD-STRUCTURE ! ENDOF
      some OF {: namei:n :}
         kind DEF-KIND !
         k DEF-DEFINER-I !
         namei DEF-NAME-I !
         kind LINT-DEF:DATA = if
            CUR-FILE F-NOTCOLON T+
         else
            true DEF-ON !
         then
         namei 1+ LEX-I !
      ENDOF
   ;MATCH ;

\ ---- the sections that may not be reopened at all ---------------------------------
\ A package whose wordlists were sealed with `prot-wid-add` - the idiom at the foot
\ of every substrate file - cannot be reopened. The engine does not throw there: it
\ ENDS THE PROCESS, status 84, with the package's name as its whole message. So a
\ census that tried one would take down the whole run, losing every file already
\ measured, and nothing catchable would say why.
\
\ IT IS ASKED STRUCTURALLY AND NOT GUESSED. tools/prot-wid-probe.f reads the
\ engine's own protected-wordlist bitmap, the same bits the engine's guard branches
\ on, so the answer is the engine's rather than a rule about file names. What it
\ needs is the section's wordlist id, and that is read off the dictionary: a
\ definition of package X publishes a record whose QUALIFIED spelling `X:NAME`
\ resolves globally, and the record carries the wordlist it landed in. So the
\ section is scanned for the first definition whose qualified name the dictionary
\ holds, and that record's wordlist is the section's.
\
\ A SECTION WHOSE WORDLIST CANNOT BE DETERMINED IS SKIPPED TOO. That happens when
\ no definition in the section is public - the qualified spelling of a private word
\ resolves nowhere - and the honest answer is that the census does not know whether
\ opening it is safe. Guessing costs the whole run; skipping costs one section and
\ is counted on its own line.
: NAME-WID ( n -- n ) {: namei:n :}
   SB-RESET
   PKG$ SB-APPEND
   58 SB-APPEND-C
   namei TOK$ SB-APPEND
   SB$ XREF-FIND-INDEX {: k:n :}
   k 0 < if -1 exit then
   k XREF-REC XREF-WORDLIST ;

: PROBE-AT ( n -- n ) {: k:n :}
   k WORD? 0= if -1 exit then
   k LINT-DEF:DIRECT-KIND LINT-DEF:NONE = if -1 exit then
   k LINT-DEF:NAME-I
   MATCH option
      none OF -1 ENDOF
      some OF NAME-WID ENDOF
   ;MATCH ;

: SECTION-WID ( n -- n ) {: from:n :}
   from
   begin dup LINT-LEX:COUNT < while
      dup s" ;package" TOK=CI if drop -1 exit then
      dup PROBE-AT dup 0 >= if nip exit then drop
      1+
   repeat drop -1 ;

: REOPENABLE? ( n -- bool ) {: wid:n :}
   wid 0 < if false exit then
   wid PROT-WID-PROBE:MEMBER? 0= ;

\ ---- and the engine's OTHER seal, which the bitmap knows nothing about ----------
\ src/habu/habu2.f C-PACKAGE-SEAL-GUARD runs TWO gates and the protected-wordlist
\ bitmap is only the second. The first is C-SEAL-MATCH: it folds the candidate
\ package name and compares it against RESTAB, a baked table of names no user
\ source may open once the friend latch is sealed. A package whose name is in that
\ table is refused the same uncatchable way - exit 84 with the name as the whole
\ message - and it need not be protected at all. `package LOWER-CERT-HOOK` (opened
\ by src/core/check-hook.f) is the case that found this: an unprotected package
\ that still cannot be reopened.
\
\ THIS IS A MIRROR OF AN ENGINE TABLE AND IT IS CHECKED AGAINST IT. RESTAB-BUF is
\ private to `package KWDATA` inside the image, so the census cannot read the
\ engine's own bytes at runtime and a second copy has to exist here - and a second
\ copy of a table is exactly the thing that drifts and then kills a run. So the
\ suite re-derives these seven names from src/habu/habu2.f's own `create
\ RESTAB-BUF` block, record by record, and fails if this list differs. A name
\ added to the engine's table turns the suite red instead of turning a census into
\ an exit 84.
\
\ The comparison is case-insensitive because the guard's is: the table is
\ lowercase in CHECKER-FOLD-C canonical form and C-SEAL-MATCH folds each candidate
\ byte A-Z to a-z, which is exactly what LINT-STR=CI does to both sides.
7 constant RESERVED#

: RESERVED-AT$ ( n -- ptr u8 n )
   case
      0 of s" tfam" endof
      1 of s" type" endof
      2 of s" match" endof
      3 of s" checker-cert" endof
      4 of s" lower-cert" endof
      5 of s" lower-cert-hook" endof
      6 of s" engine-error" endof
      E-TBL-BOUNDS throw
   endcase ;

: RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup RESERVED# < while
      dup RESERVED-AT$ a u LINT-STR=CI if drop true exit then
      1+
   repeat drop false ;

\ Both seals, asked as one question, because both have the same answer for this
\ tool: the section cannot be reopened and its definitions are counted rather than
\ offered. The name is asked FIRST - a reserved package need not be protected, so
\ the bitmap would let it through.
: SECTION-OPENABLE? ( n -- bool ) {: from:n :}
   PKG$ RESERVED? if false exit then
   from SECTION-WID REOPENABLE? ;

\ A `package` inside an open one is not source the engine accepts - it refuses the
\ nested opener outright - so no loadable file can hold one, and a census that met
\ one would be replaying a structure the engine cannot. It is recorded as a
\ file-level structure line rather than evaluated.
: OPEN-SECTION ( n -- ) {: k:n :}
   PKG-ON @ if true BAD-STRUCTURE ! exit then
   SKIP-ON @ if true BAD-STRUCTURE ! exit then
   k 1+ WORD? 0= if true BAD-STRUCTURE ! exit then
   k 1+ TOK$ {: a:ptr u:n :}
   u NAME-CAP > if true BAD-STRUCTURE ! exit then
   a PKG-BUF u BYTE-COPY
   u PKG-U !
   k 2 + SECTION-OPENABLE? if SECTION-OPEN else true SKIP-ON ! then
   k 2 + LEX-I ! ;

: CLOSE-SECTION ( n -- ) {: k:n :}
   PKG-ON @ 0= SKIP-ON @ 0= and if true BAD-STRUCTURE ! exit then
   SKIP-ON @ if false SKIP-ON ! else SECTION-CLOSE then
   k 1+ LEX-I ! ;

: PACKAGE-TOKEN ( n -- bool ) {: k:n :}
   k s" package" TOK=CI if k OPEN-SECTION true exit then
   k s" ;package" TOK=CI if k CLOSE-SECTION true exit then
   false ;

: STEP ( n -- ) {: k:n :}
   DEF-ON @ if
      k DEF-KIND @ LINT-DEF:CLOSE? if k FINISH-DEF then
      k 1+ LEX-I ! exit
   then
   k WORD? 0= if k 1+ LEX-I ! exit then
   k PACKAGE-TOKEN if exit then
   k LINT-DEF:DIRECT-KIND dup LINT-DEF:NONE = if drop k 1+ LEX-I ! exit then
   k swap START-DEF ;

: WALK ( -- )
   0 LEX-I !
   false DEF-ON !
   false SKIP-ON !
   false BAD-STRUCTURE !
   begin LEX-I @ LINT-LEX:COUNT < BAD-STRUCTURE @ 0= and while
      LEX-I @ STEP
   repeat
   DEF-ON @ if true BAD-STRUCTURE ! then
   PKG-ON @ if true BAD-STRUCTURE ! then
   SKIP-ON @ if true BAD-STRUCTURE ! then ;

\ ---- one file -------------------------------------------------------------------
\ The file is LOADED before it is read. The checker's effect store only answers for
\ words compiled in this process, so a file nobody required would present every one
\ of its definitions as having no certified effect and the census would measure
\ nothing at all. `required` on a file already in the image is a no-op, so this
\ costs nothing for the engine's own sources and is what makes lib/ and src/
\ measurable. A file that will not load is a counted line and the run goes on to
\ the next one.
create REQ-BUF FS-PATH-CAP allot
variable REQ-U

: REQUIRE-IT ( -- )
   REQ-BUF REQ-U @ required ;

\ `.f` and nothing else. `.fs` files are the gforth recovery host's sources; the
\ checker never certified them, so there is no tape to compile and no effect to
\ ask about.
: HABU-SOURCE? ( ptr u8 n -- bool )
   s" .f" LINT-ENDS-WITH? ;

\ ---- the files this tool must not load ---------------------------------------------
\ LOADING IS HOW THE CENSUS MEASURES, and a suite RUNS when it is loaded. Its
\ assertions execute, its fixtures are written and its failure ends the process -
\ so a census pointed at a directory holding suites would be reporting on whether
\ the tree's tests pass, which is not its question, and one failing suite would
\ take every measurement already made down with it.
\
\ THE SUFFIX IS THE TREE'S OWN CLASSIFIER and not a guess about content:
\ test/lint-cli-standalone-load.f uses exactly these tails to decide, by structure
\ alone, which files in tools/ are entry points. A suite's definitions are fixtures
\ rather than the code the chain has to compile, so nothing the census exists to
\ measure is lost by leaving them out - and they are counted rather than dropped.
\
\ IT IS NOT A GENERAL DEFENCE, and the limit is worth stating: any Habu file may
\ execute at load, and a CLI entry runs its own main. Point this tool at sources.
: SUITE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -test.f" LINT-ENDS-WITH? if true exit then
   a u s" -test-lib.f" LINT-ENDS-WITH? ;

\ ---- putting back what a refused load did not ------------------------------------
\ src/core/include.f INCLUDE-LOAD is push / read / evaluate / pop, and the pop is
\ NOT on the failure path: a file that throws while compiling unwinds past it and
\ leaves the include depth one level higher for good. Sixteen distinct files that
\ will not compile therefore exhaust INCLUDE-MAX-DEPTH, and the seventeenth load -
\ a perfectly good file - ENDS THE PROCESS with "include: nested too deeply". A
\ census over src/ meets sixteen such files easily and used to die at exactly that
\ point, losing everything measured before it.
\
\ THE DEPTH IS A STACK POINTER AND THE LEVEL IS DEAD. Each level owns one read
\ buffer, indexed by the depth, and it is live only while that level is
\ evaluating. Once the throw has unwound past INCLUDE-LOAD nothing reads that
\ buffer again - nor any level a nested require inside the failed file pushed - so
\ restoring the depth to the value it had before the call is exactly the pop that
\ did not happen, not a new rule. It is done here, immediately, in the one word
\ that made the call, so no other include can be in flight across it.
\
\ IT BELONGS IN THE ENGINE, NOT HERE. The repair is one catch inside INCLUDE-LOAD,
\ and until it lands every long-running loader in the tree has this hole; this
\ file only stops its own sweep from dying of it.
variable LOAD-DEPTH

: LOAD-FILE ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u FS-PATH-CAP > if false exit then
   a REQ-BUF u BYTE-COPY
   u REQ-U !
   INCLUDE-DEPTH @ LOAD-DEPTH !
   [: REQUIRE-IT ;] catch {: rc:n :}
   LOAD-DEPTH @ INCLUDE-DEPTH !
   rc 0= ;

\ The scope is closed on every path out of the walk. A section left open would put
\ the NEXT file's definitions - and everything the process compiles afterwards -
\ into a package nobody asked for, so the close is paired with the walk rather
\ than left to whatever the walk did.
: SCAN-FILE ( -- )
   [: WALK ;] catch {: rc:n :}
   SECTION-CLOSE
   false SKIP-ON !
   rc 0 <> if rc throw then
   BAD-STRUCTURE @ if ST-STRUCTURE STATUS! then ;

\ THE EXISTENCE CHECK IS NOT BELT AND BRACES. `required` on a path it cannot open
\ does not throw - it ends the process (measured: exit 74, "include: open
\ failed"), so a single bad path on the command line would take the whole census
\ down with it and every file already measured would be lost. A path that exists
\ and will not COMPILE is catchable, and that is the other line below.
: CENSUS-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u POOL+ u FILE+
   a u HABU-SOURCE? 0= if ST-NOT-SOURCE STATUS! exit then
   a u SUITE? if ST-SUITE STATUS! exit then
   a u EXISTS? 0= if ST-MISSING STATUS! exit then
   a u LOAD-FILE 0= if ST-LOAD-REFUSED STATUS! exit then
   a u LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   LINT-LEX:ERROR? if ST-LEX-ERROR STATUS! exit then
   SCAN-FILE ;

\ ---- the histograms --------------------------------------------------------------
: BUCKET+ ( n -- ) {: code:n :}
   0 begin dup BK-N @ < while
      dup BK-CODE swap T@ code = if BK-COUNT T+ exit then
      1+
   repeat drop
   BK-N @ BUCKETS-MAX >= if s" BUCKETS-MAX" CAP-FULL then
   code BK-CODE BK-N @ T!
   1 BK-COUNT BK-N @ T!
   BK-N @ 1+ BK-N ! ;

: SP$ ( n -- ptr u8 n ) {: k:n :}
   SP-OFF k T@  SP-U k T@ POOL$ ;

: SPELL+ ( n n -- ) {: off:n u:n :}
   u 0= if exit then
   off u POOL$ {: a:ptr au:n :}
   0 begin dup SP-N @ < while
      dup SP$ a au LINT-STR= if SP-COUNT T+ exit then
      1+
   repeat drop
   SP-N @ SPELLS-MAX >= if s" SPELLS-MAX" CAP-FULL then
   off SP-OFF SP-N @ T!
   u SP-U SP-N @ T!
   1 SP-COUNT SP-N @ T!
   SP-N @ 1+ SP-N ! ;

: TALLY-ONE ( n -- ) {: k:n :}
   D-CODE k T@ {: rc:n :}
   rc 0= if exit then
   rc BUCKET+
   rc SPELLED-CODE? 0= if exit then
   D-SPELL-OFF k T@  D-SPELL-U k T@ SPELL+ ;

: TALLY ( -- )
   0 BK-N !  0 SP-N !
   0 begin dup D-N @ < while
      dup TALLY-ONE
      1+
   repeat drop ;

: BK-BEFORE? ( n n -- bool ) {: l:n r:n :}
   BK-COUNT l T@ {: ln:n :}
   BK-COUNT r T@ {: rn:n :}
   ln rn <> if ln rn > exit then
   BK-CODE l T@  BK-CODE r T@ < ;

: SP-BEFORE? ( n n -- bool ) {: l:n r:n :}
   SP-COUNT l T@ {: ln:n :}
   SP-COUNT r T@ {: rn:n :}
   ln rn <> if ln rn > exit then
   l SP$ r SP$ LINT-ORDER:CMP-CI {: c:n :}
   c 0 <> if c 0 < exit then
   l r < ;

\ Density is a ratio and is compared as one, by cross-multiplication: two refusals
\ out of three is denser than ninety out of two hundred, and no division and no
\ float is needed to say so.
\
\ A FILE WITH NOTHING EXAMINED HAS NO DENSITY, and it has to be said separately
\ rather than left to the arithmetic. Cross-multiplying against it makes both
\ products zero, so it compares EQUAL to every file at once - and a comparator
\ that calls a equal to b and b equal to c while a and b differ is not an ordering
\ at all; the sort built on it returns whatever the heap happened to do. That is
\ how a file of density one came to be printed below a file of density 0.94. Such
\ a file sorts after every file that has one, and ties with the others by path.
: DN-BEFORE? ( n n -- bool ) {: l:n r:n :}
   l F-EXAMINED@ {: le:n :}
   r F-EXAMINED@ {: re:n :}
   le 0= re 0= or if
      le 0 > re 0 > or if le 0 > exit then
   else
      F-REFUSED l T@ re * {: a:n :}
      F-REFUSED r T@ le * {: b:n :}
      a b <> if a b > exit then
   then
   l F$ r F$ LINT-ORDER:CMP-CI {: c:n :}
   c 0 <> if c 0 < exit then
   l r < ;

: ORDER-FILL ( ptr a n -- ) {: v:ptr n:n :}
   v VEC-CLEAR
   n 0 ?do i v VEC-PUSH-N drop loop ;

: ORDER ( -- )
   BK-ORD BK-N @ ORDER-FILL
   BK-ORD VEC-DATA@ BK-N @ [: BK-BEFORE? ;] SORT:SORT!
   SP-ORD SP-N @ ORDER-FILL
   SP-ORD VEC-DATA@ SP-N @ [: SP-BEFORE? ;] SORT:SORT!
   DN-ORD F-N @ ORDER-FILL
   DN-ORD VEC-DATA@ F-N @ [: DN-BEFORE? ;] SORT:SORT! ;

\ ---- printing --------------------------------------------------------------------
\ The report goes through the shared lint writer rather than straight to the
\ terminal, for one reason: a report a test cannot read is a report nothing holds
\ to its own shape. LINT-OUT-BUFFER! redirects it into a caller's buffer, which is
\ how the suite checks that an error code no row names really does reach the page
\ with its raw number instead of being quietly dropped. With no buffer installed it
\ writes to standard output exactly as `type` did.
: OUT ( ptr u8 n -- )
   1 -rot LINT-OUT-WRITE ;

create NL-BUF 1 allot

: NL ( -- )
   10 NL-BUF c!
   NL-BUF 1 OUT ;

: U. ( n -- )
   SB-RESET FMT:SB-U SB$ OUT ;

: I. ( n -- )
   SB-RESET FMT:SB-INT SB$ OUT ;

: STATUS$ ( n -- ptr u8 n )
   case
      ST-OK of s" ok" endof
      ST-NOT-SOURCE of s" not a Habu source path" endof
      ST-LOAD-REFUSED of s" would not load" endof
      ST-LEX-ERROR of s" the lexer refused the text" endof
      ST-STRUCTURE of s" package or definition structure the census cannot replay" endof
      ST-MISSING of s" no file at that path" endof
      ST-SUITE of s" a suite rather than a source" endof
      E-TBL-BOUNDS throw
   endcase ;

: CLASS$ ( n -- ptr u8 n )
   case
      CL-DIALECT of s" dialect" endof
      CL-PRESSURE of s" pressure" endof
      CL-INSTRUMENT of s" instrument" endof
      CL-SELF-CHECK of s" self-check" endof
      E-TBL-BOUNDS throw
   endcase ;

: FILE-LINE ( n -- ) {: k:n :}
   k F$ OUT
   F-STATUS k T@ {: st:n :}
   st ST-OK <> if
      s"   [" OUT st STATUS$ OUT s" ]" OUT NL exit
   then
   s"   examined " OUT k F-EXAMINED@ U.
   s"  compiled " OUT F-COMPILED k T@ U.
   s"  refused " OUT F-REFUSED k T@ U.
   s"  not-a-colon-definition " OUT F-NOTCOLON k T@ U.
   s"  no-certified-effect " OUT F-NOEFFECT k T@ U.
   s"  in-a-package-the-census-cannot-reopen " OUT F-CLOSED k T@ U.
   NL ;

: FILES. ( -- )
   s" -- per file --------------------------------------------------" OUT NL
   0 begin dup F-N @ < while
      dup FILE-LINE
      1+
   repeat drop ;

: BUCKET-LINE ( n -- ) {: k:n :}
   BK-CODE k T@ {: code:n :}
   s"   " OUT BK-COUNT k T@ U.
   s"  " OUT
   code CODE-ROW {: row:n :}
   row 0 < if
      s" unlisted code " OUT code I.
   else
      row CODE-NAME$ OUT
      s"  (" OUT code I. s" )" OUT
   then
   s"  [" OUT code CODE-CLASS CLASS$ OUT s" ]" OUT NL ;

variable ACC

: CLASS-SUM ( n -- n ) {: cl:n :}
   0 ACC !
   0 begin dup BK-N @ < while
      dup BK-CODE swap T@ CODE-CLASS cl = if
         dup BK-COUNT swap T@ ACC @ + ACC !
      then
      1+
   repeat drop
   ACC @ ;

: CLASS-LINE ( n -- ) {: cl:n :}
   s"   " OUT cl CLASS-SUM U.
   s"  " OUT cl CLASS$ OUT NL ;

: BUCKETS. ( -- )
   s" -- refusals by reason, biggest first -------------------------" OUT NL
   0 begin dup BK-N @ < while
      dup BK-ORD swap VEC-IDX VEC-N@ BUCKET-LINE
      1+
   repeat drop
   s" -- refusals by class ----------------------------------------" OUT NL
   CL-DIALECT CLASS-LINE
   CL-PRESSURE CLASS-LINE
   CL-INSTRUMENT CLASS-LINE
   CL-SELF-CHECK CLASS-LINE ;

: SPELL-LINE ( n -- ) {: k:n :}
   s"   " OUT SP-COUNT k T@ U.
   s"  " OUT k SP$ OUT NL ;

: SPELLS. ( -- )
   s" -- refused spellings, biggest first -------------------------" OUT NL
   0 begin dup SP-N @ < while
      dup SP-ORD swap VEC-IDX VEC-N@ SPELL-LINE
      1+
   repeat drop ;

: DENSE-LIMIT ( -- n )
   F-N @ DENSE-SHOWN > if DENSE-SHOWN exit then
   F-N @ ;

: DENSE-LINE ( n -- ) {: k:n :}
   k F-EXAMINED@ 0= if exit then
   s"   " OUT F-REFUSED k T@ U.
   s" /" OUT k F-EXAMINED@ U.
   s"  " OUT k F$ OUT NL ;

: DENSE. ( -- )
   s" -- files by refusal density ---------------------------------" OUT NL
   DENSE-LIMIT {: n:n :}
   0 begin dup n < while
      dup DN-ORD swap VEC-IDX VEC-N@ DENSE-LINE
      1+
   repeat drop ;

: TOTAL ( ptr a -- n ) {: t:ptr :}
   0 ACC !
   0 begin dup F-N @ < while
      dup t swap T@ ACC @ + ACC !
      1+
   repeat drop
   ACC @ ;

: CODE-COUNT ( n -- n ) {: code:n :}
   0 begin dup BK-N @ < while
      dup BK-CODE swap T@ code = if BK-COUNT swap T@ exit then
      1+
   repeat drop 0 ;

: TOTALS. ( -- )
   s" -- totals ---------------------------------------------------" OUT NL
   s"   files " OUT F-N @ U. NL
   s"   examined " OUT F-COMPILED TOTAL F-REFUSED TOTAL + U. NL
   s"   compiled " OUT F-COMPILED TOTAL U. NL
   s"   refused " OUT F-REFUSED TOTAL U. NL
   s"   not-a-colon-definition " OUT F-NOTCOLON TOTAL U. NL
   s"   no-certified-effect " OUT F-NOEFFECT TOTAL U. NL
   s"   in-a-package-the-census-cannot-reopen " OUT F-CLOSED TOTAL U. NL
   s" -- the census reporting on itself ---------------------------" OUT NL
   s"   declared-arity disagreements " OUT E-NELAB-ARITY CODE-COUNT U. NL
   s"   names the reopened package did not resolve " OUT
      RC-UNDEFINED CODE-COUNT U. NL
   s"   renames the dictionary refused " OUT RC-DUPLICATE CODE-COUNT U. NL
   s"   outcomes carrying a stale elaborator record " OUT STALE-N @ U. NL ;

\ The report position a caller asks by, turned into the table row it names.
: BK-CK ( n -- n ) {: k:n :}
   k 0 < k BK-N @ >= or if E-TBL-BOUNDS throw then
   BK-ORD k VEC-IDX VEC-N@ ;

: SP-CK ( n -- n ) {: k:n :}
   k 0 < k SP-N @ >= or if E-TBL-BOUNDS throw then
   SP-ORD k VEC-IDX VEC-N@ ;

public

\ ---- the surface ------------------------------------------------------------------
: RESET ( -- )
   TABLES-INIT
   POOL-RESET
   0 D-N !  0 F-N !  0 P-N !
   0 BK-N !  0 SP-N !  0 STALE-N !
   false PKG-ON !  false SKIP-ON ! ;

\ Point the census at one path. A directory is walked for its Habu sources; a file
\ is taken as it stands. Nothing is censused yet - the whole list is sorted first.
: PATH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u EXISTS? 0= if a u PATH-KEEP exit then
   a u DIR? 0= if a u PATH-KEEP exit then
   a u [: PATH-KEEP ;] WALK-FILES ;

\ Census every collected path, in sorted order.
: RUN ( -- )
   P-ORD P-N @ ORDER-FILL
   P-ORD VEC-DATA@ P-N @ [: P-BEFORE? ;] SORT:SORT!
   0 begin dup P-N @ < while
      dup P-ORD swap VEC-IDX VEC-N@ P$ CENSUS-FILE
      1+
   repeat drop ;

\ Build the histograms. Every query below reads what this wrote.
: FINISH ( -- )
   TALLY
   ORDER ;

: REPORT ( -- )
   FINISH
   FILES. BUCKETS. SPELLS. DENSE. TOTALS. ;

\ ---- what a caller, and a test, can ask ---------------------------------------------
: FILES ( -- n )
   F-N @ ;

: FILE-PATH$ ( n -- ptr u8 n )
   F-CK F$ ;

: FILE-STATUS ( n -- n )
   F-CK F-STATUS swap T@ ;

: FILE-EXAMINED ( n -- n )
   F-CK F-EXAMINED@ ;

: FILE-COMPILED ( n -- n )
   F-CK F-COMPILED swap T@ ;

: FILE-REFUSED ( n -- n )
   F-CK F-REFUSED swap T@ ;

: FILE-NOTCOLON ( n -- n )
   F-CK F-NOTCOLON swap T@ ;

: FILE-NOEFFECT ( n -- n )
   F-CK F-NOEFFECT swap T@ ;

: FILE-CLOSED ( n -- n )
   F-CK F-CLOSED swap T@ ;

: DEFS ( -- n )
   D-N @ ;

: DEF-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k D-CK {: ck:n :}
   D-NAME-OFF ck T@  D-NAME-U ck T@ POOL$ ;

: DEF-CODE ( n -- n )
   D-CK D-CODE swap T@ ;

: DEF-SPELL$ ( n -- ptr u8 n ) {: k:n :}
   k D-CK {: ck:n :}
   D-SPELL-OFF ck T@  D-SPELL-U ck T@ POOL$ ;

: DEF-IN ( n -- n )
   D-CK D-IN swap T@ ;

: DEF-OUT ( n -- n )
   D-CK D-OUT swap T@ ;

\ How many source bytes this definition was offered to the chain as. It is the
\ recorder's own subject: the ceiling a length refusal names is a ceiling on
\ exactly this number.
: DEF-SRC-U ( n -- n )
   D-CK D-SRC-U swap T@ ;

\ Which file the definition came from, as an index into the file table. A run over
\ many files answers many rows and the only thing that ties one back to its source
\ is this.
: DEF-FILE ( n -- n )
   D-CK D-FILE swap T@ ;

: BUCKETS ( -- n )
   BK-N @ ;

\ In report order: biggest first, ties by code.
: BUCKET-CODE ( n -- n )
   BK-CK BK-CODE swap T@ ;

: BUCKET-N ( n -- n )
   BK-CK BK-COUNT swap T@ ;

: SPELLS ( -- n )
   SP-N @ ;

: SPELL$ ( n -- ptr u8 n )
   SP-CK SP$ ;

: SPELL-N ( n -- n )
   SP-CK SP-COUNT swap T@ ;

\ How many definitions one error code refused, asked by code rather than by
\ report position.
: COUNT-OF ( n -- n )
   CODE-COUNT ;

\ Which class a code falls into. Answers for a code no row names too, which is
\ what makes the taxonomy a checkable claim rather than a table.
: CLASS-OF ( n -- n )
   CODE-CLASS ;

\ Files in refusal-density order, densest first; files with nothing examined last.
: DENSE-FILE ( n -- n ) {: k:n :}
   k F-CK drop
   DN-ORD k VEC-IDX VEC-N@ ;

\ The mirror of the engine's sealed reserved-package table, so a suite can hold it
\ against src/habu/habu2.f's own bytes.
: RESERVED ( -- n )
   RESERVED# ;

: RESERVED$ ( n -- ptr u8 n ) {: k:n :}
   k 0 < k RESERVED# >= or if E-TBL-BOUNDS throw then
   k RESERVED-AT$ ;

\ How many outcomes arrived carrying an elaborator record they cannot have made.
\ Zero is the only acceptable answer; anything else means the per-attempt clear is
\ not happening and no spelling in this run can be trusted.
: STALE-RECORDS ( -- n )
   STALE-N @ ;

;package
