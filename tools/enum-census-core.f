\ enum-census-core.f - every plain ENUM declaration in the tree, re-declared
\ through the production ENUM keyword, reported as a comparable record.
\
\ Why this exists. The global `ENUM` keyword used to be a compact parser and
\ generator of its own in src/core/sumtype.f; it is now the unified front end
\ (src/core/enum-decl.f, ENUM-DECL:ED-RUN). Suites prove the two agree on the
\ cases somebody thought to write down. This walks the whole repository instead:
\ it finds every plain `ENUM name variant... ;ENUM` declaration that actually
\ exists, re-declares each one through the same global keyword the source uses,
\ and writes down what the type registry then holds for it - kind, visibility,
\ variant names and tags, payload width, layout policy, derive flags, and the
\ constructor package and symbol each variant carries. The report is compared
\ against a recorded baseline, so a divergence names the site and the field.
\
\ The baseline that ships with this tool was recorded on the commit BEFORE the
\ keyword moved, where `ENUM` was still sumtype.f's compact definer. Comparing
\ against it is therefore a real before/after parity check, not a snapshot of
\ the new behaviour agreeing with itself.
\
\ How a site is re-declared. Each site is replayed into its own scratch package
\ (`package ecN`) with the visibility it has at its own site, so two files may
\ declare the same family tail without shadowing each other, and a private
\ declaration stays private - which is what decides whether constructors are
\ generated at all. The declaration text is handed to `evaluate`, so the tokens
\ go through the global keyword exactly as they do when the owning file loads:
\ this drives the production entry point, not a registration side door.
\
\ Scanning is structural. The source lexer (tools/lint/source-lex.f) classifies
\ comments, strings and primitive-axiom rows as their own token kinds, so an
\ `ENUM` inside a comment or a string is not a site; a definition NAME (`: ENUM`)
\ and an escaped reference (`' ENUM`, `postpone ENUM`, ...) are not sites either.
\ A declaration whose first body token is an arity is the FULL form, which is
\ counted separately and not replayed: this census is about the plain form the
\ legacy definer could express.
\
\ This core loads its own dependencies, so `bin/hb --load tools/enum-census-core.f`
\ works on its own. It used to only list them in a comment and rely on the caller
\ having loaded them first; that held for tools/enum-census.f and inside the resident
\ gate, but left the core dead on its own and took the standalone
\ test/gate-stdlib-lint-tools.f down with it (the census core is the first thing that
\ file requires, and it died on LINT-LEX:COUNT before the file's own body was reached).

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package ENUM-CENSUS

$200000 constant OUT-CAP       \ report bytes
$8000 constant BUILD-CAP       \ one reconstructed declaration
$40 constant PKG-CAP
$20 constant NUM-CAP

create OUT-BUF OUT-CAP allot
create BUILD-BUF BUILD-CAP allot
create PKG-BUF PKG-CAP allot
create NUM NUM-CAP allot

variable OUT-U     variable BUILD-U   variable PKG-U     variable NUM-I
variable SRC-A     variable SRC-U     variable SRC-CAP
variable BASE-A    variable BASE-U    variable BASE-CAP
variable FILE-A    variable FILE-U
variable SITE-N    variable FULL-N    variable FILE-N    variable BAD-N
variable IN-FILE-N \ site ordinal within the current file: the stable locator
\ One index per nesting level. COPY-I belongs to the byte-copy helpers, which the
\ record writers call from inside their own loops; sharing one index here cost an
\ afternoon (the variant loop's counter was reset by the append it called, so the
\ report grew until the buffer raised).
variable SCAN-I    variable COPY-I    variable BODY-I
variable VAR-I     variable DIG-I
variable PKG-OPEN  variable PKG-VIS
variable REPLAY?   variable TRIPWIRE?

1 constant VIS-PRIVATE
2 constant VIS-PUBLIC
0 constant VIS-GLOBAL

\ ---------------------------------------------------------------------------
\ Raw-memory boundaries. The report, the reconstructed declaration, the scratch
\ package name and the baseline all live in `create` regions, which a checked
\ body cannot type as `ptr u8` spans - the boundary src/core/structure-decl.f's
\ PEND! documents. `evaluate` and the sealed type-registry readers are pre-hook
\ words a post-hook checked body cannot call directly either.
\ ---------------------------------------------------------------------------
TRUSTED: SRC-A-FIELD ( -- ptr ptr u8 ) SRC-A 0 ptr-field ;
TRUSTED: BASE-A-FIELD ( -- ptr ptr u8 ) BASE-A 0 ptr-field ;
TRUSTED: FILE-A-FIELD ( -- ptr ptr u8 ) FILE-A 0 ptr-field ;

: SRC-A@ ( -- ptr u8 ) SRC-A-FIELD @ ;
: SRC-A! ( ptr u8 -- ) SRC-A-FIELD ! ;
: BASE-A@ ( -- ptr u8 ) BASE-A-FIELD @ ;
: BASE-A! ( ptr u8 -- ) BASE-A-FIELD ! ;
: FILE-A@ ( -- ptr u8 ) FILE-A-FIELD @ ;
: FILE-A! ( ptr u8 -- ) FILE-A-FIELD ! ;
: FILE$ ( -- ptr u8 n ) FILE-A@ FILE-U @ ;

TRUSTED: OUT$ ( -- ptr u8 n ) OUT-BUF OUT-U @ ;
TRUSTED: OUT-C ( n -- ) {: c:n :}
   OUT-U @ OUT-CAP >= IF s" enum-census: report buffer overflow" 74 die THEN
   c OUT-BUF OUT-U @ + c!  OUT-U @ 1 + OUT-U ! ;
TRUSTED: BUILD$ ( -- ptr u8 n ) BUILD-BUF BUILD-U @ ;
TRUSTED: BUILD-C ( n -- ) {: c:n :}
   BUILD-U @ BUILD-CAP >= IF s" enum-census: declaration buffer overflow" 74 die THEN
   c BUILD-BUF BUILD-U @ + c!  BUILD-U @ 1 + BUILD-U ! ;
TRUSTED: PKG$ ( -- ptr u8 n ) PKG-BUF PKG-U @ ;
TRUSTED: PKG-C ( n -- ) {: c:n :}
   PKG-U @ PKG-CAP >= IF s" enum-census: package-name buffer overflow" 74 die THEN
   c PKG-BUF PKG-U @ + c!  PKG-U @ 1 + PKG-U ! ;
TRUSTED: BASE$ ( -- ptr u8 n ) BASE-A@ BASE-U @ ;
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: TRY ( ptr u8 n -- n ) ['] EV catch ;

\ Expected declaration rejects are provoked on purpose below; route their
\ rendered packet into a buffer so a clean run says nothing on stderr. Same
\ capture channel tools/check-core.f uses.
$1000 constant DIAG-CAP
create DIAG-BUF DIAG-CAP allot
TRUSTED: DIAG-CAPTURE ( -- ) DIAG-BUF DIAG-CAP DIAG-BUFFER! ;
TRUSTED: DIAG-RELEASE ( -- ) DIAG-BUFFER-OFF ;

TRUSTED: FAM-RESOLVE ( ptr u8 n ptr u8 n -- n bool ) TFAM-SIG-RESOLVE ;
TRUSTED: FAM-KIND ( n -- n ) TFAM-KIND@ ;
TRUSTED: FAM-VIS ( n -- n ) TFAM-VIS@ ;
TRUSTED: FAM-WIDTH ( n -- n ) TFAM-WIDTH@ ;
TRUSTED: FAM-POLICY ( n -- n ) TFAM-LAYOUT-POLICY@ ;
TRUSTED: FAM-EQ? ( n -- bool ) TFAM-DERIVE-EQ? ;
TRUSTED: FAM-HASH? ( n -- bool ) TFAM-DERIVE-HASH? ;
TRUSTED: FAM-VAR-START ( n -- n ) TFAM-VAR-START@ ;
TRUSTED: FAM-VAR-COUNT ( n -- n ) TFAM-VAR-COUNT@ ;
TRUSTED: VAR-NAME$ ( n -- ptr u8 n ) SUMV-NAME$ ;
TRUSTED: VAR-TAG ( n -- n ) SUMV-TAG@ ;
TRUSTED: VAR-CTOR-PKG$ ( n -- ptr u8 n ) SUMV-CTOR-PKG$ ;
TRUSTED: VAR-CTOR-SYM ( n -- n ) SUMV-CTOR-SYM@ ;

\ ---------------------------------------------------------------------------
\ small formatting helpers
\ ---------------------------------------------------------------------------
TRUSTED: NUM$ ( -- ptr u8 n ) NUM NUM-I @ + NUM-CAP NUM-I @ - ;
TRUSTED: NUM-PUT ( n -- ) {: d:n :}
   NUM-I @ 1 - NUM-I !
   d 48 + NUM NUM-I @ + c! ;

: DEC$ ( n -- ptr u8 n ) {: v:n :}     \ non-negative decimal text
   NUM-CAP NUM-I !
   v 0= IF 0 NUM-PUT NUM$ EXIT THEN
   v BEGIN dup 0 > WHILE  dup 10 mod NUM-PUT  10 /  REPEAT drop
   NUM$ ;

: OUT$+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 COPY-I !
   BEGIN COPY-I @ u < WHILE  a COPY-I @ + c@ OUT-C  COPY-I @ 1 + COPY-I !  REPEAT ;
: OUT-N ( n -- ) DEC$ OUT$+ ;
: OUT-NL ( -- ) 10 OUT-C ;

: BUILD$+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 COPY-I !
   BEGIN COPY-I @ u < WHILE  a COPY-I @ + c@ BUILD-C  COPY-I @ 1 + COPY-I !  REPEAT ;
: PKG$+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 COPY-I !
   BEGIN COPY-I @ u < WHILE  a COPY-I @ + c@ PKG-C  COPY-I @ 1 + COPY-I !  REPEAT ;

\ ---------------------------------------------------------------------------
\ token classification over the lexed file
\ ---------------------------------------------------------------------------
: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:COUNT >= IF LINT-FALSE EXIT THEN
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: TOK= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= IF LINT-FALSE EXIT THEN
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

\ A definition NAME rather than a declaration: `: ENUM`, `TRUSTED: ENUM`, ...
: NAME-POSITION? ( n -- bool ) {: k:n :}
   k 0 <= IF LINT-FALSE EXIT THEN
   k 1 - WORD? 0= IF LINT-FALSE EXIT THEN
   k 1 - LINT-LEX:TOKEN s" :" LINT-STR= IF LINT-TRUE EXIT THEN
   k 1 - LINT-LEX:TOKEN s" +:" LINT-STR= IF LINT-TRUE EXIT THEN
   k 1 - s" TRUSTED:" TOK= IF LINT-TRUE EXIT THEN
   k 1 - s" KERNEL:" TOK= IF LINT-TRUE EXIT THEN
   k 1 - s" CHECKED:" TOK= IF LINT-TRUE EXIT THEN
   k 1 - s" PRIM:" TOK= ;

\ An escaped reference rather than a declaration: `' ENUM`, `postpone ENUM`, ...
: ESCAPED? ( n -- bool ) {: k:n :}
   k 0 <= IF LINT-FALSE EXIT THEN
   k 1 - WORD? 0= IF LINT-FALSE EXIT THEN
   k 1 - LINT-LEX:TOKEN s" '" LINT-STR= IF LINT-TRUE EXIT THEN
   k 1 - LINT-LEX:TOKEN s" [']" LINT-STR= IF LINT-TRUE EXIT THEN
   k 1 - s" postpone" TOK= IF LINT-TRUE EXIT THEN
   k 1 - s" char" TOK= IF LINT-TRUE EXIT THEN
   k 1 - s" [char]" TOK= ;

: OPENER? ( n -- bool ) {: k:n :}
   k s" enum" TOK= 0= IF LINT-FALSE EXIT THEN
   k NAME-POSITION? IF LINT-FALSE EXIT THEN
   k ESCAPED? IF LINT-FALSE EXIT THEN
   LINT-TRUE ;

: DIGIT? ( n -- bool ) {: c:n :} c 47 > c 58 < and ;
: ALL-DIGITS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF LINT-FALSE EXIT THEN
   0 DIG-I !
   BEGIN DIG-I @ u < WHILE
      a DIG-I @ + c@ DIGIT? 0= IF LINT-FALSE EXIT THEN
      DIG-I @ 1 + DIG-I !
   REPEAT LINT-TRUE ;

\ ---------------------------------------------------------------------------
\ package context, tracked while scanning so each site is replayed with the
\ visibility it has at its own site
\ ---------------------------------------------------------------------------
: CONTEXT-RESET ( -- )
   0 PKG-OPEN !   VIS-GLOBAL PKG-VIS ! ;

: CONTEXT-STEP ( n -- ) {: k:n :}
   k s" package" TOK= IF -1 PKG-OPEN !  VIS-PRIVATE PKG-VIS ! EXIT THEN
   k s" ;package" TOK= IF CONTEXT-RESET EXIT THEN
   PKG-OPEN @ 0= IF EXIT THEN
   k s" public" TOK= IF VIS-PUBLIC PKG-VIS ! EXIT THEN
   k s" private" TOK= IF VIS-PRIVATE PKG-VIS ! THEN ;

: VIS$ ( n -- ptr u8 n ) {: v:n :}
   v VIS-PUBLIC = IF s" public" EXIT THEN
   v VIS-PRIVATE = IF s" private" EXIT THEN
   s" global" ;

\ A site declared outside any package is public at top level; inside one it
\ carries the package's current mode. The scratch replay reproduces exactly that.
: SITE-VIS ( -- n )
   PKG-OPEN @ 0= IF VIS-PUBLIC EXIT THEN PKG-VIS @ ;

\ ---------------------------------------------------------------------------
\ the body span: tokens after the family name up to and including `;ENUM`
\ ---------------------------------------------------------------------------
: END-INDEX ( n -- n )                 \ index of the site's `;ENUM`, or -1
   {: k:n :}
   k 2 +
   BEGIN dup LINT-LEX:COUNT < WHILE
      dup s" ;enum" TOK= IF EXIT THEN
      1 +
   REPEAT drop -1 ;

: BODY-ALL-WORDS? ( n n -- bool ) {: k:n e:n :}   \ every body token is a plain word
   k 2 + BODY-I !
   BEGIN BODY-I @ e < WHILE
      BODY-I @ WORD? 0= IF LINT-FALSE EXIT THEN
      BODY-I @ 1 + BODY-I !
   REPEAT LINT-TRUE ;

: FULL-FORM? ( n n -- bool ) {: k:n e:n :}        \ first body token is an arity
   k 2 + e >= IF LINT-FALSE EXIT THEN
   k 2 + LINT-LEX:TOKEN ALL-DIGITS? ;

\ ---------------------------------------------------------------------------
\ rebuild one site's declaration into its own scratch package
\ ---------------------------------------------------------------------------
\ A site declared inside a package is replayed into a scratch package of its own,
\ so two files may declare the same tail without shadowing each other. A site
\ declared at TOP LEVEL is replayed at top level, bare — wrapping it in a package
\ would change the thing being measured: a global family's constructor package is
\ derived from its bare tail (`SLOT-STATE:EMPTY`), and a package-scoped one from
\ the package (`EC12-SLOT--STATE:EMPTY`). Fourteen of the sites are top level,
\ three of them with no other coverage anywhere, so measuring them in a wrapper
\ would have measured a shape that exists nowhere in the tree.
: PKG-NAME! ( -- )                     \ scratch package: `ec<site index>`, or none at top level
   0 PKG-U !
   PKG-OPEN @ 0= IF EXIT THEN
   s" ec" PKG$+ SITE-N @ DEC$ PKG$+ ;

: BUILD-OPEN ( -- )                    \ package header, for a site that has one
   PKG-U @ 0= IF EXIT THEN
   s" package " BUILD$+ PKG$ BUILD$+
   SITE-VIS VIS-PUBLIC = IF s"  public " BUILD$+ ELSE s"  private " BUILD$+ THEN ;

: BUILD-CLOSE ( -- )
   PKG-U @ 0= IF EXIT THEN
   s"  ;package" BUILD$+ ;

: BUILD-DECL ( n n -- ) {: k:n e:n :}
   0 BUILD-U !
   BUILD-OPEN
   s" ENUM " BUILD$+
   k 1 + LINT-LEX:TOKEN BUILD$+ 32 BUILD-C
   k 2 + BODY-I !
   BEGIN BODY-I @ e < WHILE
      BODY-I @ LINT-LEX:TOKEN BUILD$+ 32 BUILD-C
      BODY-I @ 1 + BODY-I !
   REPEAT
   s" ;ENUM" BUILD$+
   BUILD-CLOSE ;

\ ---------------------------------------------------------------------------
\ the record for one replayed site
\ ---------------------------------------------------------------------------
: RECORD-VARIANT ( n -- ) {: v:n :}
   s"  var=" OUT$+
   v VAR-NAME$ OUT$+
   s" :tag=" OUT$+ v VAR-TAG OUT-N
   s" :ctor=" OUT$+
   v VAR-CTOR-PKG$ dup 0= IF 2drop s" -" OUT$+ ELSE OUT$+ THEN
   s" :sym=" OUT$+
   v VAR-CTOR-SYM 0= IF 0 OUT-N ELSE 1 OUT-N THEN ;

: RECORD-VARIANTS ( n -- ) {: fam:n :}
   fam FAM-VAR-START {: v0:n :}
   fam FAM-VAR-COUNT {: n:n :}
   0 VAR-I !
   BEGIN VAR-I @ n < WHILE
      v0 VAR-I @ + RECORD-VARIANT
      VAR-I @ 1 + VAR-I !
   REPEAT ;

: RECORD-FAMILY ( n -- ) {: fam:n :}
   s"  kind=" OUT$+ fam FAM-KIND OUT-N
   s"  vis=" OUT$+ fam FAM-VIS OUT-N
   s"  vars=" OUT$+ fam FAM-VAR-COUNT OUT-N
   s"  width=" OUT$+ fam FAM-WIDTH OUT-N
   s"  policy=" OUT$+ fam FAM-POLICY OUT-N
   s"  eq=" OUT$+ fam FAM-EQ? 0= IF 0 OUT-N ELSE 1 OUT-N THEN
   s"  hash=" OUT$+ fam FAM-HASH? 0= IF 0 OUT-N ELSE 1 OUT-N THEN
   fam RECORD-VARIANTS ;

\ The header locates the site and names what the source asked for: the owning
\ file, WHICH declaration in that file (first, second, ...), the declared family
\ tail, and the visibility the source gives it.
\
\ The locator is the site's ordinal within its file, not its line number. A line
\ number is not stable under edits that have nothing to do with the declaration —
\ adding a test above one moves it, and the baseline would then report a
\ divergence for a declaration that did not change. The ordinal moves only when a
\ declaration is added, removed, or reordered, which is exactly when a human
\ should look. Line numbers stay out of the compared record; `#2 name=colour` in
\ a named file is enough to find the site.
: RECORD-HEAD ( n -- ) {: k:n :}
   FILE$ OUT$+ 35 OUT-C IN-FILE-N @ OUT-N
   s"  name=" OUT$+ k 1 + LINT-LEX:TOKEN OUT$+
   s"  site=" OUT$+ SITE-VIS VIS$ OUT$+
   IN-FILE-N @ 1 + IN-FILE-N ! ;

: RECORD-RESOLVED ( n -- ) {: k:n :}
   PKG$ k 1 + LINT-LEX:TOKEN FAM-RESOLVE {: fam:n found:bool :}
   found 0= IF
      s"  UNRESOLVED" OUT$+ OUT-NL
      BAD-N @ 1 + BAD-N ! EXIT THEN
   fam RECORD-FAMILY OUT-NL ;

: REPLAY-SITE ( n n -- ) {: k:n e:n :}
   PKG-NAME!
   k e BUILD-DECL
   BUILD$ TRY {: rc:n :}
   s"  rc=" OUT$+ rc OUT-N
   rc 0 <> IF
      s"  REJECTED" OUT$+ OUT-NL
      BAD-N @ 1 + BAD-N ! EXIT THEN
   k RECORD-RESOLVED ;

\ ---------------------------------------------------------------------------
\ scanning
\ ---------------------------------------------------------------------------
: MALFORMED ( n ptr u8 n -- ) {: k:n a:ptr u:n :}
   k RECORD-HEAD
   32 OUT-C a u OUT$+ OUT-NL
   BAD-N @ 1 + BAD-N !
   SITE-N @ 1 + SITE-N ! ;

: SITE ( n -- n ) {: k:n :}            \ record one site; answer the index to resume at
   k 1 + WORD? 0= IF k s" MALFORMED-NAME" MALFORMED k 1 + EXIT THEN
   k END-INDEX {: e:n :}
   e 0 < IF k s" MALFORMED-UNTERMINATED" MALFORMED k 1 + EXIT THEN
   k e BODY-ALL-WORDS? 0= IF k s" MALFORMED-BODY" MALFORMED e 1 + EXIT THEN
   k e FULL-FORM? IF FULL-N @ 1 + FULL-N ! e 1 + EXIT THEN
   k RECORD-HEAD
   REPLAY? @ 0= IF OUT-NL SITE-N @ 1 + SITE-N ! e 1 + EXIT THEN
   k e REPLAY-SITE
   SITE-N @ 1 + SITE-N !
   e 1 + ;

\ ---------------------------------------------------------------------------
\ the second-parser tripwire
\
\ The per-site comparison cannot notice a legacy compact parser coming back,
\ because the whole point of the cutover is that the two parsers agree on every
\ plain declaration. This does notice: the names the legacy branch owned are
\ retired, and the same structural scan that finds declaration sites reports any
\ file that defines one of them again, or that adds back a primitive-axiom row
\ for the ENUM keyword. Comments and strings are separate token kinds, so writing
\ one of these names in prose - as this file's own header does - is not a hit;
\ only a real definition name or a real axiom row is.
\ ---------------------------------------------------------------------------
: RETIRED-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" CHECKER-DEFENUM" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" CHECKER-DEFENUM-BODY" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" ENUM-COLLECT" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" TDECL-ENUM-NOEND-BODY" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" TDECL-ENUM-VARIANT" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" TDECL-ENUM-VARIANTS" LINT-STR=CI ;

: RETIRED-ROW? ( n -- bool ) {: k:n :}
   k LINT-LEX:COUNT >= IF LINT-FALSE EXIT THEN
   k LINT-LEX:KIND@ LINT-LEX:REGISTRY = 0= IF LINT-FALSE EXIT THEN
   k LINT-LEX:TOKEN s" PRIM: ENUM " LINT-STARTS-WITH? IF LINT-TRUE EXIT THEN
   k LINT-LEX:TOKEN s" PRIM: CHECKER-DEFENUM " LINT-STARTS-WITH? ;

: REPORT-RETIRED ( n ptr u8 n -- ) {: k:n what:ptr whatu:n :}
   FILE$ OUT$+ 58 OUT-C k LINT-LEX:LINE@ OUT-N
   s"  SECOND-PARSER " OUT$+ what whatu OUT$+ 32 OUT-C
   k LINT-LEX:TOKEN OUT$+ OUT-NL
   BAD-N @ 1 + BAD-N ! ;

: RETIRED-CHECK ( n -- ) {: k:n :}
   TRIPWIRE? @ 0= IF EXIT THEN
   k RETIRED-ROW? IF k s" axiom-row" REPORT-RETIRED EXIT THEN
   k WORD? 0= IF EXIT THEN
   k NAME-POSITION? 0= IF EXIT THEN
   k LINT-LEX:TOKEN RETIRED-NAME? IF k s" definition" REPORT-RETIRED THEN ;

: SCAN ( -- )
   CONTEXT-RESET
   0 SCAN-I !
   BEGIN SCAN-I @ LINT-LEX:COUNT < WHILE
      SCAN-I @ RETIRED-CHECK
      SCAN-I @ OPENER? IF
         SCAN-I @ SITE SCAN-I !
      ELSE
         SCAN-I @ CONTEXT-STEP
         SCAN-I @ 1 + SCAN-I !
      THEN
   REPEAT ;

: ALLOC-NEED ( n -- n ) {: n:n :} n 0 <= IF 1 EXIT THEN n ;

: LOAD-SOURCE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE ALLOC-NEED MEM-ALLOC-64K-SPAN SRC-CAP ! SRC-A!
   path pathu SRC-A@ SRC-CAP @ READ-ALL SRC-U ! ;

public

\ FILE ( path -- ) : scan one source file. The label in the report is the path
\ as given, so a fixture can scan a temporary file under its own name.
: FILE-AS ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n label:ptr labelu:n :}
   label FILE-A!  labelu FILE-U !
   0 IN-FILE-N !
   path pathu LOAD-SOURCE
   FILE-N @ 1 + FILE-N !
   SRC-A@ SRC-U @ LINT-LEX:SOURCE
   LINT-LEX:ERROR? IF
      FILE$ OUT$+ s"  LEX-ERROR" OUT$+ OUT-NL
      BAD-N @ 1 + BAD-N ! EXIT THEN
   SCAN ;

: FILE ( ptr u8 n -- ) 2dup FILE-AS ;

private

: FORTH-FILE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT? IF LINT-TRUE EXIT THEN
   a u s" .fs" HAS-EXT? ;

: WALK-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FORTH-FILE? 0= IF EXIT THEN
   a u FILE ;

public

: RESET ( -- )
   0 OUT-U !  0 SITE-N !  0 FULL-N !  0 FILE-N !  0 BAD-N !  0 IN-FILE-N !
   -1 REPLAY? !  -1 TRIPWIRE? !
   CONTEXT-RESET ;

: SCAN-ONLY! ( -- ) 0 REPLAY? ! ;      \ fixtures that only exercise site detection

\ Recording a baseline turns the second-parser tripwire off, for the same reason
\ PROVE-UNIFIED belongs to the verifying run only: the tripwire is a claim about
\ the tree AFTER the cutover, and the baseline is deliberately taken on the tree
\ BEFORE it, where the legacy words are supposed to be present. Leaving it armed
\ would make the tool unable to record the one baseline it exists to compare
\ against. It says nothing about the sites, which is all a baseline holds.
: TRIPWIRE-OFF! ( -- ) 0 TRIPWIRE? ! ;

: SITES ( -- n ) SITE-N @ ;
: FULL-SITES ( -- n ) FULL-N @ ;
: FILES ( -- n ) FILE-N @ ;
: FINDINGS ( -- n ) BAD-N @ ;
: REPORT$ ( -- ptr u8 n ) OUT$ ;

\ The trees a declaration can live in. Everything under them that ends in .f or
\ .fs is scanned; nothing inside them is skipped, because a site the census does
\ not see is a site the cutover was never checked against.
\
\ The list itself is the weak point: a repository that grows a SIXTH top-level
\ tree of Forth sources would still walk five, still find every site in those
\ five, and still compare clean against the baseline — a silent hole exactly
\ where a new declaration is most likely to appear. So the count of files
\ actually walked is pinned. It is a deliberately loud, deliberately annoying
\ guard: adding or deleting Forth files anywhere moves it, and the number has to
\ be updated with the change that moved it, which is also when somebody notices
\ that a new tree exists. Raising it without looking at WHAT moved defeats it.
1271 constant WALKED-FILES               \ .f/.fs files under the five trees below

: WALK-TREES ( -- )
   s" src" [: WALK-FILE ;] WALK-FILES
   s" lib" [: WALK-FILE ;] WALK-FILES
   s" tools" [: WALK-FILE ;] WALK-FILES
   s" test" [: WALK-FILE ;] WALK-FILES
   s" maki" [: WALK-FILE ;] WALK-FILES ;

: REQUIRE-WALKED ( -- )
   FILE-N @ WALKED-FILES = IF EXIT THEN
   s" enum-census: walked " type FILE-N @ DEC$ type
   s"  file(s), expected " type WALKED-FILES DEC$ type cr
   s" enum-census:   Forth sources were added or removed, or a whole top-level" type cr
   s" enum-census:   tree is missing from WALK-TREES. Check which, then update" type cr
   s" enum-census:   WALKED-FILES in tools/enum-census-core.f." type cr
   1 throw ;

: WALK ( -- )
   WALK-TREES
   REQUIRE-WALKED ;

: SUMMARY ( -- )
   s" enum-census: " type FILES DEC$ type s"  file(s), " type
   SITES DEC$ type s"  plain site(s), " type
   FULL-SITES DEC$ type s"  full-form site(s), " type
   FINDINGS DEC$ type s"  finding(s)" type cr ;

\ ---- baseline compare ------------------------------------------------------
\ The comparison is byte-for-byte over the whole report. Anything less would
\ have to decide which fields matter, and the point of the census is that none
\ of them may move without somebody saying so.
: BASELINE-LOAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE ALLOC-NEED MEM-ALLOC-64K-SPAN BASE-CAP ! BASE-A!
   path pathu BASE-A@ BASE-CAP @ READ-ALL BASE-U ! ;

: FIRST-DIFF ( -- n )                  \ byte offset of the first difference, or -1
   REPORT$ {: ra:ptr ru:n :}
   BASE$ {: ba:ptr bu:n :}
   0
   BEGIN dup ru < over bu < and WHILE
      dup ra + c@  over ba + c@ <> IF EXIT THEN
      1 +
   REPEAT
   ru bu = IF drop -1 EXIT THEN ;

\ Where the divergence is, in source terms: the start of the report line that
\ contains the first differing byte. A byte offset alone would not name a site.
: LINE-START ( ptr u8 n n -- n ) {: a:ptr u:n off:n :}
   off u >= IF u ELSE off THEN {: p:n :}
   p
   BEGIN dup 0 > WHILE
      dup 1 - a + c@ 10 = IF EXIT THEN
      1 -
   REPEAT ;

: LINE-END ( ptr u8 n n -- n ) {: a:ptr u:n off:n :}
   off
   BEGIN dup u < WHILE
      dup a + c@ 10 = IF EXIT THEN
      1 +
   REPEAT ;

: SHOW-LINE ( ptr u8 n ptr u8 n n -- ) {: la:ptr lu:n a:ptr u:n off:n :}
   la lu type
   a u off LINE-START {: s:n :}
   a u off LINE-END {: e:n :}
   a s + e s - type cr ;

: VERIFY ( ptr u8 n -- )               \ compare the report against a baseline file
   BASELINE-LOAD
   FIRST-DIFF {: d:n :}
   d 0 < IF
      s" enum-census: baseline identical (" type REPORT$ nip DEC$ type s"  bytes)" type cr
      EXIT THEN
   s" enum-census: BASELINE DIVERGENCE at byte " type d DEC$ type
   s"  (report " type REPORT$ nip DEC$ type s"  bytes, baseline " type
   BASE$ nip DEC$ type s"  bytes)" type cr
   s" enum-census:   baseline: " BASE$ d SHOW-LINE
   s" enum-census:   now:      " REPORT$ d SHOW-LINE
   1 throw ;

: EMIT ( -- ) REPORT$ type ;

\ ---- the front end behind the keyword --------------------------------------
\ The per-site comparison above proves the SITES did not move. It cannot, on its
\ own, prove WHICH parser answered: the whole point of the cutover is that the
\ unified front end agrees with the legacy definer on every plain declaration, so
\ a tree that quietly went back to the old parser would compare clean. These
\ three checks answer that question directly, and they belong to the verifying
\ run only - a baseline recorded on the pre-cutover commit is expected to fail
\ them, which is what makes the comparison a before/after check at all.
: PROVE-FULL-FORM ( -- )
   \ The full named-variant form is grammar only the unified front end has. The
   \ legacy compact parser read every token after the name as a variant name and
   \ refused an arity outright, so this accepting is proof of which parser ran.
   s" package ecprobe public ENUM censusprobe 0 VARIANT one ;VARIANT ;ENUM ;package"
   TRY {: rc:n :}
   rc 0= IF EXIT THEN
   s" enum-census: the global ENUM keyword refused the full form (rc " type rc DEC$ type
   s" ) - it is not ENUM-DECL:ED-RUN" type cr
   1 throw ;

: PROVE-RESERVED ( -- )
   \ A control word may not name a family. Both parsers refuse it, but only after
   \ the front ends learned the shared list; a front end without it accepts.
   DIAG-CAPTURE
   s" package ecprobe2 public ENUM if red green ;ENUM ;package" TRY {: rc:n :}
   DIAG-RELEASE
   rc 7110 = IF EXIT THEN
   s" enum-census: `ENUM if ...` answered " type rc DEC$ type
   s"  instead of 7110 - the reserved-name list is not shared" type cr
   1 throw ;

: PROVE-UNIFIED ( -- )
   PROVE-FULL-FORM
   PROVE-RESERVED
   s" enum-census: the global ENUM keyword is the unified front end" type cr ;

\ ---- argv-free committed-baseline verification ------------------------------
\ The whole verify sequence as one word, so the gate
\ (test/gate-stdlib-lint-tools.f) can run it directly: a gate child inherits the
\ pool's own argv (--pool-slots ...), which the CLI's strict argv parse rightly
\ refuses, so the gate must never reach verification THROUGH the CLI file. The
\ CLI's verify verb dispatches here too - one implementation, two callers.
: BASELINE-PATH$ ( -- ptr u8 n )       \ the committed baseline artifact
   s" tools/enum-census-baseline.txt" ;

: REQUIRE-CLEAN ( -- )                 \ a census that cannot replay proves nothing
   FINDINGS 0= IF EXIT THEN
   s" enum-census: " type FINDINGS DEC$ type s"  site(s) failed to replay" type cr
   1 throw ;

: VERIFY-COMMITTED ( -- )              \ tree vs the committed baseline
   PROVE-UNIFIED
   RESET
   WALK
   SUMMARY
   REQUIRE-CLEAN
   BASELINE-PATH$ VERIFY ;

;package
