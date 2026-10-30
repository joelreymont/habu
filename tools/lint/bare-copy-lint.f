\ bare-copy-lint.f -- BYTE-COPY and SPAN:MAKE take a reach on the caller's word:
\ outside src/ (and in src/examples/) a use redoes what the span type does. PATHZ
\ and PATH0 copy a path for the engine's OS primitives; outside PATH-OWNED? a path
\ goes through FS:>PATHZ or FFI:CSTR. Names each site; not a gate yet, never dies.
\ Run, from the habu root: bin/hb --load tools/lint/bare-copy-lint.f
\ For a consumer tree:    bin/hb --load tools/lint/bare-copy-lint.f -- /path/to/tree

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f
require src/lib/adt/option.f
require tools/lint/def.f

package BARE-COPY-LINT

\ One file at a time in a slab sized from the file (tools/lint/text.f LINT-SLAB).
create FB-SLAB LINT-SLAB:CELLS cells allot

: FB-LOAD ( ptr u8 n -- ptr u8 n ) {: pa:ptr pu:n :}
   pa pu FB-SLAB LINT-SLAB:LOAD
   FB-SLAB LINT-SLAB:TEXT ;

\ ---- counters ----------------------------------------------------------------
variable BAD          \ findings in this run
variable FILES        \ files scanned
variable NEAR         \ BYTE-COPY-LEN sightings (informational, never a finding)
variable IN-USING     \ this file said `using SPAN`, so a bare MAKE is the mint
variable OWNS-COPY    \ this file owns BYTE-COPY and SPAN:MAKE
variable OWNS-PATH    \ this file owns PATHZ and PATH0
variable LI

\ Per-directory finding counts. The bucket is the path's FIRST segment, which is
\ what makes the report readable as "who still has to convert".
variable N-LIB   variable N-SRC   variable N-TOOLS
variable N-TEST  variable N-EX    variable N-OTHER

: RESET ( -- )
   0 BAD ! 0 FILES ! 0 NEAR !
   0 N-LIB ! 0 N-SRC ! 0 N-TOOLS ! 0 N-TEST ! 0 N-EX ! 0 N-OTHER ! ;

: BUMP ( ptr n -- ) {: v:ptr :}
   v @ 1+ v ! ;

\ ---- the tree to lint ---------------------------------------------------------
\ The one optional argument names a consumer tree, linted from the habu root
\ where the lint's own `require` rows resolve; without it the walk starts at `.`.
create ROOT-BUF PATH-CAP allot
variable ROOT-U

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: ROOT! ( -- )
   SCRIPT-ARGC 0= if [char] . ROOT-BUF c! 1 ROOT-U ! exit then
   0 SCRIPT-ARGV$ {: a:ptr u:n :}
   u PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY u ROOT-U ! ;

\ ---- ownership: an anchored path prefix, never a substring --------------------
\ The walk hands back `<root>/src/lib/x.f`; the root's spelling and its slash are
\ dropped once, here, so the anchored prefix below answers about the repository
\ path and not about the walk root's spelling.
: REL$ ( ptr u8 n -- ptr u8 n ) {: pa:ptr pu:n :}
   ROOT-U @ {: ru:n :}
   pa pu ROOT$ LINT-PREFIX? if
      pu ru > if
         ROOT$ FS:JOIN-SEPARATED? if pa ru + pu ru - exit then
         pa ru + c@ [char] / = if pa ru + 1+ pu ru - 1- exit then
      then
   then
   pa pu ;

: OWNED? ( ptr u8 n -- bool ) {: pa:ptr pu:n :}
   pa pu s" src/examples/" LINT-PREFIX? if LINT-FALSE exit then
   pa pu s" src/" LINT-PREFIX? ;

\ The engine's copier also belongs to test/engine-suite.f, which tests those OS
\ primitives, and to the two tier tools, which load nothing but code-bytes.f:
\ a library they loaded would weigh nothing in their corpus (tools/tier-census.f).
\ A hand-written `0 ... c!` terminator cannot be linted; review guards it.
: PATH-OWNED? ( ptr u8 n -- bool )
   {: pa:ptr pu:n :}
   pa pu s" src/core/" LINT-PREFIX? if LINT-TRUE exit then
   pa pu s" src/habu/" LINT-PREFIX? if LINT-TRUE exit then
   pa pu s" tools/tier-census.f" LINT-STR= if LINT-TRUE exit then
   pa pu s" tools/tier-dump.f" LINT-STR= if LINT-TRUE exit then
   pa pu s" test/engine-suite.f" LINT-STR= ;

: COUNT-DIR ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu s" src/lib/" LINT-PREFIX? if N-LIB BUMP exit then
   pa pu s" src/examples/" LINT-PREFIX? if N-EX BUMP exit then
   pa pu s" src/" LINT-PREFIX? if N-SRC BUMP exit then
   pa pu s" tools/" LINT-PREFIX? if N-TOOLS BUMP exit then
   pa pu s" test/" LINT-PREFIX? if N-TEST BUMP exit then
   N-OTHER BUMP ;

\ ---- the report line ----------------------------------------------------------
\ `.` ends the line, so the line number goes last and no `cr` follows it.
: SITE. ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   s" bare-copy: " type pa pu type s" : " type k LINT-LEX:TOKEN type ;

: AT-LINE. ( n -- ) {: k:n :}
   s" , line " type k LINT-LEX:LINE@ . ;

: FIND-COPY ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   pa pu k SITE.
   s"  -> SPAN:COPY ( ptr u8 n span u8 -- ) into a span from its producer" type
   k AT-LINE.
   BAD BUMP  pa pu COUNT-DIR ;

: FIND-MINT ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   pa pu k SITE.
   s"  -> take the span from its producer (SPAN-BUFFER:, SPAN-CELLS:, MEM:ALLOC-SPAN) or narrow one" type
   k AT-LINE.
   BAD BUMP  pa pu COUNT-DIR ;

: FIND-PATH ( ptr u8 n n -- )
   {: pa:ptr pu:n k:n :}
   pa pu k SITE.
   s"  -> copy through FFI:CSTR or FS; the engine's copier belongs to its owners (see the header)" type
   k AT-LINE.
   BAD BUMP  pa pu COUNT-DIR ;

\ ---- token classification -----------------------------------------------------
\ Only WORD tokens match, and only whole: a COMMENT body, a REGISTRY row and a
\ string literal (one WORD token whose text is the whole literal) never match,
\ and BYTE-COPY-LEN is not BYTE-COPY.
: LEX-WORD= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD <> if LINT-FALSE exit then
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

: USING-SPAN? ( n -- bool ) {: k:n :}
   k s" using" LEX-WORD= LINT-NOT if LINT-FALSE exit then
   k 1+ LINT-LEX:COUNT >= if LINT-FALSE exit then
   k 1+ s" SPAN" LEX-WORD= ;

: COPY-TOKEN ( ptr u8 n n -- )
   {: pa:ptr pu:n k:n :}
   k s" BYTE-COPY-LEN" LEX-WORD= if NEAR BUMP exit then
   k s" BYTE-COPY" LEX-WORD= if pa pu k FIND-COPY exit then
   k s" SPAN:MAKE" LEX-WORD= if pa pu k FIND-MINT exit then
   IN-USING @ if
      k s" MAKE" LEX-WORD= if pa pu k FIND-MINT then
   then ;

\ ---- what binds PATHZ or PATH0 nearer than the engine ------------------------
\ A name resolves to the nearest binding: a local of the enclosing definition,
\ byte for byte as the engine and the checker compare locals, then a word the
\ file defined above it, case-folded as the dictionary looks words up, and only
\ then the engine's word. The engine hides a definition's name until its closing
\ token, so inside its own body the name is still the engine's word; a storage
\ definer's name binds at once. A word defined inside a package binds until the
\ `;package` that closes it. A local binds to the end of the block that declared
\ it (LINT-LEX:LIVE-LOCAL?). LINT-DEF names the definers and their kinds.
variable DEF-AT       \ the token that opened the body being read, or -1
variable DEF-KIND     \ the LINT-DEF kind of that body
variable DEF-NAME     \ the token naming that body, or -1
variable IN-PKG       \ a `package` is open
variable DEFS-Z       \ how far the file's own PATHZ binds
variable DEFS-0       \ how far the file's own PATH0 binds
0 constant NO-DEF     \ the file has not defined the name
1 constant PKG-DEF    \ defined in the open package
2 constant FILE-DEF   \ defined outside a package: binds to the end of the file

: DEF-MARK ( ptr n -- )
   {: v:ptr :}
   IN-PKG @ 0= if FILE-DEF v ! exit then
   v @ NO-DEF = if PKG-DEF v ! then ;

: DEF-CLOSE ( ptr n -- )
   {: v:ptr :}
   v @ PKG-DEF = if NO-DEF v ! then ;

\ Token k is the name a definer makes; -1 when it made none.
: NAME-DEFINED ( n -- )
   {: k:n :}
   k 0 < if exit then
   k s" PATHZ" LEX-WORD= if DEFS-Z DEF-MARK exit then
   k s" PATH0" LEX-WORD= if DEFS-0 DEF-MARK then ;

\ The token LINT-DEF names after definer k, or -1.
: NAME-AT ( n -- n )
   LINT-DEF:NAME-I MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ A data definer binds its name at once; any other kind opens a body that
\ binds its name at the close.
: OUTSIDE-STEP ( n -- )
   {: k:n :}
   k s" package" LEX-WORD= if LINT-TRUE IN-PKG ! exit then
   k s" ;package" LEX-WORD= if
      LINT-FALSE IN-PKG !  DEFS-Z DEF-CLOSE  DEFS-0 DEF-CLOSE  exit
   then
   k LINT-DEF:DIRECT-KIND {: kind:n :}
   kind LINT-DEF:NONE-KIND = if exit then
   kind LINT-DEF:DATA-KIND = if k NAME-AT NAME-DEFINED exit then
   k DEF-AT !  kind DEF-KIND !  k NAME-AT DEF-NAME ! ;

\ Read token k into the scope that holds after it.
: SCOPE-STEP ( n -- )
   {: k:n :}
   k LINT-LEX:OPERAND? if exit then
   DEF-AT @ 0 < if k OUTSIDE-STEP exit then
   k DEF-KIND @ LINT-DEF:CLOSE? if
      DEF-NAME @ NAME-DEFINED  -1 DEF-AT !  -1 DEF-NAME !
   then ;

\ Token k is a local, not the engine's word, when its name is live after k: a
\ live local's use, or a `{: … :}` group's own name.
: PATH-USE ( ptr u8 n n ptr n -- )
   {: pa:ptr pu:n k:n v:ptr :}
   k DEF-NAME @ = if exit then
   v @ NO-DEF <> if exit then
   k 1+ k LINT-LEX:TOKEN LINT-LEX:LIVE-LOCAL? if exit then
   pa pu k FIND-PATH ;

: PATH-TOKEN ( ptr u8 n n -- )
   {: pa:ptr pu:n k:n :}
   k SCOPE-STEP
   k s" PATHZ" LEX-WORD= if pa pu k DEFS-Z PATH-USE exit then
   k s" PATH0" LEX-WORD= if pa pu k DEFS-0 PATH-USE then ;

: LINT-TOKEN ( ptr u8 n n -- )
   {: pa:ptr pu:n k:n :}
   k USING-SPAN? if LINT-TRUE IN-USING ! exit then
   OWNS-COPY @ 0= if pa pu k COPY-TOKEN then
   OWNS-PATH @ 0= if pa pu k PATH-TOKEN then ;

\ ---- fail-closed on a lexer defect --------------------------------------------
\ An unterminated literal or a malformed axiom row means the lexer swallowed the
\ rest of the file, so later crossings would go unseen. Name the file and throw.
: BCL-LEX-FAIL ( ptr u8 n -- ) {: pa:ptr pu:n :}
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY = if
      s" bare-copy: malformed primitive registry row in " type pa pu type
      s"  at line " type LINT-LEX:ERROR-LINE@ .
      E-SPAN-REGISTRY throw
   then
   s" bare-copy: unterminated string literal in " type pa pu type
   s"  at line " type LINT-LEX:ERROR-LINE@ .
   E-SPAN-UNTERM throw ;

: LINT-SCAN ( ptr u8 n ptr u8 n -- )
   {: pa:ptr pu:n a:ptr u:n :}
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? if pa pu BCL-LEX-FAIL then
   LINT-FALSE IN-USING !
   -1 DEF-AT !  -1 DEF-NAME !  LINT-FALSE IN-PKG !  NO-DEF DEFS-Z !  NO-DEF DEFS-0 !
   pa pu OWNED? OWNS-COPY !
   pa pu PATH-OWNED? OWNS-PATH !
   OWNS-COPY @ if OWNS-PATH @ if exit then then
   0 LI !
   begin LI @ LINT-LEX:COUNT < while
      pa pu LI @ LINT-TOKEN
      LI @ 1+ LI !
   repeat ;

\ The file is opened by the path the walk handed back and reported by its
\ repository path, so a foreign root reads its own files and still buckets them
\ by the anchored prefix.
: LINT-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   FILES BUMP
   pa pu FB-LOAD {: a:ptr u:n :}
   pa pu REL$ a u LINT-SCAN ;

\ ---- the tree -----------------------------------------------------------------
\ src/lib/fs.f's walk already skips .git, .jj, .jj-ws, .dots and .out. The
\ repository's build directory is skipped before stat or descent: its generated
\ trees can disappear while the lint runs.
: WALK-PATH? ( ptr u8 n -- bool ) {: pa:ptr pu:n :}
   pa pu ROOT$ LINT-STR= if LINT-TRUE exit then
   pa pu REL$ s" build" LINT-STR= LINT-NOT ;

: WALK-ONE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu s" .f" HAS-EXT? LINT-NOT if exit then
   pa pu LINT-FILE ;

: DIR. ( ptr u8 n ptr n -- ) {: a:ptr u:n v:ptr :}
   s"   " type a u type s" =" type v @ . ;

: REPORT ( -- )
   s" bare-copy: files=" type FILES @ .
   s" bare-copy: findings=" type BAD @ .
   s" bare-copy: byte-copy-len sightings=" type NEAR @ .
   s" src/lib" N-LIB DIR.
   s" src" N-SRC DIR.
   s" tools" N-TOOLS DIR.
   s" test" N-TEST DIR.
   s" src/examples" N-EX DIR.
   s" other" N-OTHER DIR. ;

: CENSUS ( -- )
   RESET
   ROOT!
   ROOT$ [: WALK-PATH? ;] [: WALK-ONE ;] FS:WALK-FILES-IF
   REPORT ;

CENSUS

;package
