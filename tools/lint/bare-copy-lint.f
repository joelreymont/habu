\ bare-copy-lint.f -- a raw byte copy and the span mint belong to lib/ and src/.
\
\ Two words cross from "a pointer" to "a pointer and how far it reaches", and
\ both of them take the reach on the caller's word: BYTE-COPY writes `n` bytes
\ into a destination nothing checked, and SPAN:MAKE turns an address and a number
\ into a span whose reach every later bounds check trusts. Inside lib/ and src/
\ that crossing is the library's own business - a producer there publishes spans
\ and BYTE-COPY is how SPAN:COPY moves the bytes once the reach has been checked.
\ Everywhere else it is a consumer re-doing by hand what the span type already
\ does, so this lint names the site and the replacement.
\
\ Run: bin/hb --load tools/lint/text.f tools/lint/token.f tools/lint/lib.f \
\              tools/lint/source-lex.f tools/lint/bare-copy-lint.f
\
\ NOT A GATE YET. Band 0 of habu-bound-pointers reports; the consumer bands
\ convert their files and the band that finishes them turns REPORT into a die.
\
\ Three false alarms are ruled out by construction. Prose is not code: the scan
\ runs on tools/lint/source-lex.f, which consumes `s" ... "`, `."`, `c"` bodies
\ and `( ... )` / `\ ...` comments the way the real parser does, so the words
\ named in this very comment are not findings. A longer name that merely starts
\ the same way is not the word: BYTE-COPY-LEN is counted separately and never
\ reported, because the match is whole-token. And ownership is decided by an
\ ANCHORED path prefix, so test/lib/x.f is a consumer while lib/adt/x.f is not.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

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

\ ---- ownership: an anchored path prefix, never a substring --------------------
\ The walk hands back `./lib/x.f`; the leading `./` is dropped once, here, so the
\ anchored prefix below answers about the repository path and not about the walk
\ root's spelling.
: REL$ ( ptr u8 n -- ptr u8 n ) {: pa:ptr pu:n :}
   pa pu s" ./" LINT-PREFIX? if pa 2 + pu 2 - exit then
   pa pu ;

: OWNED? ( ptr u8 n -- bool ) {: pa:ptr pu:n :}
   pa pu s" lib/" LINT-PREFIX? if LINT-TRUE exit then
   pa pu s" src/" LINT-PREFIX? ;

: COUNT-DIR ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu s" lib/" LINT-PREFIX? if N-LIB BUMP exit then
   pa pu s" src/" LINT-PREFIX? if N-SRC BUMP exit then
   pa pu s" tools/" LINT-PREFIX? if N-TOOLS BUMP exit then
   pa pu s" test/" LINT-PREFIX? if N-TEST BUMP exit then
   pa pu s" examples/" LINT-PREFIX? if N-EX BUMP exit then
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

\ ---- token classification -----------------------------------------------------
\ Only WORD tokens are looked at, so a COMMENT body and a REGISTRY row never
\ match, and a string literal is one opaque WORD token whose text is the whole
\ literal - never the bare name.
: LEX-WORD= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD <> if LINT-FALSE exit then
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

: USING-SPAN? ( n -- bool ) {: k:n :}
   k s" using" LEX-WORD= LINT-NOT if LINT-FALSE exit then
   k 1+ LINT-LEX:COUNT >= if LINT-FALSE exit then
   k 1+ s" SPAN" LEX-WORD= ;

: LINT-TOKEN ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   k USING-SPAN? if LINT-TRUE IN-USING ! exit then
   k s" BYTE-COPY-LEN" LEX-WORD= if NEAR BUMP exit then
   k s" BYTE-COPY" LEX-WORD= if pa pu k FIND-COPY exit then
   k s" SPAN:MAKE" LEX-WORD= if pa pu k FIND-MINT exit then
   IN-USING @ if
      k s" MAKE" LEX-WORD= if pa pu k FIND-MINT then
   then ;

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

: LINT-SCAN ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n a:ptr u:n :}
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? if pa pu BCL-LEX-FAIL then
   LINT-FALSE IN-USING !
   pa pu OWNED? if exit then
   0 LI !
   begin LI @ LINT-LEX:COUNT < while
      pa pu LI @ LINT-TOKEN
      LI @ 1+ LI !
   repeat ;

: LINT-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   FILES BUMP
   pa pu FB-LOAD {: a:ptr u:n :}
   pa pu a u LINT-SCAN ;

\ ---- the tree -----------------------------------------------------------------
\ lib/fs.f's walk already skips .git, .jj, .jj-ws and .dots, so a sibling
\ workspace is never read. Only `.f` sources are scanned.
: WALK-ONE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu s" .f" HAS-EXT? LINT-NOT if exit then
   pa pu REL$ LINT-FILE ;

: DIR. ( ptr u8 n ptr n -- ) {: a:ptr u:n v:ptr :}
   s"   " type a u type s" =" type v @ . ;

: REPORT ( -- )
   s" bare-copy: files=" type FILES @ .
   s" bare-copy: findings=" type BAD @ .
   s" bare-copy: byte-copy-len sightings=" type NEAR @ .
   s" lib" N-LIB DIR.
   s" src" N-SRC DIR.
   s" tools" N-TOOLS DIR.
   s" test" N-TEST DIR.
   s" examples" N-EX DIR.
   s" other" N-OTHER DIR. ;

: CENSUS ( -- )
   RESET
   s" ." [: WALK-ONE ;] WALK-FILES
   REPORT ;

CENSUS

;package
