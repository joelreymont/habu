\ filemap-lint.f - keep FILEMAP.md useful as an agent navigation index.
\ Policy is derived, not transcribed: every .f/.fs source file under the
\ walked roots (src/, tools/, test/, lib/, bootstrap/) must appear in FILEMAP.md unless a
\ committed exclusion row names it. Listed paths must exist (staleness),
\ exclusion rows must exist (no stale exclusions), and the contract docs must
\ stay listed.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f,
\ tools/lint/text.f, tools/lint/intern.f, tools/lint/token.f, and
\ tools/lint/lib.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f

$20000 constant FM-BUF-CAP
8 constant FM-ROOT-MAX
16 constant FM-EXC-MAX

create FM-BUF FM-BUF-CAP allot
create FM-NUM 32 allot
create FM-FILEMAP-BUF FS-PATH-CAP allot
create FM-ROOT-PATHS FM-ROOT-MAX FS-PATH-CAP * allot
create FM-ROOT-US FM-ROOT-MAX cells allot
create FM-EXC-PATHS FM-EXC-MAX FS-PATH-CAP * allot
create FM-EXC-US FM-EXC-MAX cells allot

variable FM-LEN
variable FM-I
variable FM-START
variable FM-BAD
variable FM-NUM-L
variable FM-FILEMAP-U
variable FM-ROOT#
variable FM-EXC#

: FM-NL ( -- ) 10 emit ;

: FM-PATHISH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" /" LINT-CONTAINS? if LINT-TRUE exit then
   a u s" .md" HAS-EXT? if LINT-TRUE exit then
   a u s" .sh" HAS-EXT? if LINT-TRUE exit then
   a u s" .f" HAS-EXT? if LINT-TRUE exit then
   a u s" .fs" HAS-EXT? if LINT-TRUE exit then
   a u s" .tsv" HAS-EXT? ;

: FM-POLICY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT? if LINT-TRUE exit then
   a u s" .fs" HAS-EXT? ;

: FM-EXISTS? ( ptr u8 n -- bool )
   LINT-PATHZ PATHBUF 0 access 0= ;

: FM-PRINT-PATH ( ptr u8 n -- )
   96 emit type 96 emit ;

: FM-FIND+ ( -- )
   FM-BAD @ 1+ FM-BAD ! ;

: FM-STALE ( ptr u8 n -- )
   s" FILEMAP-STALE FILEMAP.md: " type
   2dup FM-PRINT-PATH
   s"  does not exist" type FM-NL
   2drop
   FM-FIND+ ;

: FM-MISSING ( ptr u8 n -- )
   s" FILEMAP-MISSING FILEMAP.md: required entry " type
   2dup FM-PRINT-PATH
   s"  is absent" type FM-NL
   2drop
   FM-FIND+ ;

: FM-UNLISTED ( ptr u8 n -- )
   s" FILEMAP-UNLISTED FILEMAP.md: policy source file " type
   2dup FM-PRINT-PATH
   s"  is not listed" type FM-NL
   2drop
   FM-FIND+ ;

: FM-EXC-STALE ( ptr u8 n -- )
   s" FILEMAP-EXCLUDED-STALE filemap-lint: excluded path " type
   2dup FM-PRINT-PATH
   s"  does not exist" type FM-NL
   2drop
   FM-FIND+ ;

: FM-FILEMAP! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a FM-FILEMAP-BUF u BYTE-COPY
   u FM-FILEMAP-U ! ;

: FM-FILEMAP$ ( -- ptr u8 n )
   FM-FILEMAP-BUF FM-FILEMAP-U @ ;

: FM-TBL-SLOT ( ptr a n -- ptr u8 ) {: buf:ptr idx:n :}
   idx 0 < if E-TBL-BOUNDS throw then
   buf idx FS-PATH-CAP * + ;

: FM-TBL+ ( ptr u8 n ptr a ptr a ptr a n -- )
   {: a:ptr u:n paths:ptr us:ptr np:ptr max:n :}
   np @ max >= if E-TBL-BOUNDS throw then
   u 0 < if E-TBL-FIELD throw then
   u FS-PATH-CAP > if E-TBL-FIELD throw then
   a paths np @ FM-TBL-SLOT u BYTE-COPY
   u us np @ cells + !
   np @ 1+ np ! ;

: FM-ROOT+ ( ptr u8 n -- )
   FM-ROOT-PATHS FM-ROOT-US FM-ROOT# FM-ROOT-MAX FM-TBL+ ;

: FM-ROOT$ ( n -- ptr u8 n ) {: idx:n :}
   FM-ROOT-PATHS idx FM-TBL-SLOT
   FM-ROOT-US idx cells + @ ;

: FM-EXC ( ptr u8 n -- )
   FM-EXC-PATHS FM-EXC-US FM-EXC# FM-EXC-MAX FM-TBL+ ;

: FM-EXC$ ( n -- ptr u8 n ) {: idx:n :}
   FM-EXC-PATHS idx FM-TBL-SLOT
   FM-EXC-US idx cells + @ ;

: FM-EXCLUDED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup FM-EXC# @ < while
      dup FM-EXC$ a u LINT-STR= if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: FM-LINT-RESET ( -- )
   0 FM-BAD !
   0 FM-ROOT# !
   0 FM-EXC# ! ;

: FM-SCAN-BTICK ( -- )
   FM-I @ 1+ FM-START !
   FM-I @ 1+ FM-I !
   begin FM-I @ FM-LEN @ < while
      FM-BUF FM-I @ + c@ 96 = if
         FM-BUF FM-START @ +  FM-I @ FM-START @ -  2dup FM-PATHISH? if INTERN drop else 2drop then
         FM-I @ 1+ FM-I !
         exit
      then
      FM-I @ 1+ FM-I !
   repeat ;

: FM-SCAN-PATHS ( -- )
   INTERN-RESET
   FM-FILEMAP$ FM-BUF FM-BUF-CAP READ-FILE nip FM-LEN !
   0 FM-I !
   begin FM-I @ FM-LEN @ < while
      FM-BUF FM-I @ + c@ 96 = if
         FM-SCAN-BTICK
      else
         FM-I @ 1+ FM-I !
      then
   repeat ;

: FM-CHECK-PATHS ( -- )
   0 begin dup INTERN# < while
      dup INTERN$ 2dup FM-EXISTS? 0= if FM-STALE else 2drop then
      1+
   repeat drop ;

: FM-REQ ( ptr u8 n -- )
   2dup INTERN? 0= if FM-MISSING else 2drop then ;

: FM-CHECK-EXCLUSIONS ( -- )
   0 begin dup FM-EXC# @ < while
      dup FM-EXC$ 2dup FM-EXISTS? 0= if FM-EXC-STALE else 2drop then
      1+
   repeat drop ;

: FM-TREE-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FM-POLICY? 0= if exit then
   a u FM-EXCLUDED? if exit then
   a u INTERN? 0= if a u FM-UNLISTED then ;

: FM-CHECK-TREE ( -- )
   0 begin dup FM-ROOT# @ < while
      dup FM-ROOT$ [: FM-TREE-FILE ;] WALK-FILES
      1+
   repeat drop ;

: FM-U. ( n -- )
   0 FM-NUM-L !
   dup 0= if drop 48 emit exit then
   begin dup 0 > while
      dup 10 mod 48 + FM-NUM FM-NUM-L @ + c!
      10 /
      FM-NUM-L @ 1+ FM-NUM-L !
   repeat drop
   begin FM-NUM-L @ 0 > while
      FM-NUM-L @ 1- FM-NUM-L !
      FM-NUM FM-NUM-L @ + c@ emit
   repeat ;

: FM-REPORT ( -- )
   s" filemap-lint: " type INTERN# FM-U.
   s"  path(s), " type FM-BAD @ FM-U.
   s"  finding(s)" type FM-NL
   FM-BAD @ 0 > if 1 throw then ;

: FM-ROOTS! ( -- )
   s" src" FM-ROOT+
   s" tools" FM-ROOT+
   s" test" FM-ROOT+
   s" lib" FM-ROOT+
   s" bootstrap" FM-ROOT+ ;

\ Committed exclusions: policy files deliberately kept out of the index.
\ Every row must still exist on disk; stale rows fail the lint.
: FM-EXCLUSIONS! ( -- )
   s" tools/lint/clobber-sys-x8-fixture.f" FM-EXC
   s" tools/ptx/saxpy-wrong-cg.f" FM-EXC ;

: FM-CHECK-REQUIRED-DOCS ( -- )
   s" AGENTS.md" FM-REQ
   s" LLM.md" FM-REQ
   s" LESSONS.md" FM-REQ
   s" STATUS.md" FM-REQ
   s" TRUSTED.md" FM-REQ
   s" docs/parallel-agents.md" FM-REQ
   s" docs/seed.md" FM-REQ
   s" docs/swiftforth-task-api.md" FM-REQ
   s" docs/type-families.md" FM-REQ ;

: FILEMAP-LINT ( -- )
   FM-LINT-RESET
   s" FILEMAP.md" FM-FILEMAP!
   FM-ROOTS!
   FM-EXCLUSIONS!
   FM-SCAN-PATHS
   FM-CHECK-PATHS
   FM-CHECK-REQUIRED-DOCS
   FM-CHECK-EXCLUSIONS
   FM-CHECK-TREE
   FM-REPORT ;

: FM-MAIN ( -- )
   [: FILEMAP-LINT ;] catch {: code:n :}
   s" filemap-lint" code LINT-MAIN ;

FM-MAIN
