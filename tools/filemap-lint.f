\ filemap-lint.f - keep FILEMAP.md useful as an agent navigation index.
\ Load after tools/lint/lib.f.

0 set-check

create FM-BUF $20000 allot
create FM-NUM 32 allot

variable FM-LEN
variable FM-I
variable FM-START
variable FM-BAD
variable FM-NUM-L

: FM-NL ( -- ) 10 emit ;

: FM-PATHISH? {: a u :} ( a u -- f )
   a u s" /" CONTAINS? IF -1 exit THEN
   a u s" .md" HAS-EXT? IF -1 exit THEN
   a u s" .sh" HAS-EXT? IF -1 exit THEN
   a u s" .py" HAS-EXT? IF -1 exit THEN
   a u s" .f" HAS-EXT? IF -1 exit THEN
   a u s" .fs" HAS-EXT? IF -1 exit THEN
   a u s" .tsv" HAS-EXT? ;

: FM-EXISTS? ( a u -- f )
   PATHZ PATHBUF 0 access 0= ;

: FM-PRINT-PATH ( a u -- )
   96 emit type 96 emit ;

: FM-STALE ( a u -- )
   s" FILEMAP-STALE FILEMAP.md: " type
   2dup FM-PRINT-PATH
   s"  does not exist" type FM-NL
   2drop
   FM-BAD @ 1+ FM-BAD ! ;

: FM-MISSING ( a u -- )
   s" FILEMAP-MISSING FILEMAP.md: required entry " type
   2dup FM-PRINT-PATH
   s"  is absent" type FM-NL
   2drop
   FM-BAD @ 1+ FM-BAD ! ;

: FM-SCAN-BTICK ( -- )
   FM-I @ 1+ FM-START !
   FM-I @ 1+ FM-I !
   begin FM-I @ FM-LEN @ < while
      FM-BUF FM-I @ + c@ 96 = IF
         FM-BUF FM-START @ +  FM-I @ FM-START @ -  2dup FM-PATHISH? IF INTERN drop ELSE 2drop THEN
         FM-I @ 1+ FM-I !
         exit
      THEN
      FM-I @ 1+ FM-I !
   repeat ;

: FM-SCAN-PATHS ( -- )
   INTERN-RESET
   s" FILEMAP.md" FM-BUF $20000 READ-FILE nip FM-LEN !
   0 FM-I !
   begin FM-I @ FM-LEN @ < while
      FM-BUF FM-I @ + c@ 96 = IF
         FM-SCAN-BTICK
      ELSE
         FM-I @ 1+ FM-I !
      THEN
   repeat ;

: FM-CHECK-PATHS ( -- )
   0 begin dup INTERN# < while
      dup INTERN$ 2dup FM-EXISTS? 0= IF FM-STALE ELSE 2drop THEN
      1+
   repeat drop ;

: FM-REQ ( a u -- )
   2dup INTERN? 0= IF FM-MISSING ELSE 2drop THEN ;

: FM-CHECK-REQUIRED ( -- )
   s" AGENTS.md" FM-REQ
   s" LLM.md" FM-REQ
   s" LESSONS.md" FM-REQ
   s" STATUS.md" FM-REQ
   s" TRUSTED.md" FM-REQ
   s" src/core/checker.f" FM-REQ
   s" src/core/render.f" FM-REQ
   s" src/habu/aot.f" FM-REQ
   s" src/habu/build.f" FM-REQ
   s" tools/check.sh" FM-REQ
   s" tools/hb-build.sh" FM-REQ
   s" tools/signature-lint.py" FM-REQ
   s" tools/aot-lint.py" FM-REQ
   s" tools/forth_lex.py" FM-REQ
   s" tools/diag-origin.py" FM-REQ
   s" tools/json-only.f" FM-REQ
   s" tools/check-all-errors.py" FM-REQ
   s" tools/diag-to-sarif.f" FM-REQ
   s" tools/public-signatures.f" FM-REQ
   s" tools/aot-call-report.f" FM-REQ
   s" tools/filemap-lint.f" FM-REQ
   s" tools/trust-lint.f" FM-REQ
   s" tools/trust-lint-test.sh" FM-REQ
   s" tools/stale-status-lint.f" FM-REQ
   s" test/run.sh" FM-REQ
   s" test/t-sh-jdiag.fs" FM-REQ
   s" bench/llm/tasks.tsv" FM-REQ ;

: FM-U. ( u -- )
   0 FM-NUM-L !
   dup 0= IF drop 48 emit exit THEN
   begin dup 0 > while
      dup 10 mod 48 + FM-NUM FM-NUM-L @ + c!
      10 /
      FM-NUM-L @ 1+ FM-NUM-L !
   repeat drop
   begin FM-NUM-L @ 0 > while
      FM-NUM-L @ 1- FM-NUM-L !
      FM-NUM FM-NUM-L @ + c@ emit
   repeat ;

: FILEMAP-LINT ( -- )
   0 FM-BAD !
   FM-SCAN-PATHS
   FM-CHECK-PATHS
   FM-CHECK-REQUIRED
   s" filemap-lint: " type INTERN# FM-U. s"  path(s), " type FM-BAD @ FM-U. s"  finding(s)" type FM-NL
   FM-BAD @ 0 > IF 1 throw THEN ;

FILEMAP-LINT
