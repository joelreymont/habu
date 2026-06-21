\ grade-test.f - focused tests for native benchmark grader.
\
\ Load after lib/errors.f, lib/string.f, lib/test.f, lib/fs.f,
\ lib/fs-mutate.f, lib/process.f, lib/process-argv.f, and bench/llm/grade.f.

3000 constant GRT-TIMEOUT-MS
200 constant GRT-QUICK-TIMEOUT-MS

create GRT-ROOT-BUF FS-PATH-CAP allot
create GRT-CAND-BUF FS-PATH-CAP allot
create GRT-VEC-BUF FS-PATH-CAP allot

variable GRT-ROOT-U
variable GRT-CAND-U
variable GRT-VEC-U

: GRT-ROOT$ ( -- ptr u8 n )
   GRT-ROOT-BUF GRT-ROOT-U @ ;

: GRT-CAND$ ( -- ptr u8 n )
   GRT-CAND-BUF GRT-CAND-U @ ;

: GRT-VEC$ ( -- ptr u8 n )
   GRT-VEC-BUF GRT-VEC-U @ ;

: GRT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: GRT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr up:ptr :}
   pa pu na nu dst JOIN-PATH up ! ;

: GRT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-grade-test" TMPDIR-MKDIR GRT-ROOT-BUF GRT-ROOT-U GRT-COPY!
   GRT-ROOT$ CLEANUP-TREE+
   GRT-ROOT$ s" cand.f" GRT-CAND-BUF GRT-CAND-U GRT-PATH!
   GRT-ROOT$ s" vec.f" GRT-VEC-BUF GRT-VEC-U GRT-PATH! ;

: GRT-WRITE ( ptr u8 n ptr u8 n -- ) {: cand:ptr candu vec:ptr vecu :}
   GRT-CAND$ cand candu WRITE-ALL
   GRT-VEC$ vec vecu WRITE-ALL ;

: GRT-CHECK-MS ( ptr u8 n ptr u8 n ptr u8 n n -- ) {: want:ptr wantu cand:ptr candu vec:ptr vecu timeout :}
   cand candu vec vecu GRT-WRITE
   STR-FALSE timeout GRT-CAND$ GRT-VEC$ GR-RUN-FILES want wantu T$= ;

: GRT-CHECK ( ptr u8 n ptr u8 n ptr u8 n -- )
   GRT-TIMEOUT-MS GRT-CHECK-MS ;

: GRT-CHECK-NOCHECK ( ptr u8 n ptr u8 n ptr u8 n -- ) {: want:ptr wantu cand:ptr candu vec:ptr vecu :}
   cand candu vec vecu GRT-WRITE
   STR-TRUE GRT-TIMEOUT-MS GRT-CAND$ GRT-VEC$ GR-RUN-FILES want wantu T$= ;

: GRT-PASS ( -- )
   s" pass" s" : SQ ( i64 -- i64 ) dup * ;" s" 7 SQ 49 G=" GRT-CHECK ;

: GRT-FAIL ( -- )
   s" fail" s" : SQ ( i64 -- i64 ) dup * ;" s" 7 SQ 50 G=" GRT-CHECK ;

: GRT-REJECT ( -- )
   s" reject" s" : SQ ( i64 -- i64 ) dup ;" s" 7 SQ 49 G=" GRT-CHECK ;

: GRT-TRAP ( -- )
   s" trap" s" : DZ ( i64 -- i64 ) 0 / ;" s" 7 DZ 0 G=" GRT-CHECK ;

: GRT-TIMEOUT ( -- )
   s" timeout" s" : LP ( -- ) begin again ;" s" LP" GRT-QUICK-TIMEOUT-MS GRT-CHECK-MS ;

: GRT-NOCHECK ( -- )
   s" pass" s" : SQ ( i64 -- i64 ) dup ;" s" 7 SQ 7 G=" GRT-CHECK-NOCHECK ;

: GRT-MISSING ( -- )
   STR-FALSE GRT-TIMEOUT-MS s" no-such-habu-grade-candidate" GRT-VEC$ GR-RUN-FILES s" error" T$= ;

: GRT-MAIN ( -- )
   T-RESET
   GRT-PREPARE
   GRT-PASS
   GRT-FAIL
   GRT-REJECT
   GRT-TRAP
   GRT-TIMEOUT
   GRT-NOCHECK
   GRT-MISSING
   CLEANUP-RUN
   GRT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" grade-test: ok" type cr ;

GRT-MAIN
