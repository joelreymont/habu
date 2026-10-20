\ Build and run one stripped application whose root's repeated DATA loads are
\ more than the literal-load window away from the final pointer pool.
require test/gate-common.f
require test/gate-aot-image.f
require src/lib/string.f
require src/lib/engine-candidate.f

package STRIPPED-POOL-REACH-TEST
private

2048 constant CHAIN-N
128 constant STEP-N
900000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: NUM+ ( n -- ) {: value:n :}
   value 10 >= if value 10 / recurse then
   value 10 mod STR-ZERO + SB-APPEND-C ;

\ Small straight-line bodies provide reachable code beyond literal-load reach
\ without making every increment a dictionary entry.
: WORD! ( n -- )
   {: k:n :}
   SB-RESET
   s" : CW" SB-APPEND k NUM+ s"  ( n -- n ) " SB-APPEND
   k 0 > if s" CW" SB-APPEND k 1- NUM+ s"  " SB-APPEND then
   STEP-N 0 ?do S\" 1+\n" SB-APPEND loop
   S\" ;\n" SB-APPEND
   SUBJECT$ SB$ APPEND-FILE ;

: WRITE-SUBJECT ( -- )
   SUBJECT$ S\" variable SHARED\n" WRITE-ALL
   CHAIN-N 0 ?do i WORD! loop
   SB-RESET
   S\" : MAIN ( -- )\n   3 SHARED !\n   0 CW" SB-APPEND
   CHAIN-N 1- NUM+
   8 0 ?do s"  SHARED @ +" SB-APPEND loop
   S\"  262168 = if s\q reach=ok\q type cr else s\q reach: mismatch\q 74 die then ;\n" SB-APPEND
   SUBJECT$ SB$ APPEND-FILE ;

: BUILD ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ TIMEOUT-MS GE-RUN-ENV
   s" large pooled image build" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" large pooled image missing" GE-FAIL then ;

: CHECK ( -- )
   IMAGE$ AOT-IMAGE:CODE-RANGE nip 1048576 > 0= if
      s" pooled reach image does not exceed literal reach" GE-FAIL
   then
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" large pooled image run" GE-EXPECT-OK
   S\" reach=ok\n" s" large pooled image output" GE-EXPECT-OUT ;

: BODY ( -- )
   s" stripped-pool-reach" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   WRITE-SUBJECT BUILD CHECK
   s" PASS: stripped pointer pool beyond literal reach" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
