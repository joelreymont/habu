\ forth-bundle-test.f - focused tests for Forth bundle building.

4096 constant FBT-CAP
1024 constant FBT-SCRATCH-CAP
8 constant FBT-SMALL-CAP

create FBT-ROOT FS-PATH-CAP allot
create FBT-REF FS-PATH-CAP allot
create FBT-CAND FS-PATH-CAP allot
create FBT-TESTS FS-PATH-CAP allot
create FBT-PATH FS-PATH-CAP allot
create FBT-OUT FBT-CAP allot
create FBT-SCRATCH FBT-SCRATCH-CAP allot

variable FBT-ROOT-U
variable FBT-REF-U
variable FBT-CAND-U
variable FBT-TESTS-U
variable FBT-PATH-U
variable FBT-OUT-U

: FBT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: FBT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: FBT-ROOT$ ( -- ptr u8 n )
   FBT-ROOT FBT-ROOT-U @ ;

: FBT-REF$ ( -- ptr u8 n )
   FBT-REF FBT-REF-U @ ;

: FBT-CAND$ ( -- ptr u8 n )
   FBT-CAND FBT-CAND-U @ ;

: FBT-TESTS$ ( -- ptr u8 n )
   FBT-TESTS FBT-TESTS-U @ ;

: FBT-PATH$ ( -- ptr u8 n )
   FBT-PATH FBT-PATH-U @ ;

: FBT-OUT$ ( -- ptr u8 n )
   FBT-OUT FBT-OUT-U @ ;

: FBT-TASKS$ ( -- ptr u8 n )
   s" 1	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-
2	TWO	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-
" ;

: FBT-DUP-TASKS$ ( -- ptr u8 n )
   s" 1	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-
1	ONE-AGAIN	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-
" ;

: FBT-BAD-TASKS$ ( -- ptr u8 n )
   s" 1	ONE
" ;

: FBT-ONE-SRC$ ( -- ptr u8 n )
   s" : ONE ( -- i64 ) 1 ;" ;

: FBT-TWO-SRC$ ( -- ptr u8 n )
   s" : TWO ( -- i64 ) 2 ;" ;

: FBT-EXTRA-SRC$ ( -- ptr u8 n )
   s" : EXTRA ( -- i64 ) 3 ;" ;

: FBT-CAND-SRC$ ( -- ptr u8 n )
   s" : TWO ( -- i64 ) 22 ;" ;

: FBT-TESTS-SRC$ ( -- ptr u8 n )
   s" ONE drop TWO drop 111 emit 107 emit" ;

: FBT-EXPECTED$ ( -- ptr u8 n )
   s" : ONE ( -- i64 ) 1 ;
: TWO ( -- i64 ) 22 ;
ONE drop TWO drop 111 emit 107 emit" ;

: FBT-PREPARE-PATHS ( -- )
   CLEANUP-RESET
   s" habu-forth-bundle" TMPDIR-MKDIR FBT-ROOT FBT-ROOT-U FBT-COPY!
   FBT-ROOT$ CLEANUP-TREE+
   FBT-ROOT$ s" ref" FBT-REF FBT-REF-U FBT-PATH!
   FBT-ROOT$ s" cand.f" FBT-CAND FBT-CAND-U FBT-PATH!
   FBT-ROOT$ s" tests.f" FBT-TESTS FBT-TESTS-U FBT-PATH!
   FBT-REF$ MAKE-DIRS ;

: FBT-REF-PATH! ( ptr u8 n -- )
   FBT-REF$ 2swap FBT-PATH FBT-PATH-U FBT-PATH! ;

: FBT-WRITE-REF ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu body:ptr bodyu :}
   name nameu FBT-REF-PATH!
   FBT-PATH$ body bodyu WRITE-ALL ;

: FBT-PREPARE ( -- )
   FBT-PREPARE-PATHS
   s" 1.f" FBT-ONE-SRC$ FBT-WRITE-REF
   s" 2.f" FBT-TWO-SRC$ FBT-WRITE-REF
   s" 3.f" FBT-EXTRA-SRC$ FBT-WRITE-REF
   FBT-CAND$ FBT-CAND-SRC$ WRITE-ALL
   FBT-TESTS$ FBT-TESTS-SRC$ WRITE-ALL ;

: FBT-BUILD ( ptr u8 n ptr u8 n -- )
   {: tasks:ptr tasksu target:ptr targetu :}
   tasks tasksu FBT-REF$ target targetu FBT-CAND$ FBT-TESTS$
   FBT-OUT FBT-CAP FBT-SCRATCH FBT-SCRATCH-CAP FB-BUILD-BUNDLE-INTO FBT-OUT-U ! ;

: FBT-BUILD-SMALL ( -- )
   FBT-TASKS$ FBT-REF$ s" 2" FBT-CAND$ FBT-TESTS$
   FBT-OUT FBT-SMALL-CAP FBT-SCRATCH FBT-SCRATCH-CAP FB-BUILD-BUNDLE-INTO drop ;

: FBT-LIMITS ( ptr u8 n ptr u8 n -- n n )
   {: tasks:ptr tasksu target:ptr targetu :}
   tasks tasksu FBT-REF$ target targetu FBT-CAND$ FBT-TESTS$ FB-BUNDLE-LIMITS ;

: FBT-EXPECT-BUNDLE ( -- )
   FBT-PREPARE
   FBT-TASKS$ s" 2" FBT-BUILD
   FBT-OUT$ FBT-EXPECTED$ T$=
   FBT-OUT$ FBT-EXTRA-SRC$ CONTAINS? TFALSE
   CLEANUP-RUN ;

: FBT-EXPECT-LIMITS ( -- )
   FBT-PREPARE
   FBT-TASKS$ s" 2" FBT-LIMITS
   FBT-TESTS-SRC$ nip T=
   FBT-EXPECTED$ nip T=
   CLEANUP-RUN ;

: FBT-EXPECT-MISSING-REF ( -- )
   FBT-PREPARE-PATHS
   s" 2.f" FBT-TWO-SRC$ FBT-WRITE-REF
   FBT-CAND$ FBT-CAND-SRC$ WRITE-ALL
   FBT-TESTS$ FBT-TESTS-SRC$ WRITE-ALL
   FBT-TASKS$ s" 2" FBT-BUILD ;

: FBT-EXPECT-MISSING-CAND ( -- )
   FBT-PREPARE-PATHS
   s" 1.f" FBT-ONE-SRC$ FBT-WRITE-REF
   FBT-TESTS$ FBT-TESTS-SRC$ WRITE-ALL
   FBT-TASKS$ s" 2" FBT-BUILD ;

: FBT-EXPECT-MISSING-TARGET ( -- )
   FBT-PREPARE
   FBT-TASKS$ s" 99" FBT-BUILD ;

: FBT-EXPECT-DUPLICATE ( -- )
   FBT-PREPARE
   FBT-DUP-TASKS$ s" 2" FBT-BUILD ;

: FBT-EXPECT-BAD-SCHEMA ( -- )
   FBT-PREPARE
   FBT-BAD-TASKS$ s" 1" FBT-BUILD ;

: FBT-EXPECT-CAPACITY ( -- )
   FBT-PREPARE
   FBT-BUILD-SMALL ;

: FBT-MAIN ( -- )
   T-RESET
   FBT-EXPECT-BUNDLE
   FBT-EXPECT-LIMITS
   [: FBT-EXPECT-MISSING-REF ;] E-FB-MISSING TTHROWSQ
   CLEANUP-RUN
   [: FBT-EXPECT-MISSING-CAND ;] E-FB-MISSING TTHROWSQ
   CLEANUP-RUN
   [: FBT-EXPECT-MISSING-TARGET ;] E-FB-MISSING TTHROWSQ
   CLEANUP-RUN
   [: FBT-EXPECT-DUPLICATE ;] E-FB-DUPLICATE TTHROWSQ
   CLEANUP-RUN
   [: FBT-EXPECT-BAD-SCHEMA ;] E-BM-SCHEMA TTHROWSQ
   CLEANUP-RUN
   [: FBT-EXPECT-CAPACITY ;] E-FB-CAPACITY TTHROWSQ
   CLEANUP-RUN
   T-REPORT
   s" forth-bundle-test: ok" type cr ;

FBT-MAIN
