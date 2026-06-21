\ run-attempts-test.f - focused tests for attempt runner helpers.

create RATT-ROOT FS-PATH-CAP allot
create RATT-PATH FS-PATH-CAP allot
create RATT-DIR FS-PATH-CAP allot
create RATT-REF FS-PATH-CAP allot
create RATT-CAND FS-PATH-CAP allot
create RATT-TESTS FS-PATH-CAP allot
create RATT-EXP RA-SRC-CAP allot

variable RATT-ROOT-U
variable RATT-PATH-U
variable RATT-DIR-U
variable RATT-REF-U
variable RATT-CAND-U
variable RATT-TESTS-U
variable RATT-EXP-U

: RATT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-RA-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: RATT-ROOT$ ( -- ptr u8 n )
   RATT-ROOT RATT-ROOT-U @ ;

: RATT-PATH$ ( -- ptr u8 n )
   RATT-PATH RATT-PATH-U @ ;

: RATT-DIR$ ( -- ptr u8 n )
   RATT-DIR RATT-DIR-U @ ;

: RATT-REF$ ( -- ptr u8 n )
   RATT-REF RATT-REF-U @ ;

: RATT-CAND$ ( -- ptr u8 n )
   RATT-CAND RATT-CAND-U @ ;

: RATT-TESTS$ ( -- ptr u8 n )
   RATT-TESTS RATT-TESTS-U @ ;

: RATT-EXP$ ( -- ptr u8 n )
   RATT-EXP RATT-EXP-U @ ;

: RATT-PATH! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-ROOT$ name nameu RATT-PATH JOIN-PATH RATT-PATH-U ! ;

: RATT-DIR! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-ROOT$ name nameu RATT-DIR JOIN-PATH RATT-DIR-U ! ;

: RATT-DIR-PATH! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-DIR$ name nameu RATT-PATH JOIN-PATH RATT-PATH-U ! ;

: RATT-REF-FILE! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-REF$ name nameu RATT-PATH JOIN-PATH RATT-PATH-U ! ;

: RATT-WRITE-PATH ( -- )
   RATT-PATH$ s" : CAND ( -- ) ;" WRITE-ALL ;

: RATT-WRITE-ROOT ( ptr u8 n -- )
   RATT-PATH!
   RATT-WRITE-PATH ;

: RATT-WRITE-IN-DIR ( ptr u8 n -- )
   RATT-DIR-PATH!
   RATT-WRITE-PATH ;

: RATT-WRITE-REF ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu text:ptr textu :}
   name nameu RATT-REF-FILE!
   RATT-PATH$ text textu WRITE-ALL ;

: RATT-WRITE-CAND ( ptr u8 n -- ) {: text:ptr textu :}
   RATT-CAND$ text textu WRITE-ALL ;

: RATT-WRITE-TESTS ( ptr u8 n -- ) {: text:ptr textu :}
   RATT-TESTS$ text textu WRITE-ALL ;

: RATT-MAKE-DIR ( ptr u8 n -- )
   RATT-DIR!
   RATT-DIR$ MAKE-DIR ;

: RATT-EXP-ROOM ( n -- ) {: add :}
   add 0 < if E-RA-CAPACITY throw then
   add RA-SRC-CAP RATT-EXP-U @ - > if E-RA-CAPACITY throw then ;

: RATT-EXP+ ( ptr u8 n -- ) {: a:ptr u :}
   u RATT-EXP-ROOM
   a RATT-EXP RATT-EXP-U @ + u BYTE-COPY
   RATT-EXP-U @ u + RATT-EXP-U ! ;

: RATT-EXP-C ( n -- ) {: c :}
   1 RATT-EXP-ROOM
   c RATT-EXP RATT-EXP-U @ + c!
   RATT-EXP-U @ 1+ RATT-EXP-U ! ;

: RATT-EXP-LN ( ptr u8 n -- )
   RATT-EXP+
   STR-LF RATT-EXP-C ;

: RATT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-run-attempts" TMPDIR-MKDIR RATT-ROOT RATT-ROOT-U RATT-COPY!
   RATT-ROOT$ CLEANUP-TREE+ ;

: RATT-BUNDLE-PATHS! ( -- )
   RATT-ROOT$ s" ref" RATT-REF JOIN-PATH RATT-REF-U !
   RATT-ROOT$ s" cand.f" RATT-CAND JOIN-PATH RATT-CAND-U !
   RATT-ROOT$ s" tests.f" RATT-TESTS JOIN-PATH RATT-TESTS-U ! ;

: RATT-BUNDLE-TASKS$ ( -- ptr u8 n )
   s" 1	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-
2	TWO	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-
" ;

: RATT-ONE-SRC$ ( -- ptr u8 n )
   s" : ONE ( -- i64 ) 1 ;" ;

: RATT-TWO-SRC$ ( -- ptr u8 n )
   s" : TWO ( -- i64 ) 2 ;" ;

: RATT-CAND-SRC$ ( -- ptr u8 n )
   s" : TWO ( -- i64 ) 22 ;" ;

: RATT-TESTS-SRC$ ( -- ptr u8 n )
   s" ONE drop TWO drop 111 emit 107 emit" ;

: RATT-PREPARE-BUNDLE-FIXTURE ( -- )
   RATT-BUNDLE-PATHS!
   RATT-REF$ MAKE-DIR
   s" 1.f" RATT-ONE-SRC$ RATT-WRITE-REF
   s" 2.f" RATT-TWO-SRC$ RATT-WRITE-REF
   RATT-CAND-SRC$ RATT-WRITE-CAND
   RATT-TESTS-SRC$ RATT-WRITE-TESTS ;

: RATT-EXPECTED-BUNDLE! ( -- )
   0 RATT-EXP-U !
   RATT-ONE-SRC$ RATT-EXP-LN
   RATT-CAND-SRC$ RATT-EXP-LN
   RATT-TESTS-SRC$ RATT-EXP+ ;

: RATT-EXPECT-ROUND ( n ptr u8 n -- ) {: idx name:ptr nameu :}
   name nameu RATT-DIR-PATH!
   idx RA-ROUND$ RATT-PATH$ T$= ;

: RATT-EXPECT-SINGLE ( -- )
   s" 7.f" RATT-WRITE-ROOT
   RATT-ROOT$ s" 7" RA-CANDIDATES 1 T=
   s" 7.f" RATT-PATH!
   0 RA-ROUND$ RATT-PATH$ T$= ;

: RATT-EXPECT-MULTI-ROUND ( -- )
   s" 8" RATT-MAKE-DIR
   s" 10.f" RATT-WRITE-IN-DIR
   s" 1.f" RATT-WRITE-IN-DIR
   s" 2.f" RATT-WRITE-IN-DIR
   RATT-ROOT$ s" 8" RA-CANDIDATES 3 T=
   0 s" 1.f" RATT-EXPECT-ROUND
   1 s" 2.f" RATT-EXPECT-ROUND
   2 s" 10.f" RATT-EXPECT-ROUND ;

: RATT-EXPECT-DIR-PRECEDENCE ( -- )
   s" 11.f" RATT-WRITE-ROOT
   s" 11" RATT-MAKE-DIR
   s" 1.f" RATT-WRITE-IN-DIR
   RATT-ROOT$ s" 11" RA-CANDIDATES 1 T=
   0 s" 1.f" RATT-EXPECT-ROUND ;

: RATT-EXPECT-MISSING ( -- )
   RATT-ROOT$ s" missing" RA-CANDIDATES drop ;

: RATT-EXPECT-EMPTY-DIR ( -- )
   s" 9" RATT-MAKE-DIR
   RATT-ROOT$ s" 9" RA-CANDIDATES drop ;

: RATT-EXPECT-BAD-ROUND ( -- )
   RA-ROUND-MAX RA-ROUND$ 2drop ;

: RATT-EXPECT-BUNDLE ( -- )
   RATT-PREPARE-BUNDLE-FIXTURE
   RATT-EXPECTED-BUNDLE!
   RATT-BUNDLE-TASKS$ RATT-REF$ s" 2" RATT-CAND$ RATT-TESTS$ RA-BUILD-BUNDLE
   RATT-EXP$ T$= ;

: RATT-EXPECT-MISSING-CANDIDATE ( -- )
   s" missing-cand.f" RATT-PATH!
   RATT-BUNDLE-TASKS$ RATT-REF$ s" 2" RATT-PATH$ RATT-TESTS$ RA-BUILD-BUNDLE 2drop ;

: RATT-EXPECT-MISSING-REF ( -- )
   s" no-ref" RATT-DIR!
   RATT-BUNDLE-TASKS$ RATT-DIR$ s" 1" RATT-CAND$ RATT-TESTS$ RA-BUILD-BUNDLE 2drop ;

: RATT-EXPECT-MISSING-TARGET ( -- )
   RATT-BUNDLE-TASKS$ RATT-REF$ s" 99" RATT-CAND$ RATT-TESTS$ RA-BUILD-BUNDLE 2drop ;

: RATT-EXPECT-BUNDLE-CAPACITY ( -- )
   RA-BUNDLE-CAP 1+ RA-BUNDLE-ROOM ;

: RATT-MAIN ( -- )
   T-RESET
   RATT-PREPARE
   RATT-EXPECT-SINGLE
   RATT-EXPECT-MULTI-ROUND
   RATT-EXPECT-DIR-PRECEDENCE
   RATT-EXPECT-BUNDLE
   ['] RATT-EXPECT-MISSING E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-EMPTY-DIR E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-BAD-ROUND E-RA-CAPACITY TTHROWS
   ['] RATT-EXPECT-MISSING-CANDIDATE E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-MISSING-REF E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-MISSING-TARGET E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-BUNDLE-CAPACITY E-RA-CAPACITY TTHROWS
   CLEANUP-RUN
   RATT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" run-attempts-test: ok" type cr ;

RATT-MAIN
