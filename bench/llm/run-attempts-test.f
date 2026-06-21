\ run-attempts-test.f - focused tests for attempt runner helpers.

create RATT-ROOT FS-PATH-CAP allot
create RATT-PATH FS-PATH-CAP allot
create RATT-DIR FS-PATH-CAP allot

variable RATT-ROOT-U
variable RATT-PATH-U
variable RATT-DIR-U

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

: RATT-PATH! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-ROOT$ name nameu RATT-PATH JOIN-PATH RATT-PATH-U ! ;

: RATT-DIR! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-ROOT$ name nameu RATT-DIR JOIN-PATH RATT-DIR-U ! ;

: RATT-DIR-PATH! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-DIR$ name nameu RATT-PATH JOIN-PATH RATT-PATH-U ! ;

: RATT-WRITE-PATH ( -- )
   RATT-PATH$ s" : CAND ( -- ) ;" WRITE-ALL ;

: RATT-WRITE-ROOT ( ptr u8 n -- )
   RATT-PATH!
   RATT-WRITE-PATH ;

: RATT-WRITE-IN-DIR ( ptr u8 n -- )
   RATT-DIR-PATH!
   RATT-WRITE-PATH ;

: RATT-MAKE-DIR ( ptr u8 n -- )
   RATT-DIR!
   RATT-DIR$ MAKE-DIR ;

: RATT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-run-attempts" TMPDIR-MKDIR RATT-ROOT RATT-ROOT-U RATT-COPY!
   RATT-ROOT$ CLEANUP-TREE+ ;

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

: RATT-MAIN ( -- )
   T-RESET
   RATT-PREPARE
   RATT-EXPECT-SINGLE
   RATT-EXPECT-MULTI-ROUND
   RATT-EXPECT-DIR-PRECEDENCE
   ['] RATT-EXPECT-MISSING E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-EMPTY-DIR E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-BAD-ROUND E-RA-CAPACITY TTHROWS
   CLEANUP-RUN
   RATT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" run-attempts-test: ok" type cr ;

RATT-MAIN
