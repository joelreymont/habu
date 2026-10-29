\ A definition whose effect is inferred from a fresh-region producer mints a new
\ region per call, exactly as a declared producer does: two calls never share an
\ allocation, one call's shared outputs still do. Each refused program runs in
\ its own subject process (the first refusal ends it); the accepted program
\ runs in one more.

require src/lib/test.f
require src/lib/string.f
require src/lib/test/subject.f

package RIGID-REGION-INFERRED-TEST

$1000 constant PROG-CAP
create PROG PROG-CAP allot
variable PROG-U

: PRELUDE$ ( -- ptr u8 n )
   s" -1 JSON-DIAGS ! package RQ public defer RG-ONE ( -- fresh-region-a ) defer RG-PAIR ( -- fresh-region-a fresh-region-a ) defer RG-DIFF ( -- fresh-region-a fresh-region-b ) : SAME-ID ( x x -- ) 2drop ; : USE-INF RG-ONE ; : USE-PAIR RG-PAIR ; : USE-DIFF RG-DIFF ; : NEST USE-INF ; " ;

: PROGRAM ( ptr u8 n -- ptr u8 n )
   {: case:ptr caseu:n :}
   0 PROG-U !
   PRELUDE$ PROG PROG-CAP PROG-U BUF-APPEND
   case caseu PROG PROG-CAP PROG-U BUF-APPEND
   s"  ;package" PROG PROG-CAP PROG-U BUF-APPEND
   PROG PROG-U @ ;

: REFUSED ( ptr u8 n -- )
   PROGRAM 70 s\" \"code\":\"E-RIGID-REGION-MISMATCH\"" SUBJECT:REFUSED? TTRUE ;

public

: RUN ( -- )
   T-RESET
   s" two calls of an inferred producer are two regions" T-LABEL
   s" : T3 USE-INF USE-INF SAME-ID ;" REFUSED
   s" a declared producer's region differs from an inferred one's" T-LABEL
   s" : T4 RG-ONE USE-INF SAME-ID ;" REFUSED
   s" an inferred producer of an inferred producer mints per call" T-LABEL
   s" : T5 NEST NEST SAME-ID ;" REFUSED
   s" a nested and a direct inferred producer are two regions" T-LABEL
   s" : T6 NEST USE-INF SAME-ID ;" REFUSED
   s" an inferred producer keeps two distinct identities distinct" T-LABEL
   s" : TD USE-DIFF SAME-ID ;" REFUSED
   s" an inferred consumer of one region refuses a second region" T-LABEL
   s" : F RG-ONE SAME-ID ; : G RG-ONE F ;" REFUSED
   s" a declared producer wrapper mints per call" T-LABEL
   s" : USE-DECL ( -- fresh-region-a ) RG-ONE ; : T7 USE-DECL USE-DECL SAME-ID ;" REFUSED
   s" one call's shared identities stay shared" T-LABEL
   s" : TP USE-PAIR SAME-ID ;"
      PROGRAM 0 SUBJECT:EXITED? TTRUE
   T-REPORT ;

;package

RIGID-REGION-INFERRED-TEST:RUN
