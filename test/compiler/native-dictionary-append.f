\ The publication primitive is internal, typed, and requires a native owner.
require lib/test.f
require lib/test/subject.f

package NDICT-APPEND-TEST

$1000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;

: SUBJECT-RC ( ptr u8 n n -- ) {: src:ptr size:n expected:n :}
   src size OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc expected <> if OUT outu LEN>N type ERR erru LEN>N type then
   rc expected T= ;

: BOUNDARY ( -- )
   s" ndict-append" 0 RECORD XREF-FLAGS DNAME-INT and 0<> TTRUE
   s" ndict-append" 0 search-wl 0 T=
   s" NDA-ESCAPE ( n -- ) ndict-append" CHECK-CANDIDATE! 0 T=
   s" ndict-append" 70 SUBJECT-RC
   s" ' ndict-append" 70 SUBJECT-RC ;

: REFUSALS ( -- )
   s" TRUSTED: NDA-CALL ( n -- ) ndict-append ; ndict@ NDA-CALL"
   ENGINE-ERROR:SEAL-VIOLATION SUBJECT-RC
   s" TRUSTED: NDA-CALL ( n -- ) ndict-append ; ndict@ 1+ NDA-CALL"
   ENGINE-ERROR:SEAL-VIOLATION SUBJECT-RC
   s" TRUSTED: NDA-CALL ( n -- ) ndict-append ; -1 NDA-CALL"
   ENGINE-ERROR:SEAL-VIOLATION SUBJECT-RC ;

: RUN ( -- )
   T-RESET
   s" append has only an internal trusted surface" T-LABEL BOUNDARY T-NEXT
   s" append refuses absent owner and stale or negative index" T-LABEL REFUSALS T-NEXT
   T-REPORT ;

RUN
;package
