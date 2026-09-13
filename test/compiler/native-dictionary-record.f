\ The compiler record boundary shares the engine's wordlist lookup.
require lib/test.f

package NDICT-RECORD-FIXTURE

7 constant HIDDEN

public

42 constant ANSWER
: LONG-DICTIONARY-RECORD-SPELLING ( -- n ) 43 ;

;package

package NDICT-RECORD-TEST

\ Test-only access to the internal primitive; no record leaves this package.
\ Retirement: habu-attr-and-remove-2b13e978.
TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;


: SAME-RECORD ( ptr u8 n n -- )
   {: name:ptr size:n wid:n :}
   name size wid RECORD
   name size wid XREF-FIND-WL = TTRUE ;


: PUBLIC-WID ( -- n )
   s" NDICT-RECORD-FIXTURE" XREF-NAMESPACE-WL RECORD XREF-START ;


: PRIVATE-WID ( -- n )
   s" NDICT-RECORD-FIXTURE" XREF-NAMESPACE-WL RECORD XREF-LEN ;


: LOOKUPS ( -- )
   s" indexed record agrees with the live dictionary" T-LABEL
   s" dup" 0 SAME-RECORD
   s" DuP" 0 SAME-RECORD
   s" NDICT-RECORD-FIXTURE" XREF-NAMESPACE-WL SAME-RECORD
   s" answer" PUBLIC-WID SAME-RECORD
   s" LONG-DICTIONARY-RECORD-SPELLING" PUBLIC-WID SAME-RECORD
   s" HIDDEN" PRIVATE-WID SAME-RECORD
   s" HIDDEN" PUBLIC-WID RECORD XREF-FOUND? TFALSE
   s" NDICT-RECORD-ABSENT" 0 RECORD XREF-FOUND? TFALSE
   s" ANSWER" PUBLIC-WID RECORD XREF-START
   s" ANSWER" PUBLIC-WID search-wl T= ;


: INTERPRET-RAW ( -- ) s" xref-search-wl" INCLUDE-EVALUATE ;
: TICK-RAW ( -- ) s" ' xref-search-wl drop" INCLUDE-EVALUATE ;


: PROTECTED-BOUNDARY ( -- )
   s" the raw compiler record has no checked or interpreted public surface" T-LABEL
   s" xref-search-wl" 0 RECORD XREF-FLAGS DNAME-INT and 0<> TTRUE
   s" xref-search-wl" 0 search-wl 0 T=
   s" NDREC-ESCAPE ( ptr u8 n n -- ptr n ) xref-search-wl"
   CHECK-CANDIDATE! 0 T=
   ['] INTERPRET-RAW 70 TTHROWS
   ['] TICK-RAW 70 TTHROWS ;


: RUN ( -- )
   T-RESET
   LOOKUPS T-NEXT
   PROTECTED-BOUNDARY T-NEXT
   T-REPORT ;

RUN
;package
