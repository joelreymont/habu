\ xref-test.f - focused tests for live dictionary xref words.

require lib/test.f

: XRT-SAMPLE ( -- n )
   42 ;

: HB:XRT-SAMPLE ( -- n )
   43 ;

: XRT-REDEF ( -- n )
   1 ;

undefine XRT-REDEF

: XRT-REDEF ( -- n )
   2 ;

package XRT
: XRT-PRIVATE ( -- n )
   45 ;
public
: XRT-PUBLIC ( -- n )
   XRT-PRIVATE 1 + ;
;package

package XRT:LEFT
public
: VALUE ( -- n )
   51 ;
;package

package XRT:RIGHT
public
: VALUE ( n -- n )
   1 + ;
;package

package XRT-TYPE:A
public
ENUM state ready ;ENUM
;package

: XRT-CALLER ( n -- n )
   XRT-SAMPLE + ;

: XRT-EXPECT-FOUND ( ptr u8 n -- ptr a )
   XREF-FIND dup XREF-FOUND? TTRUE ;

: XRT-FOUND ( -- )
   s" xrt-sample" XRT-EXPECT-FOUND {: rec :}
   rec XREF-NAME$ s" XRT-SAMPLE" T$=
   rec XREF-LEN 0 > TTRUE
   rec XREF-START 0 > TTRUE
   rec XREF-NAME-LEN 10 T=
   rec XREF-WORDLIST get-current T=
   s" HB:XRT-SAMPLE" XRT-EXPECT-FOUND {: qrec :}
   qrec XREF-NAME$ s" XRT-SAMPLE" T$=
   qrec XREF-WORDLIST get-current T<>
   s" HB:XRT-SAMPLE" 0 search-wl 0= TTRUE
   s" hB:xRt-SaMpLe" 0 search-wl 0= TTRUE
   s" XRT:XRT-PUBLIC" XRT-EXPECT-FOUND {: prec :} \ typed-local-lint: allow-bare-local
   prec XREF-NAME$ s" XRT-PUBLIC" T$=
   s" XRT:XRT-PRIVATE" XREF-FIND XREF-FOUND? TFALSE ;

: XRT-MISSING ( -- )
   s" XRT-NO-SUCH-WORD" XREF-FIND XREF-FOUND? TFALSE
   s" XRT:MISSING:VALUE" 0 XREF:RESOLVE
   XREF:NONE T=
   -1 T= ;

package XREF-TEST
public

: QUALIFIED ( -- )
   s" XRT:LEFT:VALUE" 0 XREF:RESOLVE
   XREF:FOUND T=
   {: left:n :}
   s" XRT:RIGHT:VALUE" 0 XREF:RESOLVE
   XREF:FOUND T=
   left T<>
   s" XRT:LEFT:VALUE" XRT-EXPECT-FOUND XREF-NAME$ s" VALUE" T$=
   s" XRT:RIGHT:VALUE" XRT-EXPECT-FOUND XREF-NAME$ s" VALUE" T$=
   s" XRT-TYPE:A:STATE:READY" 0 XREF:RESOLVE
   XREF:FOUND T=
   drop
   s" XRT-TYPE:A:STATE:READY" XRT-EXPECT-FOUND XREF-NAME$ s" READY" T$=
   XRT:LEFT:VALUE 51 T=
   6 XRT:RIGHT:VALUE 7 T= ;

: MALFORMED ( -- )
   s" :XRT:VALUE" 0 XREF:RESOLVE
   XREF:MALFORMED T=
   -1 T=
   s" XRT:VALUE:" 0 XREF:RESOLVE
   XREF:MALFORMED T=
   -1 T=
   s" XRT::VALUE" 0 XREF:RESOLVE
   XREF:MALFORMED T=
   -1 T=
   [: s" XRT::VALUE" XREF-FIND drop ;] QNAME:E-SYNTAX TTHROWSQ ;

;package

: XRT-UNDEFINE ( -- )
   XRT-REDEF 2 T=
   s" XRT-REDEF" XREF-FIND dup XREF-FOUND? TTRUE XREF-WORDLIST XREF-RETIRED-WL T<> ;

: XRT-LATEST ( -- )
   LATEST XREF-NAME$ s" XRT-MAIN" T$= ;

: XRT-MAIN ( -- )
   T-RESET
   XRT-FOUND
   XRT-MISSING
   XREF-TEST:QUALIFIED
   XREF-TEST:MALFORMED
   XRT-UNDEFINE
   XRT-LATEST
   T-REPORT
   s" xref-test: ok" type cr ;

XRT-MAIN
