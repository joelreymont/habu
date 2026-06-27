\ xref-test.f - focused tests for live dictionary xref words.
\ Load after lib/test.f and src/habu/xref.f.

: XRT-SAMPLE ( -- n )
   42 ;

: HB:XRT-SAMPLE ( -- n )
   43 ;

: XRT-SUFFIX: ( -- n )
   44 ;

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
   s" xrt-suffix:" XRT-EXPECT-FOUND {: erec :}
   erec XREF-NAME$ s" XRT-SUFFIX:" T$=
   erec XREF-WORDLIST get-current T= ;

: XRT-MISSING ( -- )
   s" XRT-NO-SUCH-WORD" XREF-FIND XREF-FOUND? TFALSE ;

: XRT-LATEST ( -- )
   LATEST XREF-NAME$ s" XRT-MAIN" T$= ;

: XRT-MAIN ( -- )
   T-RESET
   XRT-FOUND
   XRT-MISSING
   XRT-LATEST
   T-REPORT
   s" xref-test: ok" type cr ;

XRT-MAIN
