\ A public native-unit import can append a second name in the same real
\ wordlist. XREF keeps the descending scan's newest-row answer after indexing.
\ Run with bin/hb --load test/native-unit-xref-newest.f.

require src/lib/test.f
require src/compiler/native/publish.f
require src/lib/executable-build.f

package NATIVE-UNIT-XREF-TEST
public
: DUPLICATE ( -- n ) 7 ;
;package

: XREF-NEWEST-CHECK ( -- )
   s" NATIVE-UNIT-XREF-TEST" XREF-NAMESPACE-WL XREF-FIND-WL
      XREF-PKG-PUBLIC {: wid:n :}
   wid XREF-WID-PROTECTED? TFALSE
   s" DUPLICATE" wid XREF-FIND-WL {: prior:ptr :}
   prior XREF-FOUND? TTRUE
   ndict@ {: next:n :}
   prior BYTE-VIEW 4 prior BYTE-VIEW 1
      NULL-PTR 0 NULL-PTR 0 NPUB:IMPORT-FILE-UNIT
   s" DUPLICATE" wid XREF-FIND-WL next XREF-REC = TTRUE
   s" DUPLICATE" wid XREF-FIND-WL-INDEX next = TTRUE ;

: XREF-NEWEST-RUN ( -- )
   T-RESET
   [: XREF-NEWEST-CHECK ;] EXECUTABLE-BUILD:WITH
   T-REPORT
   s" native-unit-xref-newest: ok" type cr ;

XREF-NEWEST-RUN
