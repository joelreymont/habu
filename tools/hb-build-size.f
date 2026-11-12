\ hb-build-size.f - image reports loaded only for explicit report options.

require tools/hb-build-report.f
require tools/hb-build-lib.f
require tools/image-size-lib.f

package HB-BUILD-CLI

\ Where this build's bytes went (tools/image-size-lib.f): --size-report prints
\ the whole table; --report-json puts the numbers in the report object instead.
: HBB-MEASURE-OUT ( -- )
   HBB-OUT$ IMAGE-SIZE:MEASURE ;

: HBB-SIZE-LINE ( -- )
   s" hb-build " type
   HBB-OUT$ IMAGE-SIZE:.SUMMARY ;

: HBB-CAPTURE-REPORT ( -- )
   HBB-ELAPSED-NS @
   HB-BUILD:CAPTURE
   IMAGE-SIZE:TOTAL-BYTES IMAGE-SIZE:CODE-BYTES IMAGE-SIZE:NAME-BYTES
   IMAGE-SIZE:DATA-WRITTEN IMAGE-SIZE:DATA-ZERO IMAGE-SIZE:PAD-BYTES
   IMAGE-SIZE:OTHER-BYTES
   HB-BUILD:CAPTURE-SIZE ;

: HBB-REPORT ( -- )
   HBB-MEASURE-OUT
   HBB-CAPTURE-REPORT
   HBB-REPORT-JSON @ if HB-BUILD:REPORT$ type cr exit then
   HBB-SUCCESS
   HBB-SIZE-REPORT @ if IMAGE-SIZE:REPORT HBB-SIZE-LINE then ;

: HBB-INSTALL-REPORT ( -- )
   [: HBB-REPORT ;] is HBB-REPORT-HOOK ;

HBB-INSTALL-REPORT

;package
