\ hb-build-size.f - image reports loaded only for explicit report options.

require tools/hb-build-report.f
require tools/hb-build-lib.f
require tools/image-size-lib.f

package HB-BUILD-CLI

\ Where the bytes of what this build just wrote went. The walk is the same one
\ tools/engine-size.f runs, so the summary line and the tool cannot disagree,
\ and it refuses to answer at all unless the classes sum to the file's length --
\ which is what makes one line of six numbers evidence rather than a guess.
\ --size-report prints the whole table under it; --report-json puts the same
\ numbers in the report object instead, so the JSON stays one parseable object.
: HBB-MEASURE-OUT ( -- )
   HBB-OUT$ IMAGE-SIZE:MEASURE ;

: HBB-SIZE-LINE ( -- )
   s" hb-build " type
   HBB-OUT$ IMAGE-SIZE:.SUMMARY ;

: HBB-CAPTURE-REPORT ( -- )
   HBB-MAKER-HIT @ 0 <>
   HBB-MAKER-BUILD @ 0 <>
   HBB-MAKER-RUN @ 0 <>
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
