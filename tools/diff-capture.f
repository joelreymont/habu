\ diff-capture.f - immutable-revision framed jj diff producer.

require tools/diff-capture-core.f
require tools/diff-report.f
require tools/argv.f

package DIFF-CAPTURE-CLI
private

PTR-VARIABLE REPORT-A
variable REPORT-U

: CAPTURE ( -- )
   s" tools/diff-capture.f artifact.hbdiff from-rev to-rev" ARGV-USAGE!
   ARGV-PARSE
   3 ARGV-EXPECT-POS-EXACT
   0 ARGV-POS$ 1 ARGV-POS$ 2 ARGV-POS$ DIFF-CAPTURE:RUN ;

: RENDER-REPORT ( -- )
   DIFF-CAPTURE:REPORT$ REPORT-U ! REPORT-A ! ;

: REPORT ( -- )
   [: RENDER-REPORT ;] catch {: code:n :}
   code 0<> if code DIFF-REPORT:RECORD-FAILURE drop exit then
   REPORT-A @ REPORT-U @ DIFF-REPORT:DELIVER drop ;

: RUN ( -- )
   [: CAPTURE ;] catch {: code:n :}
   code 0= if exit then
   code DIFF-REPORT:START
   DIFF-CAPTURE:REPORT? if REPORT then
   code throw ;

RUN

;package
