\ diff-capture.f - immutable-revision framed jj diff producer.

require tools/diff-capture-core.f
require tools/argv.f

package DIFF-CAPTURE-CLI
private

: CAPTURE ( -- )
   s" tools/diff-capture.f artifact.hbdiff from-rev to-rev" ARGV-USAGE!
   ARGV-PARSE
   3 ARGV-EXPECT-POS-EXACT
   0 ARGV-POS$ 1 ARGV-POS$ 2 ARGV-POS$ DIFF-CAPTURE:RUN ;

: REPORT ( -- )
   DIFF-CAPTURE:REPORT$ {: a:ptr u:n :}
   2 a u write u <> if E-FS-IO throw then
   2 s" \n" write 1 <> if E-FS-IO throw then ;

: RUN ( -- )
   [: CAPTURE ;] catch {: code:n :}
   code 0= if exit then
   DIFF-CAPTURE:REPORT? if REPORT then
   code throw ;

RUN

;package
