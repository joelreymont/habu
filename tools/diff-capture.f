\ diff-capture.f - immutable-revision framed jj diff producer.

require tools/diff-capture-core.f
require tools/argv.f

package DIFF-CAPTURE-CLI
private

: RUN ( -- )
   s" tools/diff-capture.f artifact.hbdiff from-rev to-rev" ARGV-USAGE!
   ARGV-PARSE
   3 ARGV-EXPECT-POS-EXACT
   0 ARGV-POS$ 1 ARGV-POS$ 2 ARGV-POS$ DIFF-CAPTURE:RUN ;

RUN

;package
