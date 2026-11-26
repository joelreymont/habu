\ shadow-plain.f - shadow-bad.f with a plain call of a file's global word.
require test/fixtures/require-check/global-sibling.f
package RQC-SHADOW-PLAIN
public
: RQC-GLOBAL ( -- n ) 5 ;
;package
using RQC-SHADOW-PLAIN
RQC-GLOBAL drop
