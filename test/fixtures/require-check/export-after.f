\ export-after.f - export-bad.f with a body before the EXPORT that calls a
\ boot prefix word.
require test/fixtures/require-check/sibling.f
package RQC-EXPORT-AFTER
: SAME? ( -- bool ) s" ab" s" ab" STR:= ;
public
EXPORT RQC-WORD:SEVEN
;package
