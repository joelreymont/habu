\ good.f - bad.f with word.f required; it calls a prefix word and a word of
\ the file it includes.
require test/fixtures/require-check/sibling.f
require test/fixtures/require-check/word.f
include test/fixtures/require-check/part.f
package RQC-GOOD
public
: SUM ( -- n ) RQC-WORD:SEVEN RQC-PART:FOURTEEN + ;
: SAME? ( -- bool ) s" ab" s" ab" STR:= ;
;package
