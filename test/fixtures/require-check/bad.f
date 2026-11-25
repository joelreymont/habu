\ bad.f - calls word.f's word, which only sibling.f's require loaded.
require test/fixtures/require-check/sibling.f
package RQC-BAD
public
: SEVEN ( -- n ) RQC-WORD:SEVEN ;
;package
