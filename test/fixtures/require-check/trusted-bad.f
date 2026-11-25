\ trusted-bad.f - a trusted body calls word.f's word, which only sibling.f's
\ require loaded.
require test/fixtures/require-check/sibling.f
package RQC-TRUSTED-BAD
public
TRUSTED: USE ( -- n ) RQC-WORD:SEVEN ;
;package
