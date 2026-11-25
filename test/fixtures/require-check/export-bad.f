\ export-bad.f - exports word.f's word, which only sibling.f's require loaded.
require test/fixtures/require-check/sibling.f
package RQC-EXPORT-BAD
public
EXPORT RQC-WORD:SEVEN
;package
