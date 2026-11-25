\ is-bad.f - sets word.f's defer, which only sibling.f's require loaded.
require test/fixtures/require-check/sibling.f
package RQC-IS-BAD
: FIVE ( -- n ) 5 ;
public
: SET ( -- ) ['] FIVE is RQC-WORD:LATER ;
;package
