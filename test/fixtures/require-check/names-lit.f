\ names-lit.f - names word.f's word in a literal before a word that reads it
\ as a name; word.f is only sibling.f's require, and the literal is no call.
require test/fixtures/require-check/sibling.f
package RQC-NAMES-LIT
: LOOK ( str -- ) drop ;
names: LOOK
public
: SEEK ( -- ) s" RQC-WORD:SEVEN" LOOK ;
;package
