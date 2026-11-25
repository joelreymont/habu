\ built.f - definers whose bodies call word.f's word, or end in their caller's
\ text.
require test/fixtures/require-check/word.f
package RQC-BUILT
: HEAD ( str -- ) EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word ;
public
: MAKE ( str -- ) HEAD  postpone RQC-WORD:SEVEN  postpone ; ;
: TEXT-MAKE ( str -- ) HEAD  s" RQC-WORD:SEVEN" evaluate-closed  postpone ; ;
: OPEN ( str -- ) HEAD ;
;package
