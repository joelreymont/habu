\ text-bad.f - calls word.f's word itself after such text, one that throws too.
require test/fixtures/require-check/sibling.f
s" require test/fixtures/require-check/word.f RQC-WORD:SEVEN drop" evaluate
: RQC-TEXT-THROWS ( -- ) s" 1 throw" evaluate-closed ;
' RQC-TEXT-THROWS catch drop
RQC-WORD:SEVEN drop
