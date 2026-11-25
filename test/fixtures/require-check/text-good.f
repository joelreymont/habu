\ text-good.f - evaluates text that requires word.f and calls its word.
require test/fixtures/require-check/sibling.f
s" require test/fixtures/require-check/word.f" evaluate
s" : RQC-TEXT-SEVEN ( -- n ) RQC-WORD:SEVEN ;" evaluate
RQC-TEXT-SEVEN drop
