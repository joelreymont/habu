\ does-twice.f - a definition with two `does>` whose head and clause call a word
\ only another file's require loaded.
require test/fixtures/require-check/sibling.f
: RQC-TWICE ( n -- ) RQC-WORD:SEVEN drop create ,
   does> ( ^n -- n ) @ RQC-WORD:SEVEN + does> ( ^n -- n ) @ ;
