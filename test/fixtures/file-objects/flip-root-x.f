\ flip-root-x.f - flip-a.f before flip.f, so flip.f's require of it cuts no part; then unclean.f.
\ MAIN is flip-root-plain.f's.

require test/fixtures/file-objects/flip-a.f
require test/fixtures/file-objects/flip.f
require test/fixtures/file-objects/unclean.f

: MAIN ( -- )
   FILE-OBJECTS-FLIP-A:F .  FILE-OBJECTS-FLIP:P .  FILE-OBJECTS-FLIP:Q .  FILE-OBJECTS-FLIP:R .
   FILE-OBJECTS-UNCLEAN:A .  FILE-OBJECTS-UNCLEAN:C . ;
