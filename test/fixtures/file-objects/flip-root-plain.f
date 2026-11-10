\ flip-root-plain.f - flip.f, then unclean.f; MAIN prints flip-a.f's F, P, Q, R, A and C.

require test/fixtures/file-objects/flip.f
require test/fixtures/file-objects/unclean.f

: MAIN ( -- )
   FILE-OBJECTS-FLIP-A:F .  FILE-OBJECTS-FLIP:P .  FILE-OBJECTS-FLIP:Q .  FILE-OBJECTS-FLIP:R .
   FILE-OBJECTS-UNCLEAN:A .  FILE-OBJECTS-UNCLEAN:C . ;
