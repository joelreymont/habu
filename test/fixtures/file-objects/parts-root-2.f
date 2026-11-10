\ parts-root-2.f - parts-root-1.f's files with mid-w2.f for mid-w1.f and flip-a.f before flip.f, so
\ flip.f's require of it cuts no part. MAIN is parts-root-1.f's.

require test/fixtures/file-objects/mid-w2.f
require test/fixtures/file-objects/mid-four.f
require test/fixtures/file-objects/flip-a.f
require test/fixtures/file-objects/flip.f
require test/fixtures/file-objects/unclean.f
require test/fixtures/file-objects/redo-main.f

: MAIN ( -- )
   FILE-OBJECTS-MID-FOUR:A .  FILE-OBJECTS-MID-FOUR:B .  FILE-OBJECTS-MID-FOUR:C .
   FILE-OBJECTS-MID-FOUR:D .
   FILE-OBJECTS-FLIP-A:F .  FILE-OBJECTS-FLIP:P .  FILE-OBJECTS-FLIP:Q .  FILE-OBJECTS-FLIP:R .
   FILE-OBJECTS-UNCLEAN:A .  FILE-OBJECTS-UNCLEAN:C .
   FILE-OBJECTS-REDO:X . ;
