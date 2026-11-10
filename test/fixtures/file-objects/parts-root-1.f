\ parts-root-1.f - mid-w1.f, mid-four.f, flip.f, unclean.f and redo-main.f; MAIN prints mid-four.f's
\ A to D, flip-a.f's F, flip.f's P, Q and R, unclean.f's A and C, and redo-main.f's X.

require test/fixtures/file-objects/mid-w1.f
require test/fixtures/file-objects/mid-four.f
require test/fixtures/file-objects/flip.f
require test/fixtures/file-objects/unclean.f
require test/fixtures/file-objects/redo-main.f

: MAIN ( -- )
   FILE-OBJECTS-MID-FOUR:A .  FILE-OBJECTS-MID-FOUR:B .  FILE-OBJECTS-MID-FOUR:C .
   FILE-OBJECTS-MID-FOUR:D .
   FILE-OBJECTS-FLIP-A:F .  FILE-OBJECTS-FLIP:P .  FILE-OBJECTS-FLIP:Q .  FILE-OBJECTS-FLIP:R .
   FILE-OBJECTS-UNCLEAN:A .  FILE-OBJECTS-UNCLEAN:C .
   FILE-OBJECTS-REDO:X . ;
