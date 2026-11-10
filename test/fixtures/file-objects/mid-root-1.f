\ mid-root-1.f - mid-w1.f, then mid-four.f; MAIN prints A, B, C and D.

require test/fixtures/file-objects/mid-w1.f
require test/fixtures/file-objects/mid-four.f

: MAIN ( -- )
   FILE-OBJECTS-MID-FOUR:A .  FILE-OBJECTS-MID-FOUR:B .  FILE-OBJECTS-MID-FOUR:C .
   FILE-OBJECTS-MID-FOUR:D . ;
