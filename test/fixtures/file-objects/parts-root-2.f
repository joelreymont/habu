\ parts-root-2.f - parts-root-1.f's MAIN and files, with mid-w2.f for mid-w1.f, flip-a.f before flip.f
\ and no cut-stack-a.f: flip.f's require of flip-a.f cuts no part, cut-stack.f's of cut-stack-a.f does.

require test/fixtures/file-objects/mid-w2.f
require test/fixtures/file-objects/mid-four.f
require test/fixtures/file-objects/flip-a.f
require test/fixtures/file-objects/flip.f
require test/fixtures/file-objects/unclean.f
require test/fixtures/file-objects/redo.f
require test/fixtures/file-objects/cut-stack.f

: MAIN ( -- )
   FILE-OBJECTS-MID-FOUR:A .  FILE-OBJECTS-MID-FOUR:B .  FILE-OBJECTS-MID-FOUR:C .
   FILE-OBJECTS-MID-FOUR:D .
   FILE-OBJECTS-FLIP-A:F .  FILE-OBJECTS-FLIP:P .  FILE-OBJECTS-FLIP:Q .  FILE-OBJECTS-FLIP:R .
   FILE-OBJECTS-UNCLEAN:A .  FILE-OBJECTS-UNCLEAN:C .
   FILE-OBJECTS-REDO:X .  FILE-OBJECTS-CUT-STACK:S .  FILE-OBJECTS-CUT-STACK-B:T . ;
