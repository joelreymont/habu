\ parts-root-1.f - MAIN prints mid-four.f's A to D, flip-a.f's F, flip.f's P to R, unclean.f's A and C,
\ redo.f's X, cut-stack.f's S, cut-stack-b.f's T and root-set.f's V; cut-stack-a.f loads before cut-stack.f.

require test/fixtures/file-objects/mid-w1.f
require test/fixtures/file-objects/mid-four.f
require test/fixtures/file-objects/flip.f
require test/fixtures/file-objects/unclean.f
require test/fixtures/file-objects/redo.f
require test/fixtures/file-objects/root-set.f
require test/fixtures/file-objects/cut-stack-a.f
require test/fixtures/file-objects/cut-stack.f

: MAIN ( -- )
   FILE-OBJECTS-MID-FOUR:A .  FILE-OBJECTS-MID-FOUR:B .  FILE-OBJECTS-MID-FOUR:C .
   FILE-OBJECTS-MID-FOUR:D .
   FILE-OBJECTS-FLIP-A:F .  FILE-OBJECTS-FLIP:P .  FILE-OBJECTS-FLIP:Q .  FILE-OBJECTS-FLIP:R .
   FILE-OBJECTS-UNCLEAN:A .  FILE-OBJECTS-UNCLEAN:C .
   FILE-OBJECTS-REDO:X .  FILE-OBJECTS-CUT-STACK:S .  FILE-OBJECTS-CUT-STACK-B:T .
   FILE-OBJECTS-ROOT-SET:V . ;
