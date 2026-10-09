\ After the seal, DATA-BANDS row 3 (NCOMP-DISPATCH:TIER-CELL): a
\ byte store just outside either end passes, and a patch32 whose last byte is
\ the row's first exits 83 with no message (habu1.f:2744 BPATCH32 guards its 4
\ bytes). A patch32 the guard passes is an instruction write, which native
\ makes by flipping the target's pages, so none lands in DATA here.
require s00.inc
3 SG:FIRST 1- SG:VIA-C! 1 .
3 SG:END SG:VIA-C! 2 .
3 SG:FIRST 3 - SG:VIA-PATCH32 ." unguarded" cr
