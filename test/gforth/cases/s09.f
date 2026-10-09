\ After the seal, DATA-BANDS row 4 (NCOMP-DISPATCH:DEF-TIER-CELL): a
\ byte store just outside either end passes, and so does a read whose buffer
\ ends just before it; a read whose buffer's last byte is the row's first exits
\ 83 with no message before the fd is used (habu1.f:1982 BREAD).
require s00.inc
4 SG:FIRST 1- SG:VIA-C! 1 .
4 SG:END SG:VIA-C! 2 .
4 SG:FIRST 8 - SG:VIA-READ 3 .
4 SG:FIRST 7 - SG:VIA-READ ." unguarded" cr
