\ After the seal, DATA-BANDS row 0 (FRIEND-ARENA): a byte store
\ just outside either end passes, and so does a cell store ending just before
\ it; a cell store whose last byte is the row's first exits 83 with no message
\ (habu1.f:295 GUARD-SPAN).
require s00.inc
0 SG:FIRST 1- SG:VIA-C! 1 .
0 SG:END SG:VIA-C! 2 .
0 SG:FIRST 8 - SG:VIA-! 3 .
0 SG:FIRST 7 - SG:VIA-! ." unguarded" cr
