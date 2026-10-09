\ After the seal, DATA-BANDS row 9 (TXN-STATE-OFF): a byte store
\ just outside either end passes, and one into its first byte exits 83 with no
\ message (habu1.f:1801 BCSTORE).
require s00.inc
9 SG:FIRST 1- SG:VIA-C! 1 .
9 SG:END SG:VIA-C! 2 .
9 SG:FIRST SG:VIA-C! ." unguarded" cr
