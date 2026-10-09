\ After the seal a byte store into the last byte of DATA-BANDS
\ row 6 (UNIT-COMPILE-CELL) exits 83 with no message (habu1.f:295 GUARD-SPAN).
require s00.inc
6 SG:END 1- SG:VIA-C! ." unguarded" cr
