\ After the seal a byte store into the first byte of DATA-BANDS
\ row 6 (UNIT-COMPILE-CELL) exits 83 with no message. Rows 5 and 7 border it,
\ so neither byte just outside it is unguarded.
require s00.inc
6 SG:FIRST SG:VIA-C! ." unguarded" cr
