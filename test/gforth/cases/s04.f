\ After the seal a byte store into the last byte of DATA-BANDS
\ row 1 (PROT-REG-OFF) exits 83 with no message (habu1.f:295 GUARD-SPAN).
require s00.inc
1 SG:END 1- SG:VIA-C! ." unguarded" cr
