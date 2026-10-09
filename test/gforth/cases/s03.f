\ After the seal, DATA-BANDS row 1 (PROT-REG-OFF): a byte store just
\ outside either end passes, and so does a +! ending just before it; a +! whose
\ last byte is the row's first exits 83 with no message (habu1.f:1795
\ BPLUSSTORE).
require s00.inc
1 SG:FIRST 1- SG:VIA-C! 1 .
1 SG:END SG:VIA-C! 2 .
1 SG:FIRST 8 - SG:VIA-+! 3 .
1 SG:FIRST 7 - SG:VIA-+! ." unguarded" cr
