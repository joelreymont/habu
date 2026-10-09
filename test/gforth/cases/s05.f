\ After the seal, DATA-BANDS row 2 (ENGINE-HOOK-OFF): a byte store
\ just outside either end passes, and so does an xt! ending just before it; an
\ xt! whose last byte is the row's first exits 83 with no message (habu2.f:8002
\ BXTSTORE).
require s00.inc
2 SG:FIRST 1- SG:VIA-C! 1 .
2 SG:END SG:VIA-C! 2 .
2 SG:FIRST 8 - SG:VIA-XT! 3 .
2 SG:FIRST 7 - SG:VIA-XT! ." unguarded" cr
