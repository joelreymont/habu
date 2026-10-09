\ After the seal, DATA-BANDS row 8 (BODYBUF-OFF): a byte store just
\ outside either end passes, and so does a munmap whose span ends just before
\ it (its address is not page-aligned, so nothing is unmapped); a munmap whose
\ span's last byte is the row's first exits 83 with no message (habu1.f:2013
\ BMUNMAP).
require s00.inc
8 SG:FIRST 1- SG:VIA-C! 1 .
8 SG:END SG:VIA-C! 2 .
8 SG:FIRST 8 - SG:VIA-MUNMAP 3 .
8 SG:FIRST 7 - SG:VIA-MUNMAP ." unguarded" cr
