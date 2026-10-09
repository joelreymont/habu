\ After the seal, DATA-BANDS row 7 (REPLAY-SCOPE:LATCH): a byte
\ store just after it passes, and one into its first byte exits 83 with no
\ message. Row 6 ends where row 7 begins, so the byte before it is guarded too.
require s00.inc
7 SG:END SG:VIA-C! 1 .
7 SG:FIRST SG:VIA-C! ." unguarded" cr
