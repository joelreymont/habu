\ After the seal, DATA-BANDS row 5 (TIER-PROV:OPEN-CELL): a byte
\ store just before it passes, and so does a realpath whose 8-byte destination
\ ends just before it; one whose destination's last byte is the row's first
\ exits 83 with no message before the path resolves (habu1.f:2590 REALPATH).
\ Row 6 begins where row 5 ends, so the byte after it is guarded too.
require s00.inc
5 SG:FIRST 1- SG:VIA-C! 1 .
5 SG:FIRST 8 - SG:VIA-REALPATH 2 .
5 SG:FIRST 7 - SG:VIA-REALPATH ." unguarded" cr
