\ ndict! to the seal-time watermark (SEAL-NDICT-CELL) passes and drops what the
\ program defined; one record below it exits 83 with no message (habu1.f:1463
\ BNDSET).
: MARK ( -- n ) data-base SEAL-NDICT-CELL + @ ;
MARK dup 1- swap ndict! 1 .
ndict! ." unguarded" cr
