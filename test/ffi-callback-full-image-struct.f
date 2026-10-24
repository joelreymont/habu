\ Loaded on macOS by the save script in test/ffi-callback-full-image.f, after
\ SAVED-SUM24 is declared against test/ffi-struct-fixture.f's library. The
\ script checks before the save, so the image holds a resolved row, and
\ test/ffi-callback-full-image-struct-check.f checks again after restore, so
\ the restored row must reopen the library and resolve the symbol anew.
\ hb_sum24 receives its 24-byte record as the address of a caller-owned copy.
\ Its checksum weighs each member by position, and the record mixes widths and
\ signs, so a member read from the wrong offset or width changes the answer.

create SAVED-S24 24 allot

: SAVED-MIX ( n n -- n ) swap 31 * + ;

: SAVED-STRUCT-CHECK ( -- )
   $700000001 SAVED-S24 LE:U64!
   -5 SAVED-S24 8 + LE:U32!
   $80000003 SAVED-S24 12 + LE:U32!
   -$600000007 SAVED-S24 16 + LE:U64!
   SAVED-S24 BYTE-VIEW SAVED-SUM24
   1 $700000001 SAVED-MIX -5 SAVED-MIX $80000003 SAVED-MIX -$600000007 SAVED-MIX
   <> if 75 throw then ;

