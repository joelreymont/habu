\ A truncated answer in a query row must refuse before reading past its bytes.
\ bin/hb --load test/native-file-inputs-format.f

require src/lib/test.f
require src/lib/le.f
require tools/native-file-inputs.f

package NFILE-INPUTS-FORMAT-TEST

create ROW 72 allot

public
: MAIN ( -- )
   T-RESET
   s" FILE input query truncation" T-LABEL
   72 0 ?do 0 ROW i + c! loop
   2 ROW LE:U64!       \ query row
   3 ROW 8 + LE:U64!   \ path length
   3 ROW 16 + LE:U64!  \ answer length
   s" abc" drop ROW 64 + 3 BYTE-COPY
   s" xy" drop ROW 67 + 2 BYTE-COPY
   [: ROW 69 NFILE-INPUTS:FORMAT ;] E-NUNIT-FILE TTHROWSQ
   T-REPORT ;

;package

NFILE-INPUTS-FORMAT-TEST:MAIN
