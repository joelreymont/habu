\ a quotation passed to a word and executed
: APPLY ( n [ n -- n ] -- n ) execute ;
: C04 ( -- )
   21 [: 2 * ;] APPLY .
   5 [: dup * 1 + ;] APPLY . ;
C04
