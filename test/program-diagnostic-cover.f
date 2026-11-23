\ A call whose inputs sit over other values: each record shows the word's own
\ inputs and the cells that fill them, never the values below.
CHECK:MULTI-ERR-BEGIN
TYPED-VARIABLE PDC-V str
: PDC-STORE ( n -- n ) s" abcde" str> PDC-V ! ;
defer PDC-LEN ( str -- n )
: PDC-CALL ( n -- n n ) s" abc" str> PDC-LEN ;
: PDC-ONE ( -- n ) 1 PDC-LEN ;
: PDC-TWO ( n n -- ) 2drop ;
: PDC-WIDE ( n -- ) PDC-V @ PDC-TWO ;
CHECK:MULTI-ERR-END . cr
s" ok" type cr
