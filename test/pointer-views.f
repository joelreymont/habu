\ Pointer representation views retain bytes and reject nominal fabrication.
require lib/test.f
package POINTER-VIEWS-TEST
create CELL 1 cells allot
: READ-BYTE ( ptr n -- u8 ) BYTE-VIEW c@ ;
: READ-CELL ( ptr u8 -- n ) CELL-VIEW @ ;
T-RESET
$1234 CELL !
CELL READ-BYTE $34 T=
CELL BYTE-VIEW READ-CELL $1234 T=
NEWTYPE identity 0
s" BAD-VIEW ( ptr u8 -- ptr identity ) CELL-VIEW" CHECK! 0 T=
T-REPORT
;package
