require lib/test.f
require lib/byte-edit.f

package BYTE-EDIT-TEST
using EDIT
4 constant CAP
$100 constant OUTPUT-CAP
$12345678 constant CANARY
here CELL 1- and CELL swap - CELL 1- and allot
create BEFORE CANARY ,
create STORAGE CAP STORAGE-BYTES allot
create AFTER CANARY ,
create ZERO-STORAGE 0 STORAGE-BYTES allot
create OUTPUT OUTPUT-CAP allot
create BINARY $41 c, 0 c, $FF c, $42 c,
create BINARY-EXPECTED $41 c, $7F c, 0 c, $42 c,
create BINARY-INSERT $7F c, 0 c,

: OPEN ( ptr u8 n -- EDIT:editor )
   STORAGE CAP STORAGE-BYTES 2swap INIT ;

: RESULT= ( EDIT:editor ptr u8 n -- EDIT:editor )
   {: expected size:n :}
   OUTPUT OUTPUT-CAP EDIT:WRITE
   OUTPUT swap expected size T$= ;

: MIXED-EDITS ( -- )
   s" abcdefgh" OPEN
   1 >OFF 2 >LEN s" XYZZ" REPLACE
   5 >OFF 0 >LEN s" !" REPLACE
   7 >OFF 1 >LEN s" " REPLACE
   s" aXYZZde!fg" RESULT=
   s" aXYZZde!fg" RESULT=
   EDIT:CLOSE ;

: EXACT-PRESERVATION ( -- )
   s" <?xml?><a k='v'>old</a><!-- unchanged -->" OPEN
   16 >OFF 3 >LEN s" new&amp;more" REPLACE
   s" <?xml?><a k='v'>new&amp;more</a><!-- unchanged -->" RESULT=
   EDIT:CLOSE
   BINARY 4 OPEN
   1 >OFF 2 >LEN BINARY-INSERT 2 REPLACE
   BINARY-EXPECTED 4 RESULT= EDIT:CLOSE ;

: EMPTY-PLAN ( -- )
   ZERO-STORAGE 0 STORAGE-BYTES s" whole original" INIT
   s" whole original" RESULT= EDIT:CLOSE
   ZERO-STORAGE 0 STORAGE-BYTES s" " INIT
   s" " RESULT= EDIT:CLOSE ;

: ZERO-LENGTH-EDITS ( -- )
   s" ab" OPEN
   0 >OFF 0 >LEN s" <" REPLACE
   0 >OFF 0 >LEN s" !" REPLACE
   2 >OFF 0 >LEN s" >" REPLACE
   s" <!ab>" RESULT= EDIT:CLOSE
   s" ab" OPEN
   0 >OFF 2 >LEN s" " REPLACE
   s" " RESULT= EDIT:CLOSE ;

: BAD-RANGES ( -- )
   s" abc" OPEN
   [: -1 >OFF 1 >LEN s" x" REPLACE ;] catch E-RANGE T=
   [: 4 >OFF 0 >LEN s" x" REPLACE ;] catch E-RANGE T=
   [: 1 >OFF 3 >LEN s" x" REPLACE ;] catch E-RANGE T=
   [: 1 >OFF -1 >LEN s" x" REPLACE ;] catch E-RANGE T=
   [: 1 >OFF 1 >LEN s" x" drop -1 REPLACE ;] catch E-RANGE T=
   s" abc" RESULT= EDIT:CLOSE ;

: BAD-ORDER ( -- )
   s" abcdef" OPEN
   2 >OFF 2 >LEN s" X" REPLACE
   [: 3 >OFF 1 >LEN s" Y" REPLACE ;] catch E-ORDER T=
   [: 0 >OFF 1 >LEN s" Y" REPLACE ;] catch E-ORDER T=
   s" abXef" RESULT=
   4 >OFF 1 >LEN s" Z" REPLACE
   s" abXZf" RESULT= EDIT:CLOSE ;

: NO-PARTIAL-WRITE ( -- )
   $5A OUTPUT c!
   s" abc" OPEN
   1 >OFF 1 >LEN s" long replacement" REPLACE
   [: OUTPUT 1 EDIT:WRITE drop ;] catch E-CAPACITY T=
   OUTPUT c@ $5A T=
   s" along replacementc" RESULT= EDIT:CLOSE ;

: CAPACITY-FAILURE ( -- )
   ZERO-STORAGE 0 STORAGE-BYTES s" abc" INIT
   [: 0 >OFF 0 >LEN s" x" REPLACE ;] catch E-CAPACITY T=
   s" abc" RESULT= EDIT:CLOSE ;

: ALIAS-FAILURES ( -- )
   BINARY 4 OPEN
   [: BINARY 4 EDIT:WRITE drop ;] catch E-ALIAS T=
   [: STORAGE BYTE-VIEW OUTPUT-CAP EDIT:WRITE drop ;] catch E-ALIAS T=
   [: 0 >OFF 0 >LEN STORAGE BYTE-VIEW 1 REPLACE ;] catch E-ALIAS T=
   0 >OFF 0 >LEN OUTPUT 1 REPLACE
   [: OUTPUT OUTPUT-CAP EDIT:WRITE drop ;] catch E-ALIAS T=
   EDIT:CLOSE ;

: CHECKED-OWNERSHIP ( -- )
   s" BYTE-EDIT-DUP ( EDIT:editor -- EDIT:editor EDIT:editor ) dup" CHECK! 0 T=
   s" BYTE-EDIT-DROP ( EDIT:editor -- ) drop" CHECK! 0 T=
   s" BYTE-EDIT-SWAP ( EDIT:editor len off ptr u8 n -- EDIT:editor ) EDIT:REPLACE"
   CHECK! 0 T= ;

T-RESET
MIXED-EDITS
EXACT-PRESERVATION
EMPTY-PLAN
ZERO-LENGTH-EDITS
BAD-RANGES
BAD-ORDER
NO-PARTIAL-WRITE
CAPACITY-FAILURE
ALIAS-FAILURES
CHECKED-OWNERSHIP
BEFORE @ CANARY T=
AFTER @ CANARY T=
T-REPORT
;using
;package
