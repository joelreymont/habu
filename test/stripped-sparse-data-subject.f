\ A stripped application's persistent DATA travels as its non-zero extents, so a
\ large `allot`ed hole costs the image nothing while the initialised cells on
\ either side of it still arrive with their values.
package STRIPPED-SPARSE-DATA-SUBJECT

$4A constant FAILURE-RC
1000000 constant HOLE-BYTES
$5A5A5A5A5A5A5A5A constant LOW-MARK
$A5A5A5A5A5A5A5A5 constant HIGH-MARK
$37 constant LOW-BYTE
$5B constant HIGH-BYTE

\ The hole is `allot`ed and never written at compile time. The two marks bracket
\ it, so an image that dropped a run would lose a mark and an image that stored
\ the span verbatim would carry the whole million zero bytes.
create LOW-CELL LOW-MARK ,
create HOLE HOLE-BYTES allot
create HIGH-CELL HIGH-MARK ,

: EXPECT ( bool -- )
   0= if s" stripped-sparse-data: mismatch" FAILURE-RC die then ;

: HOLE-LAST ( -- ptr u8 )
   HOLE HOLE-BYTES 1 - + ;

: HOLE-MIDDLE ( -- ptr u8 )
   HOLE HOLE-BYTES 2 / + ;

\ The restore leaves an untravelled offset on the anonymous mapping's own zero.
: HOLE-ZEROED? ( -- bool )
   HOLE c@ 0=  HOLE-LAST c@ 0= and  HOLE-MIDDLE c@ 0= and ;

: WRITE-ENDS ( -- )
   LOW-BYTE HOLE c!
   HIGH-BYTE HOLE-LAST c! ;

: ENDS-READ-BACK? ( -- bool )
   HOLE c@ LOW-BYTE =  HOLE-LAST c@ HIGH-BYTE = and ;

: MIDDLE-UNDISTURBED? ( -- bool )
   HOLE-MIDDLE c@ 0= ;

: MARKS-TRAVELLED? ( -- bool )
   LOW-CELL @ LOW-MARK =  HIGH-CELL @ HIGH-MARK = and ;

public

: RUN ( -- )
   MARKS-TRAVELLED? EXPECT
   HOLE-ZEROED? EXPECT
   WRITE-ENDS
   ENDS-READ-BACK? EXPECT
   MIDDLE-UNDISTURBED? EXPECT
   MARKS-TRAVELLED? EXPECT
   s" stripped-sparse-data: ok" type cr ;

;package

: MAIN ( -- )
   STRIPPED-SPARSE-DATA-SUBJECT:RUN ;
