require test/fixtures/native-file-state-prefix.f
require test/fixtures/native-file-state-dependency.f
package FILE-STATE-A
private
create FILE-STATE-SPILL-BEFORE 24 allot
create FILE-STATE-SPILL-BODY 4096 allot
: OPEN-SPILL ( -- )
   NSTR:SEGMENTS {: before:n :}
   256 0 ?do
      4096 0 ?do 76 FILE-STATE-SPILL-BODY i + c! loop
      i FILE-STATE-SPILL-BODY CELL-VIEW !
      FILE-STATE-SPILL-BODY 4096 NSTR:INTERN drop
      NSTR:SEGMENTS before > if leave then
   loop
   NSTR:SEGMENTS before <= if E-NSTR-CAP throw then ;
: HIDDEN ( -- n ) 5 ;
public
: NINETEEN ( -- n ) 19 ;
TYPED-VARIABLE LOCAL-ACTION [ -- n ]
: LOCAL-ACTION@ ( -- [ -- n ] ) LOCAL-ACTION @ ;
TYPED-VARIABLE LATE-ACTION [ -- n ]
: LATE-ACTION@ ( -- [ -- n ] ) LATE-ACTION @ ;
: LATE-ACTION! ( [ -- n ] -- ) LATE-ACTION ! ;
: FROM-PRIVATE ( -- n ) HIDDEN ;
: EMPTY$ ( -- ptr u8 n ) s" " ;
: WORDS$ ( -- ptr u8 n ) s" file object" ;
: BEFORE-SPILL$ ( -- ptr u8 n ) s" before spill" ;
: UNIT-MAKE ( n -- ) create , does> ( -- n ) @ FILE-STATE-DEPENDENCY:INCREMENT ;
7 PREFIX-MAKE FROM-PREFIX
8 UNIT-MAKE FROM-UNIT
private
create FILE-STATE-SPILL-MIDDLE 8 allot
OPEN-SPILL
create FILE-STATE-SPILL-AFTER 24 allot
;package

package FILE-STATE-B
private
create FIXED-BYTES 8 allot
public
: BYTES$ ( -- ptr u8 n ) FIXED-BYTES 8 ;
: THE-VERY-LONG-PUBLIC-NAME-BEYOND-INLINE-RECORDS ( -- n )
   FILE-STATE-A:FROM-PRIVATE 30 + ;
;package

package FILE-STATE-A
public
: REOPENED ( -- n ) FILE-STATE-B:THE-VERY-LONG-PUBLIC-NAME-BEYOND-INLINE-RECORDS ;
: AFTER-SPILL$ ( -- ptr u8 n ) s" after spill" ;
;package

23 FILE-STATE-A:COUNT!
FILE-STATE-B:BYTES$ drop FILE-STATE-A:LINK!
' FILE-STATE-A:NINETEEN FILE-STATE-A:ACTION!
' FILE-STATE-A:NINETEEN FILE-STATE-A:LOCAL-ACTION !
65 FILE-STATE-A:LETTERS$ drop c!
66 FILE-STATE-B:BYTES$ drop c!
package FILE-STATE-PROVIDED
public
: NUMBER ( -- n ) 13 ;
;package
s" test/fixtures/native-file-state-provided.f" provided
