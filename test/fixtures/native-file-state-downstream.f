require test/fixtures/native-file-state-unit.f
require test/fixtures/native-file-state-dependency.f
require test/fixtures/native-file-state-provided.f
package FILE-STATE-CLIENT
public
3 FILE-STATE-A:UNIT-MAKE CREATED-LATER
: TWENTY ( -- n ) 20 ;
: VALUE ( -- n )
   FILE-STATE-A:COUNT@
   FILE-STATE-A:FROM-PREFIX +
   FILE-STATE-A:FROM-UNIT +
   CREATED-LATER +
   FILE-STATE-A:ACTION@ execute +
   FILE-STATE-A:LOCAL-ACTION@ execute +
   FILE-STATE-A:LATE-ACTION@ execute + ;
: LONG ( -- n )
   FILE-STATE-A:REOPENED ;
: DEPENDENCY ( -- n )
   0 FILE-STATE-DEPENDENCY:INCREMENT FILE-STATE-PROVIDED:NUMBER + ;
;package
' FILE-STATE-CLIENT:TWENTY FILE-STATE-A:LATE-ACTION!
