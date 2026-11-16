\ app-end.f - a stripped program's entry, loaded after the program's source.
require src/lib/error.f

package APP-END
private

: ENTRY ( -- )
   ['] MAIN ERROR:CAUGHT {: rc:n :}
   rc 0= if exit then
   REJECTION:RELEASE
   rc 0 >  rc 256 <  and if rc throw then
   rc ERROR:UNCAUGHT
   UNCAUGHT-RC throw ;

' ENTRY APP-ENTRY:XT!
;package
