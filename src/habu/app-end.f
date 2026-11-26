\ app-end.f - a stripped program's entry, loaded after the program's source.
require src/core/exit-status.f
require src/lib/error.f
HB-TARGET-WASM? 0= [if] require src/lib/process-reaper.f [then]

package APP-END
private

\ A Wasm program spawns no reaper.
HB-TARGET-WASM? [if]
: REAPER ( -- ) ;
[then]
HB-TARGET-WASM? 0= [if]
: REAPER ( -- ) PROCESS:REAPER-ENTRY ;
[then]

: ENTRY ( -- )
   REAPER
   ['] MAIN catch {: rc:n :}
   rc 0= if exit then
   REJECTION:RELEASE
   rc 0 >  rc 256 <  and if rc throw then
   rc ERROR:UNCAUGHT
   rc ERROR:STATUS throw ;

' ENTRY APP-ENTRY:XT!
;package
