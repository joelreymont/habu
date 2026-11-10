\ path-main.f - a program whose top level fills ENGINE-ID's path cache and makes
\ the main task's park, built by tools/hb-build.f before and after bare-main.f;
\ its MAIN prints the running program's own path and wakes its own park.

require src/lib/engine-id.f
require src/lib/task.f

package FILE-OBJECTS-PATH-MAIN
ENGINE-ID:PATH$ drop drop
TASK:SELF TASK:WAKE
public
: SHOW ( -- )
   ENGINE-ID:PATH$ type cr
   TASK:SELF TASK:WAKE ;
;package

: MAIN ( -- ) FILE-OBJECTS-PATH-MAIN:SHOW ;
