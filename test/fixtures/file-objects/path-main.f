\ path-main.f - a program whose top level fills OWN-PATH's cache and makes
\ the main task's park, built by tools/hb-build.f before and after bare-main.f.

require src/lib/process.f
require src/lib/task.f

package FILE-OBJECTS-PATH-MAIN
PROCESS:OWN-PATH drop
TASK:SELF TASK:WAKE
public
: SHOW ( -- )
   PROCESS:OWN-PATH type cr
   TASK:SELF TASK:WAKE ;
;package

: MAIN ( -- ) FILE-OBJECTS-PATH-MAIN:SHOW ;
