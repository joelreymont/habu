\ A task still ACTIVATED when a capture begins. No image carries a thread, so
\ lib/task.f's capture sweep refuses this program by name at
\ IMAGE-LIFECYCLE:PREPARE - the one word both capture paths run
\ (tools/aot-build-core.f NATIVE-BUILD, src/habu/snap.f RETIRE-AND-PERSIST), and
\ the reason this file calls it rather than being built: hb-build's own phases
\ mutate the dictionary after the program loads, and a live task already refuses
\ that with a bare exit $4F and no message (measured).
\
\ THE WORKER PARKS AND IS NEVER WOKEN, so its state at capture is RUNNING
\ whatever the scheduler does: TASK:ACTIVATE publishes RUNNING before
\ pthread_create, and nothing here halts or joins it.
require lib/task.f
require lib/image-lifecycle.f

package STRIPPED-LIFECYCLE-RUNNING-SUBJECT
private

TASK:MIN-STACK TASK:TASK WORKER

: BODY ( -- )
   begin TASK:STOP again ;

public

: ARM ( -- )
   ['] BODY WORKER TASK:ACTIVATE ;

\ Nothing may be defined after ARM runs: a live task refuses dictionary mutation.
: CAPTURE ( -- )
   IMAGE-LIFECYCLE:PREPARE ;

;package

STRIPPED-LIFECYCLE-RUNNING-SUBJECT:ARM
STRIPPED-LIFECYCLE-RUNNING-SUBJECT:CAPTURE
