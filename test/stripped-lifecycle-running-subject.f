\ A task still ACTIVATED when a capture begins. No image carries a thread, so
\ src/lib/task.f's capture sweep refuses this program by name at
\ IMAGE-LIFECYCLE:PREPARE, which both capture paths run (tools/aot-build-core.f
\ NATIVE-BUILD, src/habu/native-runtime.f CAPTURE-PREPARE), called directly.

\ The worker parks and is never woken, so its state at capture is RUNNING:
\ TASK:ACTIVATE publishes RUNNING before pthread_create, and nothing halts or
\ joins it.
require src/lib/task.f
require src/lib/image-lifecycle.f

package STRIPPED-LIFECYCLE-RUNNING-SUBJECT
private

TASK:MIN-STACK TASK:TASK WORKER

: BODY ( -- )
   begin TASK:STOP again ;

public

: ARM ( -- )
   ['] BODY WORKER TASK:ACTIVATE ;

: CAPTURE ( -- )
   IMAGE-LIFECYCLE:PREPARE ;

;package

STRIPPED-LIFECYCLE-RUNNING-SUBJECT:ARM
STRIPPED-LIFECYCLE-RUNNING-SUBJECT:CAPTURE
