\ Pool semaphores acquired while loading are released by the ordinary one-shot
\ capture cleanup. The subject holds more than the pool's first chunk of $40
\ records, so the capture also gives back the chunk the pool grew while loading:
\ one still mapped would leave the builder's mapping in the image's data. The
\ restored process can claim as many again.
require src/lib/task.f
require src/lib/image-lifecycle.f

package STRIPPED-LIFECYCLE-SEMAPHORE-SUBJECT
private

$4A constant FAILURE-RC
$48 constant HELD-N
HELD-N TYPED-BUFFER HELD TASK:sem

: CLEANUP ( -- )
   HELD-N 0 ?do i HELD @ TASK:FREE-SEMAPHORE loop ;

: TAKE ( -- )
   HELD-N 0 ?do
      TASK:NEW-SEMAPHORE i HELD !
      0 i HELD @ TASK:SEMAPHORE-INIT
   loop ;

: ARM ( -- )
   TAKE
   ['] CLEANUP IMAGE-LIFECYCLE:REGISTER ;

ARM

\ Each record counts on its own: one signal, one take.
: USABLE? ( -- bool )
   0 0=
   HELD-N 0 ?do
      i HELD @ TASK:SIGNAL
      i HELD @ TASK:TRY-WAIT and
   loop ;

public

: RUN ( -- )
   TAKE
   USABLE? 0= if
      s" stripped-lifecycle-semaphore: fresh pool unusable" FAILURE-RC die
   then
   CLEANUP
   s" stripped-lifecycle-semaphore: ok" type cr ;

;package
