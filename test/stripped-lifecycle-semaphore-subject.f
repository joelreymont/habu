\ A pool semaphore acquired while loading is released by the ordinary
\ one-shot capture cleanup. The restored process can claim the same pool.
require lib/task.f
require lib/image-lifecycle.f

package STRIPPED-LIFECYCLE-SEMAPHORE-SUBJECT
private

$4A constant FAILURE-RC
TYPED-VARIABLE POOL TASK:sem

: CLEANUP ( -- ) POOL @ TASK:FREE-SEMAPHORE ;

: ARM ( -- )
   TASK:NEW-SEMAPHORE POOL !
   0 POOL @ TASK:SEMAPHORE-INIT
   ['] CLEANUP IMAGE-LIFECYCLE:REGISTER ;

ARM

public

: RUN ( -- )
   TASK:NEW-SEMAPHORE {: s :}
   1 s TASK:SEMAPHORE-INIT
   s TASK:TRY-WAIT 0= if
      s" stripped-lifecycle-semaphore: fresh pool unusable" FAILURE-RC die
   then
   s TASK:FREE-SEMAPHORE
   s" stripped-lifecycle-semaphore: ok" type cr ;

;package
