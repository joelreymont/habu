\ Pool semaphores still held when a capture begins, past the pool's first chunk
\ of $40 records. A live record pins the chunk the pool grew to hold it, and that
\ chunk is a mapping of this process, so src/lib/task.f's capture cleanup refuses
\ this program by name at IMAGE-LIFECYCLE:PREPARE rather than leave the mapping
\ to the image. Called directly for the reason
\ test/stripped-lifecycle-running-subject.f gives.
require src/lib/task.f
require src/lib/image-lifecycle.f

package STRIPPED-LIFECYCLE-POOL-SUBJECT
private

$48 constant HELD-N

public

: ARM ( -- )
   HELD-N 0 ?do TASK:NEW-SEMAPHORE 0 swap TASK:SEMAPHORE-INIT loop ;

: CAPTURE ( -- )
   IMAGE-LIFECYCLE:PREPARE ;

;package

STRIPPED-LIFECYCLE-POOL-SUBJECT:ARM
STRIPPED-LIFECYCLE-POOL-SUBJECT:CAPTURE
