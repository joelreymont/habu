\ entropy.f - one cell from the kernel's secure generator through libc
\ getentropy (macOS; glibc 2.25 and later). RANDOM blocks only until the
\ generator is first seeded. A failed call is logged with its errno, then
\ E-FILL.
\ STORAGE CLASS. TASK-LOCAL: getentropy writes through a pointer, so its cell
\ is a TASK:+USER slot.

require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/log.f
require src/lib/task.f

package ENTROPY
public

\ Codes -9540..-9549, the block after UTF16's.
-9540 constant E-FIRST
-9549 constant E-LAST
-9540 s" getentropy failed to fill a cell" ERROR:FAULT E-FILL

private

TASK:+USER DRAWN n

PROCESS-SYMBOLS
FUNCTION: GETENTROPY-CALL getentropy ( ^u8 n -- i32 )
;FUNCTION

\ getentropy's failure, logged with its errno.
: FILL-FAILED ( -- )
   FFI:ERRNO {: errno:n :}
   LOG:ERROR s" entropy" LOG:LINE s" getentropy failed, errno " LOG:TEXT+
      errno LOG:INT+ LOG:;LINE
   E-FILL throw ;

public

: RANDOM ( -- n )
   DRAWN BYTE-VIEW CELL GETENTROPY-CALL 0<> if FILL-FAILED then
   DRAWN @ ;

;package
