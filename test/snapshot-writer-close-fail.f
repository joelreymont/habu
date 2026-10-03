\ Force the real image writer's final close-rc to fail after writing its bytes.
\ Only this child process arms the writer's private BEFORE-CLOSE hook, before
\ APP-IMAGE:SAVE. No qualified name reaches the hook, so a fixture reopens
\ package SNAP to arm it.

require src/habu/snap-lib.f

package SNAP

: CLOSE-EARLY ( n -- )
   close ;

: ARM-CLOSE-FAIL ( -- )
   [: CLOSE-EARLY ;] is BEFORE-CLOSE ;

ARM-CLOSE-FAIL

;package
