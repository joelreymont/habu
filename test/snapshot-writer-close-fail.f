\ Force the real image writer's final close-rc to fail after writing its bytes.
\ Only this child process installs the callback, before APP-IMAGE:SAVE.

require src/habu/snap-lib.f

package SNAP-WRITER-CLOSE-FAIL

: CLOSE-EARLY ( n -- )
   close ;

: ARM ( -- )
   [: CLOSE-EARLY ;] SNAP-CLOSE-SEAM:INSTALL-TEST ;

ARM

;package
