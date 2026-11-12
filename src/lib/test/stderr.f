\ stderr.f - a test's hold on fd 2, package STDERR. CAPTURE sends fd 2 to the
\ file at a path, created empty or truncated, until ;CAPTURE gives the process
\ its own stderr back. A failed dup or dup2 is E-FS-IO.
\ STORAGE CLASS. PROCESS-WIDE: fd 2 and the descriptor saving it belong to the
\ process, so one capture is open at a time.

require src/lib/error.f
require src/lib/fd.f
require src/lib/fs.f

package STDERR

private

-9610 constant E-FIRST
-9619 constant E-LAST

public

-9610 constant E-STATE      \ CAPTURE while one is open, ;CAPTURE while none is

private

0 constant F-DUPFD
$A constant SAVE-FD-MIN     \ the saved stderr lands clear of 0..2
variable SAVED              \ the process's own stderr while a capture is open, else 0


: REDIRECT-TO ( n -- bool )
   {: from:n :}
   from FD:STDERR FD>N dup2 {: moved:n :}
   from close-rc drop
   moved 0 >= ;

public

: CAPTURE ( ptr u8 n -- )
   {: path:ptr u:n :}
   SAVED @ 0<> if E-STATE throw then
   path u s" " FS:WRITE-ALL
   path u FS:OPEN-APPEND-FD {: file:n :}
   FD:STDERR FD>N F-DUPFD SAVE-FD-MIN fcntl {: saved:n :}
   saved 0 < if file close-rc drop E-FS-IO throw then
   file REDIRECT-TO 0= if saved close-rc drop E-FS-IO throw then
   saved SAVED ! ;


: ;CAPTURE ( -- )
   SAVED @ {: saved:n :}
   saved 0= if E-STATE throw then
   0 SAVED !
   saved REDIRECT-TO 0= if E-FS-IO throw then ;

;package
