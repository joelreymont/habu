\ stderr.f - a test's hold on fd 2, package STDERR. CAPTURE sends fd 2 to the
\ file at a path, created empty or truncated, until ;CAPTURE gives the process
\ its own stderr back. A failed dup or dup2 is E-FS-IO. CAPTURING runs a word
\ under a capture and answers the file's text, removing the file.
\ STORAGE CLASS. PROCESS-WIDE: fd 2 and the descriptor saving it belong to the
\ process, so one capture is open at a time.

require src/lib/error.f
require src/lib/arena.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/fs-mutate.f

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

: CAPTURE ( str -- )
   {: path:str :}
   SAVED @ 0<> if E-STATE throw then
   path s" " FS:WRITE-ALL
   path FS:OPEN-APPEND-FD {: file:n :}
   FD:STDERR FD>N F-DUPFD SAVE-FD-MIN fcntl {: saved:n :}
   saved 0 < if file close-rc drop E-FS-IO throw then
   file REDIRECT-TO 0= if saved close-rc drop E-FS-IO throw then
   saved SAVED ! ;


: ;CAPTURE ( -- )
   SAVED @ {: saved:n :}
   saved 0= if E-STATE throw then
   0 SAVED !
   saved REDIRECT-TO 0= if E-FS-IO throw then ;

private

\ Nothing writes the file once its capture has ended, so its size bounds it.
: READ-WHOLE ( str ^ARENA:arena -- str )
   {: path:str a:^ :}
   a path FS:FILE-SIZE ARENA:ALLOC {: at:^ cap:n :}
   at  path at cap FS:READ-ALL >str ;

public

\ Answers the code xt threw, 0 for none, and the text it wrote to fd 2, which
\ the arena holds; the capture ends whether or not xt threw.
: CAPTURING ( [ -- ] str ^ARENA:arena -- n str )
   {: run path:str a:^ :}
   path CAPTURE
   run catch ;CAPTURE
   path a READ-WHOLE
   path FS:REMOVE-FILE ;

;package
