\ process-path.f - the running program's path, as the kernel reports it: OWN-PATH.
\ Another file can take the pathname after it resolves.
\ STORAGE CLASS. PROCESS-WIDE: the cache is scratch, so a build leaves it out.

require src/lib/error.f
require src/lib/ffi-abi.f

package PROCESS
private

PATH-CAP 1 + constant OWN-PATH-CAP

OWN-PATH-CAP SCRATCH-BUFFER: OWN-PATH-BUF
SCRATCH-VARIABLE OWN-PATH-U
SCRATCH-VARIABLE OWN-PATH-DONE

\ Linux answers for the executable mapping: its pathname now, after a rename
\ too, with " (deleted)" appended once the file is unlinked.
create PROC-SELF-EXE
   char / c, char p c, char r c, char o c, char c c, char / c,
   char s c, char e c, char l c, char f c, char / c,
   char e c, char x c, char e c, 0 c,

PROCESS-SYMBOLS
FUNCTION: SELF-PATH proc_pidpath ( n ^u8 n -- n ) ;FUNCTION

\ The kernel's byte count for the program's pathname in OWN-PATH-BUF; zero or
\ negative when it refuses.
: READ-OWN-PATH ( -- n )
   HB-TARGET-MACOS? if getpid OWN-PATH-BUF OWN-PATH-CAP SELF-PATH exit then
   HB-TARGET-LINUX-KERNEL? if
      PROC-SELF-EXE OWN-PATH-BUF OWN-PATH-CAP readlink exit
   then
   0 ;

public

\ Throws E-ENGINE-PATH where the OS will not report the pathname (a sandbox, no
\ /proc) or it is longer than PATH-CAP, which a count filling OWN-PATH-BUF shows.
: OWN-PATH ( -- ^u8 n )
   OWN-PATH-DONE @ 0= if
      READ-OWN-PATH dup 0 <= over OWN-PATH-CAP >= or if
         drop E-ENGINE-PATH throw
      then
      OWN-PATH-U !  -1 OWN-PATH-DONE !
   then
   OWN-PATH-BUF OWN-PATH-U @ ;

;package
