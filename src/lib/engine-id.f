split-str
\ engine-id.f - the running program's path, OWN-PATH, as the kernel reports
\ it, and the running engine's stamp, KEY$. KEY$ is decimal
\ `<ctime seconds>.<nanoseconds>-<size>`, read by fstat from a descriptor bound
\ to the running image (/proc/self/exe on Linux, on macOS one whose vnode
\ matches the main executable's mapped vnode), never from the pathname, which
\ another file can take. The caches and query buffers are scratch, so a build
\ leaves them out.

require src/lib/error.f
require src/lib/string.f
require src/lib/ffi-abi.f
require src/lib/span.f
require src/lib/fs.f
require src/lib/fmt.f

package ENGINE-ID

PATH-CAP 1 + constant OWN-PATH-CAP

OWN-PATH-CAP SCRATCH-BUFFER: OWN-PATH-BUF
SCRATCH-VARIABLE OWN-PATH-U
SCRATCH-VARIABLE OWN-PATH-DONE

64 constant EID-KEY-LEN

EID-KEY-LEN SCRATCH-BUFFER: EID-KEY
SCRATCH-VARIABLE EID-KEY-U
SCRATCH-VARIABLE EID-KEY-DONE
FS:PATHZ-CAP SCRATCH-BUFFER: EID-PATHZ

\ Linux answers for the executable mapping: its pathname now, after a rename
\ too, with " (deleted)" appended once the file is unlinked.
create PROC-SELF-EXE
   char / c, char p c, char r c, char o c, char c c, char / c,
   char s c, char e c, char l c, char f c, char / c,
   char e c, char x c, char e c, 0 c,

\ sys/proc_info.h in the local macOS SDK: PROC_PIDREGIONPATHINFO returns a
\ proc_regionwithpathinfo (1272 bytes), with prp_vip.vip_vi.vi_stat dev/ino at
\ offsets 96/104. PROC_PIDFDVNODEINFO returns a vnode_fdinfo (176 bytes), with
\ pvi.vi_stat dev/ino at 24/32. The same SDK probe verified both full returns
\ and equal identities for the running executable and its opened descriptor.
8 constant EID-PROC-REGION
1 constant EID-PROC-FD-VNODE
1272 constant EID-REGION-BYTES
176 constant EID-FD-BYTES
96 constant EID-REGION-DEV
104 constant EID-REGION-INO
24 constant EID-FD-DEV
32 constant EID-FD-INO
EID-REGION-BYTES SCRATCH-BUFFER: EID-MAPPED
EID-FD-BYTES SCRATCH-BUFFER: EID-OPENED

PROCESS-SYMBOLS
FUNCTION: SELF-PATH proc_pidpath ( n ^u8 n -- n ) ;FUNCTION
\ dyld image 0 is the first inserted library when DYLD_INSERT_LIBRARIES names one.
FUNCTION: MAIN-HEADER _NSGetMachExecuteHeader ( -- n ) ;FUNCTION
FUNCTION: MAPPED-INFO proc_pidinfo ( n n n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: OPENED-INFO proc_pidfdinfo ( n n n ^u8 n -- i32 ) ;FUNCTION

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

private

: OPEN-RUNNING ( -- n )
   HB-TARGET-LINUX-KERNEL? if
      PROC-SELF-EXE open-rd exit
   then
   HB-TARGET-MACOS? if
      OWN-PATH EID-PATHZ FS:PATHZ-CAP SPAN:MAKE FFI:CSTR
      open-rd exit
   then
   E-ENGINE-KEY throw ;

: VERIFY-MACOS ( n -- ) {: fd:n :}
   MAIN-HEADER {: header:n :}
   header 0= if E-ENGINE-KEY throw then
   getpid EID-PROC-REGION header EID-MAPPED EID-REGION-BYTES MAPPED-INFO
   EID-REGION-BYTES <> if E-ENGINE-KEY throw then
   getpid fd EID-PROC-FD-VNODE EID-OPENED EID-FD-BYTES OPENED-INFO
   EID-FD-BYTES <> if E-ENGINE-KEY throw then
   EID-MAPPED EID-REGION-DEV + LE:U32@
   EID-OPENED EID-FD-DEV + LE:U32@ =
   EID-MAPPED EID-REGION-INO + LE:U64@
   EID-OPENED EID-FD-INO + LE:U64@ = and
   0= if E-ENGINE-KEY throw then ;

\ Appends a number, at least zero, to the key in decimal without padding.
: DEC+ ( n -- ) {: v:n :}
   v 0 < if E-ENGINE-KEY throw then
   v FMT:INT$ {: a:^ u:n :}
   a EID-KEY EID-KEY-U @ + u BYTE-COPY
   EID-KEY-U @ u + EID-KEY-U ! ;

: BYTE+ ( n -- )
   EID-KEY EID-KEY-U @ + c!
   EID-KEY-U @ 1+ EID-KEY-U ! ;

\ Preserve the descriptor across catch so KEY$ can close it even if a foreign
\ identity query or the fstat throws. The returned descriptor keeps catch's
\ stack effect preserving; KEY$ discards that output before using its own local.
: STAMP-RUNNING ( n -- n ) {: fd:n :}
   HB-TARGET-MACOS? if fd VERIFY-MACOS then
   fd >FD FS:FD-STAMP {: sec:n ns:n size:n :}
   0 EID-KEY-U !
   sec DEC+ $2E BYTE+ ns DEC+ $2D BYTE+ size DEC+
   fd ;

public

: KEY$ ( -- str )
   EID-KEY-DONE @ 0= if
      OPEN-RUNNING {: fd:n :}
      fd 0 < if E-ENGINE-KEY throw then
      fd [: STAMP-RUNNING ;] catch {: code:n :} drop
      fd close
      code 0<> if code throw then
      -1 EID-KEY-DONE !
   then
   EID-KEY EID-KEY-U @ ;

;package
