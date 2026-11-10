\ engine-id.f - the running engine's path and stamp. PATH$ is the
\ kernel-reported pathname, which can be replaced after it resolves, so KEY$
\ fstats a descriptor bound to the running image (/proc/self/exe on Linux, on
\ macOS one whose vnode matches the main executable's mapped vnode): decimal
\ `<ctime seconds>.<nanoseconds>-<size>`. The caches and query buffers are
\ scratch, so a build leaves them out.

require src/lib/error.f
require src/lib/string.f
require src/lib/ffi-abi.f
require src/lib/span.f
require src/lib/fs.f
require src/lib/fmt.f

package ENGINE-ID

PATH-CAP 1 + constant EID-PATH-CAP
64 constant EID-KEY-LEN

\ The caches belong to the process that filled them; a zero DONE flag means
\ nothing is cached yet.
EID-PATH-CAP SCRATCH-BUFFER: EID-PATH
SCRATCH-VARIABLE EID-PATH-U
SCRATCH-VARIABLE EID-PATH-DONE
EID-KEY-LEN SCRATCH-BUFFER: EID-KEY
SCRATCH-VARIABLE EID-KEY-U
SCRATCH-VARIABLE EID-KEY-DONE
EID-PATH-CAP SCRATCH-BUFFER: EID-PATHZ

\ Linux follows the process's executable mapping, even after its old pathname
\ has been renamed and another file has appeared there.
create EID-PROC-EXE
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
FUNCTION: SELF-PATH proc_pidpath ( n ptr u8 n -- n )
   1 2 WRITES-ARG
;FUNCTION
FUNCTION: MAIN-HEADER _dyld_get_image_header ( n -- n ) ;FUNCTION
FUNCTION: MAPPED-INFO proc_pidinfo ( n n n ptr u8 n -- i32 )
   3 4 WRITES-ARG
;FUNCTION
FUNCTION: OPENED-INFO proc_pidfdinfo ( n n n ptr u8 n -- i32 )
   3 4 WRITES-ARG
;FUNCTION

\ The kernel's byte count for the engine's pathname in EID-PATH; zero or
\ negative when it refuses.
: ENGINE-SELF-PATH ( -- n )
   HB-TARGET-MACOS? if getpid EID-PATH EID-PATH-CAP SELF-PATH exit then
   HB-TARGET-LINUX-KERNEL? if
      EID-PROC-EXE EID-PATH EID-PATH-CAP readlink exit
   then
   0 ;

\ Throws E-ENGINE-PATH while the kernel will not report the pathname: a sandbox
\ that refuses proc_pidpath, a Linux without /proc, or a pathname longer than
\ PATH-CAP. EID-PATH holds one byte more than PATH-CAP, so a count that fills it
\ is a truncated pathname: readlink stops at the buffer's size without saying
\ so. Only a reported, whole pathname is cached.
: CACHED-PATH ( -- ptr u8 n )
   EID-PATH-DONE @ 0= if
      ENGINE-SELF-PATH dup 0 <= over EID-PATH-CAP >= or if
         drop E-ENGINE-PATH throw
      then
      EID-PATH-U !  -1 EID-PATH-DONE !
   then
   EID-PATH EID-PATH-U @ ;

: OPEN-RUNNING ( -- n )
   HB-TARGET-LINUX-KERNEL? if
      EID-PROC-EXE open-rd exit
   then
   HB-TARGET-MACOS? if
      CACHED-PATH EID-PATHZ EID-PATH-CAP SPAN:MAKE FFI:CSTR
      open-rd exit
   then
   E-ENGINE-KEY throw ;

: VERIFY-MACOS ( n -- ) {: fd:n :}
   0 MAIN-HEADER {: header:n :}
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
   v FMT:INT$ {: a:ptr u:n :}
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

: PATH$ ( -- ptr u8 n ) CACHED-PATH ;

: KEY$ ( -- ptr u8 n )
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
