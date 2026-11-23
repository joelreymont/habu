split-str
\ inotify.f - change notices for watched directories, through Linux's
\ inotify. DRAIN discards the pending events unread: a caller learns only that
\ something changed. OPEN and WATCH answer a failure, with the call's errno as
\ FS:ERRNO; a failed DRAIN is E-INOTIFY with the errno as FS:ERRNO, logged
\ with its text.
\ STORAGE CLASS. PROCESS-WIDE: DRAIN's buffer, whose bytes nothing reads.
\ TASK-LOCAL: WATCH's path, in fs.f's band. CALLER-OWNED: every instance,
\ closed with `close`.
HB-TARGET-LINUX-KERNEL? ?TARGET
require src/lib/error.f
require src/lib/adt/option.f
require src/lib/ffi-abi.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/fs-anchored.f
require src/lib/log.f

package INOTIFY

private
-9260 constant E-FIRST
-9269 constant E-LAST

public
-9260 s" an inotify read failed" ERROR:CODE E-INOTIFY

\ Watch masks, <linux/inotify.h>.
$2 constant IN-MODIFY
$4 constant IN-ATTRIB
$8 constant IN-CLOSE-WRITE
$40 constant IN-MOVED-FROM
$80 constant IN-MOVED-TO
$100 constant IN-CREATE
$200 constant IN-DELETE
$400 constant IN-DELETE-SELF
$800 constant IN-MOVE-SELF
$1000000 constant IN-ONLYDIR
$4000000 constant IN-EXCL-UNLINK

private
\ IN_NONBLOCK and IN_CLOEXEC: O_NONBLOCK and O_CLOEXEC on arm64 and x86-64.
$800 constant IN-NONBLOCK
$80000 constant IN-CLOEXEC

\ Holds at least one event: 16 bytes and a name of at most NAME_MAX + 1.
4096 constant SINK-CAP
SINK-CAP SCRATCH-BUFFER: SINK

PROCESS-SYMBOLS
FUNCTION: INIT-CALL inotify_init1 ( n -- i32 ) ;FUNCTION
FUNCTION: ADD-CALL inotify_add_watch ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: READ-CALL read ( n ^u8 n -- n ) ;FUNCTION

: FAIL ( n -- )
   {: e:n :}
   e FS:ERRNO$ {: text:^ tu:n :}
   LOG:ERROR s" inotify" LOG:LINE
   s" read: " LOG:TEXT+ text tu LOG:TEXT+ LOG:;LINE
   e FS:CAUSE! E-INOTIFY throw ;

\ One read: the bytes it discarded, and whether more may be pending.
: READ-ONCE ( fd -- n bool )
   FD>N SINK SINK-CAP READ-CALL {: k:n :}
   k 0 >= if k  k 0 > exit then
   FFI:ERRNO {: e:n :}
   e FD:EAGAIN# = if 0 false exit then
   e FD:EINTR# = if 0 true exit then
   e FAIL ;

public
: OPEN ( -- option<fd> )
   IN-NONBLOCK IN-CLOEXEC or INIT-CALL {: rc:n :}
   rc 0 < if FFI:ERRNO FS:CAUSE! OPTION:NONE exit then
   0 FS:CAUSE! rc >FD OPTION:SOME ;

\ Adds the directory the second descriptor holds to the instance's watches;
\ under IN-ONLYDIR a descriptor of anything else is ENOTDIR.
: WATCH ( fd fd n -- bool )
   {: inst:fd dir:fd mask:n :}
   inst FD>N dir FS:FD-LINKZ mask ADD-CALL 0 >= if
      0 FS:CAUSE! true exit
   then
   FFI:ERRNO FS:CAUSE! false ;

\ Reads until none is pending; answers the bytes of events discarded.
: DRAIN ( fd -- n )
   {: f:fd :}
   0 begin f READ-ONCE while + repeat + ;

;package
