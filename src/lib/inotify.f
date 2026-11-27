\ inotify.f - change notices for watched directories, through Linux's
\ inotify. WATCH answers the watch descriptor the kernel names a directory by,
\ the same one when the directory is watched again; EACH hands each pending
\ event's descriptor and mask to a quotation, skipping its name. IN-IGNORED
\ marks a watch the kernel dropped, after UNWATCH or when its directory went;
\ IN-Q-OVERFLOW, on descriptor -1, marks events the kernel lost. OPEN and
\ WATCH leave a failed call's errno as FS:ERRNO; a failed read in EACH is
\ E-INOTIFY with the errno as FS:ERRNO, logged with its text.
\ STORAGE CLASS. SCOPE-OWNED: EACH's read buffer, one MEM:WITH-BYTES scope per
\ call, released when it returns or throws, so tasks may read at once.
\ TASK-LOCAL: WATCH's path, in fs.f's band. CALLER-OWNED: every instance,
\ closed with `close`.
HB-TARGET-LINUX? ?TARGET
require src/lib/error.f
require src/lib/adt/option.f
require src/lib/ffi-abi.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/fs-anchored.f
require src/lib/log.f
require src/lib/le.f
require src/lib/memory.f

package INOTIFY

private
-9260 constant E-FIRST
-9269 constant E-LAST

public
-9260 s" an inotify read failed" ERROR:CODE E-INOTIFY

\ Watch masks, <linux/inotify.h>.
$1 constant IN-ACCESS
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

\ Event masks the kernel sets alone.
$2000 constant IN-UNMOUNT
$4000 constant IN-Q-OVERFLOW
$8000 constant IN-IGNORED

private
\ IN_NONBLOCK and IN_CLOEXEC: O_NONBLOCK and O_CLOEXEC on arm64 and x86-64.
$800 constant IN-NONBLOCK
$80000 constant IN-CLOEXEC

\ Holds at least one event: 16 bytes and a name of at most NAME_MAX + 1.
4096 constant BUF-CAP

\ struct inotify_event: i32 wd, u32 mask, u32 cookie, u32 len, then len
\ bytes of name.
16 constant EVENT-HEAD

PROCESS-SYMBOLS
FUNCTION: INIT-CALL inotify_init1 ( n -- i32 ) ;FUNCTION
FUNCTION: ADD-CALL inotify_add_watch ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: RM-CALL inotify_rm_watch ( n n -- i32 ) ;FUNCTION
FUNCTION: READ-CALL read ( n ^u8 n -- n ) ;FUNCTION

: FAIL ( n -- )
   {: e:n :}
   e FS:ERRNO$ {: text:str :}
   LOG:ERROR s" inotify" LOG:LINE
   s" read: " LOG:TEXT+ text LOG:TEXT+ LOG:;LINE
   e FS:CAUSE! E-INOTIFY throw ;

\ One read into the buffer: the bytes it read, and whether more may be pending.
: READ-ONCE ( fd ^u8 -- n bool )
   {: f:fd buf:^ :}
   f FD>N buf BUF-CAP READ-CALL {: k:n :}
   k 0 >= if k  k 0 > exit then
   FFI:ERRNO {: e:n :}
   e FD:EAGAIN# = if 0 false exit then
   e FD:EINTR# = if 0 true exit then
   e FAIL ;

\ Calls q with the descriptor and mask of the event at byte off of the
\ buffer; answers the next event's offset.
: EVENT ( ^u8 n [ n n -- ] -- n )
   {: buf:^ off:n q :}
   buf off + {: e:^ :}
   e LE:S32@  e 4 + LE:U32@  q execute
   off EVENT-HEAD +  e 12 + LE:U32@ + ;

\ Calls q for each event in the buffer's first k bytes; answers how many.
: EVENTS ( ^u8 n [ n n -- ] -- n )
   {: buf:^ k:n q :}
   0 0 begin dup k < while
      buf swap q EVENT  swap 1 + swap
   repeat drop ;

\ One read's events: their count, and whether more may be pending.
: PASS ( fd ^u8 [ n n -- ] -- n bool )
   {: f:fd buf:^ q :}
   f buf READ-ONCE {: k:n more:bool :}
   buf k q EVENTS more ;

\ The WITH-BYTES body: the scope's bytes are this call's read buffer.
: EACH-SCOPE ( fd [ n n -- ] ^u8 NUM:alloc-byte-len -- n )
   drop {: f:fd q buf:^ :}
   0 begin f buf q PASS while + repeat + ;

public
: OPEN ( -- option<fd> )
   IN-NONBLOCK IN-CLOEXEC or INIT-CALL {: rc:n :}
   rc 0 < if FFI:ERRNO FS:CAUSE! OPTION:NONE exit then
   0 FS:CAUSE! rc >FD OPTION:SOME ;

\ Adds the directory the second descriptor holds to the instance's watches and
\ answers its watch descriptor; under IN-ONLYDIR anything else is ENOTDIR.
: WATCH ( fd fd n -- option<n> )
   {: inst:fd dir:fd mask:n :}
   inst FD>N dir FS:FD-LINKZ mask ADD-CALL {: wd:n :}
   wd 0 < if FFI:ERRNO FS:CAUSE! OPTION:NONE exit then
   0 FS:CAUSE! wd OPTION:SOME ;

\ Drops a watch. A failure only sets FS:ERRNO: the kernel already dropped a
\ watch whose directory went.
: UNWATCH ( fd n -- )
   {: inst:fd wd:n :}
   inst FD>N wd RM-CALL 0 < if FFI:ERRNO FS:CAUSE! exit then
   0 FS:CAUSE! ;

\ Reads until none is pending; answers how many events it read.
: EACH ( fd [ n n -- ] -- n )
   BUF-CAP MEM:BYTES-ALLOC-LEN [: EACH-SCOPE ;] MEM:WITH-BYTES ;

;package
