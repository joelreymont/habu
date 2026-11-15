\ fs-anchored.f - descriptor-anchored lookup, and on Linux the opens, links,
\ renames, removals, directories and stats of one name under a directory
\ descriptor. A walk owns one mapping and two fds. Each component opens
\ relative to the live parent with O_NOFOLLOW (directories also O_DIRECTORY),
\ so a replaced symlink cannot redirect a lookup and an open directory stays
\ the anchor across a rename. The walk does not prove the anchor is the
\ intended root, nor guard a lookup against cross-root renames, hard links,
\ mounts or content changes.
\ STORAGE CLASS. CALLER-OWNED: every descriptor answered is the caller's.
\ TASK-LOCAL: a name's NUL copy and TRY-STAT-AT's stat, in fs.f's band.
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/core/compile-reads.f
require src/lib/memory.f
require src/lib/ffi-abi.f
require src/lib/span.f

package FS
private

0 constant A-CUR-CELL
1 constant A-NEXT-CELL
2 constant A-RESULT-CELL
3 cells constant A-PATH-OFF
\ libc struct stat has cell alignment, independently of the byte path's length.
A-PATH-OFF PATHZ-CAP + CELL 1- + CELL negate and constant A-STAT-OFF
A-STAT-OFF STAT-CAP + constant A-PROBE-OFF
A-PROBE-OFF 1 + constant A-WORK-BYTES
4 constant A-INTERRUPTED

PROCESS-SYMBOLS
FUNCTION: A-READ-CALL read ( n ^u8 n -- n ) ;FUNCTION
FUNCTION: A-CLOSE-CALL close ( n -- i32 ) ;FUNCTION

: A-SLOT ( ^u8 n -- ^n ) {: work:^ idx:n :}
   work CELL-VIEW idx cells + ;
: A-CUR@ ( ^u8 -- n ) A-CUR-CELL A-SLOT @ ;
: A-CUR! ( n ^u8 -- ) A-CUR-CELL A-SLOT ! ;
: A-NEXT@ ( ^u8 -- n ) A-NEXT-CELL A-SLOT @ ;
: A-NEXT! ( n ^u8 -- ) A-NEXT-CELL A-SLOT ! ;
: A-PATH-BUF ( ^u8 -- ^u8 ) A-PATH-OFF + ;
: A-STAT-BUF ( ^u8 -- ^u8 ) A-STAT-OFF + ;
: A-PROBE-BUF ( ^u8 -- ^u8 ) A-PROBE-OFF + ;

: A-MAP ( -- ^u8 NUM:alloc-byte-len )
   A-WORK-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: work:^ len:NUM:alloc-byte-len :}
   -1 work A-CUR! -1 work A-NEXT!
   work len ;

\ close(2) can have retired a descriptor even on failure. Clear its slot first.
: A-CLOSE-SLOT ( ^u8 n -- n ) {: work:^ idx:n :}
   work idx A-SLOT @ {: file:n :}
   file 0 < if 0 exit then
   -1 work idx A-SLOT !
   file A-CLOSE-CALL 0= if 0 exit then
   FFI:ERRNO ;
: A-CLEANUP ( ^u8 -- n ) {: work:^ :}
   work A-NEXT-CELL A-CLOSE-SLOT {: first:n :}
   work A-CUR-CELL A-CLOSE-SLOT {: second:n :}
   first 0<> if first else second then ;

\ The call's code and its cause, 0 0 when it succeeded. The primary cause was
\ taken before cleanup's close calls replaced libc errno.
: A-SETTLE ( ^u8 NUM:alloc-byte-len n n -- n n )
   {: work:^ len:NUM:alloc-byte-len code:n cause:n :}
   work A-CLEANUP {: closing:n :}
   work len MEM:RELEASE-BYTES
   code 0<> if code cause exit then
   closing 0<> if E-FS-IO closing exit then
   0 0 ;
: A-FINISH ( ^u8 NUM:alloc-byte-len n n -- )
   A-SETTLE {: code:n cause:n :}
   cause CAUSE!
   code 0<> if code throw then ;

\ O_RDONLY|O_NOFOLLOW|O_CLOEXEC|O_NONBLOCK|O_NOCTTY, READ-FILE-AT's last
\ component. Darwin: xnu bsd/sys/fcntl.h. Linux arm64: arch/arm64 asm/fcntl.h
\ overrides the generic nofollow bit; x86-64 uses the generic bits.
: A-FILE-FLAGS ( -- n )
   HB-TARGET-MACOS? if $100 $1000000 or $4 or $20000 or exit then
   HB-TARGET-LINUX-X86-64? if $20000 $80000 or $800 or $100 or exit then
   $8000 $80000 or $800 or $100 or ;

: A-SNAPSHOT ( ^u8 n ^u8 -- ^u8 )
   {: src:^ u:n work:^ :}
   u 0 <= u PATH-CAP > or if E-FS-PATH throw then
   work A-PATH-BUF {: dst:^ :}
   src u dst PATHZ-CAP SPAN:MAKE FFI:CSTR ;
: A-COMPONENT-END ( ^u8 n n -- n ) {: path:^ u:n at:n :}
   at begin dup u < while
      path over + c@ SLASH = if exit then
      1+
   repeat ;
\ Whether path's bytes from up to stop are a name: not empty, . or ..
: A-COMPONENT? ( ^u8 n n -- bool ) {: path:^ from:n stop:n :}
   stop from - {: u:n :}
   u 0= if false exit then
   path from + {: name:^ :}
   u 1 = if name c@ DOT <> exit then
   u 2 = if name c@ DOT = name 1 + c@ DOT = and 0= exit then
   true ;
: A-COMPONENT-OK ( ^u8 n n -- )
   A-COMPONENT? 0= if E-FS-PATH throw then ;
: A-VALIDATE ( ^u8 n bool -- n )
   {: path:^ u:n base:bool :}
   path c@ SLASH = {: absolute:bool :}
   absolute base 0= and if E-FS-PATH throw then
   u 1 = base and if
      absolute if 1 exit then
      path c@ DOT = if 1 exit then
   then
   absolute if 1 else 0 then {: start:n :}
   start begin dup u < while
      {: at:n :}
      path u at A-COMPONENT-END {: stop:n :}
      path at stop A-COMPONENT-OK
      stop u < if stop 1 + else stop then
   repeat
   drop
   path u 1 - + c@ SLASH = if E-FS-PATH throw then
   start ;

: A-OPEN-BASE ( ^u8 ^u8 -- ) {: path:^ work:^ :}
   path c@ SLASH = if
      AT-CWD S\" /\z" drop OPEN-DIR-FLAGS OPEN-AT-CALL
   else
      AT-CWD S\" .\z" drop OPEN-DIR-FLAGS OPEN-AT-CALL
   then
   dup 0 < if drop E-FS-OPEN ERRNO-FAIL then
   work A-CUR! ;

: A-OPEN-NEXT ( ^u8 n ^u8 n -- ) {: work:^ parent:n component:^ flags:n :}
   parent component flags OPEN-AT-CALL
   dup 0 < if drop E-FS-OPEN ERRNO-FAIL then
   work A-NEXT!
   work A-CUR-CELL A-CLOSE-SLOT dup 0<> if CAUSE! E-FS-IO throw then drop
   work A-NEXT@ work A-CUR!
   -1 work A-NEXT! ;

\ A-VALIDATE checks every component before the first open. Splitting ends each
\ component through FFI:CSTR! inside this call's snapshot, never in the caller's
\ path.
: A-WALK ( ^u8 n n ^u8 n bool -- )
   {: path:^ u:n start:n work:^ parent:n file:bool :}
   start begin dup u < while
      {: at:n :}
      path u at A-COMPONENT-END {: stop:n :}
      stop at - path at + PATHZ-CAP at - SPAN:MAKE FFI:CSTR! drop
      stop u = file and if A-FILE-FLAGS else OPEN-DIR-FLAGS then {: flags:n :}
      work A-CUR@ dup 0 < if drop parent then {: from:n :}
      work from path at + flags A-OPEN-NEXT
      stop u < if stop 1 + else stop then
   repeat drop ;

: A-OPEN-RUN ( ^u8 n ^u8 -- ^u8 n ^u8 )
   {: src:^ u:n work:^ :}
   src u work A-SNAPSHOT {: path:^ :}
   path u true A-VALIDATE {: start:n :}
   path work A-OPEN-BASE
   path u start work -1 false A-WALK
   src u work ;
: A-OPEN-AT-RUN ( fd ^u8 n ^u8 -- fd ^u8 n ^u8 )
   {: anchor:fd src:^ u:n work:^ :}
   src u work A-SNAPSHOT {: path:^ :}
   path u false A-VALIDATE {: start:n :}
   path u start work anchor FD>N false A-WALK
   anchor src u work ;

\ The walk's descriptor, its code (0 when it opened) and that code's cause.
: A-DIR-AT ( fd ^u8 n -- n n n ) {: anchor:fd src:^ u:n :}
   COMPILE-READS:MISS
   0 CAUSE!
   A-MAP {: work:^ len:NUM:alloc-byte-len :}
   anchor src u work [: A-OPEN-AT-RUN ;] catch {: code:n :}
   drop 2drop drop
   ERRNO {: cause:n :}
   work A-CUR@ {: opened:n :}
   code 0= if -1 work A-CUR! then
   opened work len code cause A-SETTLE ;

\ TRY-OPEN-DIR-AT's cause for the walk's code: EINVAL for a refused path.
: A-DIR-CAUSE ( n n -- n ) {: code:n cause:n :}
   code E-FS-PATH = code E-FFI-NUL = or if EINVAL exit then
   code E-FS-OPEN <> code E-FS-IO <> and if cause CAUSE! code throw then
   cause ;

: A-REGULAR ( ^u8 -- ) {: work:^ :}
   work A-CUR@ work A-STAT-BUF FSTAT-CALL 0<> if E-FS-STAT ERRNO-FAIL then
   work A-STAT-BUF LIBC-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then ;

: A-READ-STEP ( ^u8 ^u8 n -- n ) {: work:^ dst:^ count:n :}
   begin
      work A-CUR@ dst count A-READ-CALL {: got:n :}
      got 0 >= if got exit then
      FFI:ERRNO dup A-INTERRUPTED = if drop else CAUSE! E-FS-IO throw then
   again ;
: A-READ-CONTENT ( ^u8 SPAN:span<u8> -- n ) {: work:^ dst :}
   dst SPAN:$ {: output:^ cap:n :}
   0 begin dup cap < while
      {: done:n :}
      work output done + cap done - A-READ-STEP {: got:n :}
      got 0= if done exit then
      got cap done - > if E-FS-IO throw then
      done got +
   repeat drop
   work work A-PROBE-BUF 1 A-READ-STEP {: extra:n :}
   extra 0 > if E-FS-CAPACITY throw then
   cap ;
: A-READ-RUN ( fd ^u8 n SPAN:span<u8> ^u8 -- fd ^u8 n SPAN:span<u8> ^u8 )
   {: anchor:fd src:^ u:n dst work:^ :}
   src u work A-SNAPSHOT {: path:^ :}
   path u false A-VALIDATE {: start:n :}
   path u start work anchor FD>N true A-WALK
   work A-REGULAR
   work dst A-READ-CONTENT work A-RESULT-CELL A-SLOT !
   anchor src u dst work ;

public

\ / for an absolute path, . for a relative one; the returned descriptor is the
\ caller's. The AT words borrow their anchor and refuse absolute paths. An
\ empty, negative or over-PATH-CAP path and an empty, repeated, trailing, . or
\ .. component throw E-FS-PATH, a NUL E-FFI-NUL (.cache and a..b are
\ ordinary names). A failed syscall leaves its errno in FS:ERRNO, a refusal 0.
: OPEN-DIR ( ^u8 n -- fd ) {: src:^ u:n :}
   COMPILE-READS:MISS
   0 CAUSE!
   A-MAP {: work:^ len:NUM:alloc-byte-len :}
   src u work [: A-OPEN-RUN ;] catch {: code:n :}
   drop 2drop
   ERRNO {: cause:n :}
   work A-CUR@ {: opened:n :}
   code 0= if -1 work A-CUR! then
   work len code cause A-FINISH
   opened >FD ;
: OPEN-DIR-AT ( fd ^u8 n -- fd )
   A-DIR-AT {: opened:n code:n cause:n :}
   cause CAUSE!
   code 0<> if code throw then
   opened >FD ;
\ OPEN-DIR-AT's walk; none where it throws E-FS-OPEN or E-FS-IO, or a refusal
\ with EINVAL. On Linux a symlink component is ENOTDIR (O_DIRECTORY first).
: TRY-OPEN-DIR-AT ( fd ^u8 n -- option<fd> )
   A-DIR-AT {: opened:n code:n cause:n :}
   code 0= if 0 CAUSE! opened >FD OPTION:SOME exit then
   code cause A-DIR-CAUSE CAUSE!
   OPTION:NONE ;
\ Reads the descriptor fstat found regular (else E-FS-STAT), probing one byte
\ past a full span (excess is E-FS-CAPACITY); the span is unspecified after a
\ throw.
: READ-FILE-AT ( fd ^u8 n SPAN:span<u8> -- n )
   {: anchor:fd src:^ u:n dst :}
   COMPILE-READS:MISS
   0 CAUSE!
   A-MAP {: work:^ len:NUM:alloc-byte-len :}
   anchor src u dst work [: A-READ-RUN ;] catch {: code:n :}
   drop 2drop 2drop
   ERRNO {: cause:n :}
   work A-RESULT-CELL A-SLOT @ {: got:n :}
   work len code cause A-FINISH
   got ;
: CLOSE-DIR ( fd -- )
   0 CAUSE!
   FD>N A-CLOSE-CALL 0<> if E-FS-IO ERRNO-FAIL then ;


HB-TARGET-LINUX-KERNEL? [if]
private
PROCESS-SYMBOLS
FUNCTION: A-LINK-CALL linkat ( n ^u8 n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: A-MKDIR-CALL mkdirat ( n ^u8 n -- i32 ) ;FUNCTION

\ Open flags: include/uapi/asm-generic/fcntl.h; arm64's asm/fcntl.h
\ overrides O_DIRECTORY and O_NOFOLLOW.
2 constant A-RDWR
$80000 constant A-CLOEXEC
$200000 constant A-PATH-ONLY                 \ O_PATH
$400000 constant A-TMPFILE                   \ __O_TMPFILE
$1000 constant A-EMPTY-PATH                  \ AT_EMPTY_PATH
: A-NOFOLLOW ( -- n )
   HB-TARGET-LINUX-X86-64? if $20000 else $8000 then ;
: A-DIRECTORY ( -- n )
   HB-TARGET-LINUX-X86-64? if $10000 else $4000 then ;
: A-ENTRY-FLAGS ( -- n )
   A-PATH-ONLY A-NOFOLLOW or A-CLOEXEC or ;
\ O_TMPFILE is __O_TMPFILE with O_DIRECTORY, and needs write access.
: A-TEMP-FLAGS ( -- n )
   A-TMPFILE A-DIRECTORY or A-RDWR or A-NOFOLLOW or A-CLOEXEC or ;

: A-NAME? ( ^u8 n -- bool ) {: name:^ u:n :}
   u 0 <= u PATH-CAP > or if false exit then
   name u 0 A-COMPONENT-END u <> if false exit then
   name u NUL-AT u <> if false exit then
   name 0 u A-COMPONENT? ;

: A-REFUSE ( -- ) EINVAL CAUSE! ;
: A-DONE ( n -- bool )
   0= if 0 CAUSE! true exit then
   FFI:ERRNO CAUSE! false ;
: A-OPENED ( n -- option<fd> ) {: rc:n :}
   rc 0 < if FFI:ERRNO CAUSE! OPTION:NONE exit then
   0 CAUSE! rc >FD OPTION:SOME ;

: A-OPEN-NAME ( fd ^u8 n n -- option<fd> )
   {: dir:fd name:^ u:n flags:n :}
   COMPILE-READS:MISS
   name u A-NAME? 0= if A-REFUSE OPTION:NONE exit then
   dir FD>N name u >PATHZ flags OPEN-AT-CALL A-OPENED ;
: A-UNLINK ( fd ^u8 n n -- bool ) {: dir:fd name:^ u:n flags:n :}
   name u A-NAME? 0= if A-REFUSE false exit then
   dir FD>N name u >PATHZ flags UNLINK-AT-CALL A-DONE ;

: A-RENAME ( fd ^u8 n fd ^u8 n n -- bool )
   {: from:fd src:^ su:n to:fd dst:^ du:n flags:n :}
   src su A-NAME? dst du A-NAME? and 0= if A-REFUSE false exit then
   from FD>N src su >PATHZ to FD>N dst du >PATHZ2 flags
   RENAMEAT2-CALL A-DONE ;

public
\ Descriptors are borrowed; opens add O_NOFOLLOW|O_CLOEXEC. A name empty, . or
\ .., over PATH-CAP, or with / or NUL is EINVAL. FS:ERRNO: the errno, else 0.

\ O_RDONLY|O_NONBLOCK; a symlink at the name is ELOOP.
: TRY-OPEN-READ-AT ( fd ^u8 n -- option<fd> )
   A-FILE-FLAGS A-OPEN-NAME ;
\ An existing file, O_RDWR|O_NONBLOCK, a symlink ELOOP; nothing is created.
: TRY-OPEN-WRITE-AT ( fd ^u8 n -- option<fd> )
   A-FILE-FLAGS A-RDWR or A-OPEN-NAME ;
\ O_PATH: any kind of entry, a symlink itself included, pinning its inode.
: TRY-OPEN-ENTRY-AT ( fd ^u8 n -- option<fd> )
   A-ENTRY-FLAGS A-OPEN-NAME ;
\ An unnamed regular file of mode 0600 in the directory, which TRY-LINK-AT
\ names. A failure is E-FS-OPEN with its errno in FS:ERRNO.
: TEMP-AT ( fd -- fd ) {: dir:fd :}
   dir FD>N S\" .\z" drop A-TEMP-FLAGS MODE-PRIVATE-FILE OPENAT-CALL
   dup 0 < if drop E-FS-OPEN ERRNO-FAIL then
   0 CAUSE! >FD ;
\ Names the open file in the directory; an existing name is EEXIST.
: TRY-LINK-AT ( fd fd ^u8 n -- bool ) {: file:fd dir:fd name:^ u:n :}
   name u A-NAME? 0= if A-REFUSE false exit then
   file FD>N S\" \z" drop dir FD>N name u >PATHZ
   A-EMPTY-PATH A-LINK-CALL A-DONE ;
\ Source then target; an existing target is replaced.
: TRY-RENAME-AT ( fd ^u8 n fd ^u8 n -- bool )
   0 A-RENAME ;
\ An existing target is EEXIST, both entries kept.
: TRY-RENAME-NEW-AT ( fd ^u8 n fd ^u8 n -- bool )
   NO-REPLACE A-RENAME ;
: TRY-REMOVE-FILE-AT ( fd ^u8 n -- bool )
   0 A-UNLINK ;
\ An empty directory.
: TRY-REMOVE-DIR-AT ( fd ^u8 n -- bool )
   AT-REMOVEDIR A-UNLINK ;
\ The last n is the new directory's mode, before the umask.
: TRY-MAKE-DIR-AT ( fd ^u8 n n -- bool ) {: dir:fd name:^ u:n mode:n :}
   name u A-NAME? 0= if A-REFUSE false exit then
   dir FD>N name u >PATHZ mode A-MKDIR-CALL A-DONE ;
\ The entry itself, never a symlink's target, into the buffer STAT-MODE@
\ and the rest read.
: TRY-STAT-AT ( fd ^u8 n -- bool ) {: dir:fd name:^ u:n :}
   COMPILE-READS:MISS
   name u A-NAME? 0= if A-REFUSE false exit then
   dir FD>N name u >PATHZ STAT-BUF AT-NOFOLLOW STAT-AT-CALL
   A-DONE dup if LIBC>STAT then ;
[then]

;package
