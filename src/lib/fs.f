\ fs.f - checked filesystem helpers.
\ STORAGE CLASS. TASK-LOCAL: the per-call slots, the FS cause, PATHZ-BUF,
\ STAT-BUF, the read probe and ERRNO$'s text are the FS-ABI band of the
\ per-task DATA region. Every data buffer an FS word takes is CALLER-OWNED.
\ Each walk maps a context of its own for one call and releases it on return
\ or throw. WALK-FILES-IF visits regular files depth-first, in the order the
\ directory stream returns (a caller that needs lexical order sorts). Its allow
\ quotation sees every path, the root and directories included, before the walk
\ stats it; false skips the path and all it holds. Below the root the walk skips
\ special files, links stat cannot follow and entries gone since the read; any
\ other entry stat rejects, and a root that is neither a file nor a directory,
\ is E-FS-STAT. The walk closes every descriptor it opened however it ends, and
\ a callback's path is valid only during the callback. Two walks in flight
\ cover disjoint trees: getdirentries64 on a directory removed meanwhile
\ answers -1 and the walk reports E-FS-DIR.

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/adt/option.f
require src/lib/le.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/memory.f
require src/core/compile-reads.f

package FS

public
PATH-CAP 1 + constant PATHZ-CAP

private
EXPORT BOOT:data-base
4096 constant DIR-CAP
256 constant STAT-CAP
64 constant TEXT-CAP
16 constant DIRENT-RECLEN-OFF
18 constant DIRENT-NAMELEN-OFF
21 constant DIRENT-NAME-OFF
19 constant LINUX-DIRENT-NAME-OFF

public
0 constant O-RDONLY
1 constant O-WRONLY
2 constant O-RDWR
4 constant O-NONBLOCK
$8 constant O-APPEND
$200 constant O-CREAT
$400 constant O-TRUNC
420 constant MODE-0644

private
1 constant X-OK
1 constant READ-PROBE-CAP

public
FS-ABI:S-IFMT constant S-IFMT
FS-ABI:S-IFDIR constant S-IFDIR
FS-ABI:S-IFREG constant S-IFREG

private
$1000 constant S-IFIFO
FS-ABI:S-IFLNK constant S-IFLNK

public
$2E constant DOT
$2F constant SLASH

\ Host errnos: macOS sys/errno.h, Linux asm-generic/errno.h on every arch.
1 constant EPERM
2 constant ENOENT
9 constant EBADF
12 constant ENOMEM
13 constant EACCES
17 constant EEXIST
18 constant EXDEV
20 constant ENOTDIR
21 constant EISDIR
22 constant EINVAL
23 constant ENFILE
24 constant EMFILE
28 constant ENOSPC
34 constant ERANGE
: ENAMETOOLONG ( -- n )
   HB-TARGET-MACOS? if 63 else 36 then ;
: ELOOP ( -- n )
   HB-TARGET-MACOS? if 62 else 40 then ;
: ENOTEMPTY ( -- n )
   HB-TARGET-MACOS? if 66 else 39 then ;
: ESTALE ( -- n )
   HB-TARGET-MACOS? if 70 else 116 then ;
: ENODATA ( -- n )
   HB-TARGET-MACOS? if 96 else 61 then ;

\ Read at once after an E-FS-* or E-FS-LIST-* throw from fs.f, fs-list.f, fs-anchored.f or
\ an fs-mutate.f word that says so, or an E-TASK-THREAD from task.f: the throw's host
\ errno, or 0 for a semantic rejection.
: ERRNO ( -- n )
   data-base FS-ABI:ERRNO-OFF + @ ;

\ Sets the errno ERRNO answers.
: CAUSE! ( n -- )
   data-base FS-ABI:ERRNO-OFF + ! ;

private
\ The one fstat binding: libc's struct stat, not the normalized stat64 layout.
PROCESS-SYMBOLS
FUNCTION: FSTAT-CALL fstat ( n ^u8 -- i32 ) ;FUNCTION

\ pread(fd, buf, count, offset), answering ssize_t: bytes read, 0 at end of
\ file, or -1 with errno set.
FUNCTION: PREAD-CALL pread ( n ^u8 n n -- n ) ;FUNCTION

\ The most one pread asks: macOS answers EINVAL to a count above INT_MAX, so a
\ longer range is read over several calls.
$7FFFFFFF constant PREAD-MAX

\ XSI strerror_r is `strerror_r` on macOS and `__xpg_strerror_r` on glibc,
\ whose `strerror_r` is the GNU form answering a pointer. Declaring the other
\ target's symbol is harmless: a row resolves its symbol at its first call.
FUNCTION: STRERROR-CALL strerror_r ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: XPG-STRERROR-CALL __xpg_strerror_r ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: GETEUID-CALL geteuid ( -- u32 ) ;FUNCTION

: STRERROR ( n ^u8 n -- n )
   HB-TARGET-MACOS? if STRERROR-CALL exit then
   XPG-STRERROR-CALL ;

: NUL-AT ( str -- n )
   {: buf:str :}
   buf STR:LEN 0 ?do buf STR:DATA i + c@ 0= if i unloop exit then loop
   buf STR:LEN ;

: TEXT-BUF ( -- ^u8 )
   data-base FS-ABI:TEXT-OFF + BYTE-VIEW ;

public
\ Errno n's host text without its NUL, valid until this task's next ERRNO$;
\ ERRNO unchanged. Text and NUL over TEXT-CAP: E-SPAN-CAPACITY, never cut.
: ERRNO$ ( n -- str )
   {: e:n :}
   TEXT-BUF {: buf:^ :}
   e buf TEXT-CAP STRERROR {: rc:n :}
   rc ERANGE = if E-SPAN-CAPACITY throw then
   buf TEXT-CAP >str NUL-AT {: len:n :}
   \ An unknown errno answers EINVAL with a text the host cuts silently.
   rc EINVAL = if len 1+ TEXT-CAP >= if E-SPAN-CAPACITY throw then then
   buf len >str ;

: CAUSE-CLEAR ( -- )
   0 data-base FS-ABI:ERRNO-OFF + ! ;

\ The effective uid, the one the kernel's file permission checks use.
: EUID ( -- n )
   GETEUID-CALL ;

private
\ A region is a fresh zeroed mapping, so a new task's cells start at zero.
\ layout.f loads before this file and cannot see STAT-CAP, READ-PROBE-CAP,
\ PATHZ-CAP or TEXT-CAP, so BAND-AGREE checks those widths once at load.
: BAND-AGREE ( -- )
   STAT-CAP FS-ABI:STAT-BYTES <> if E-FS-BAND throw then
   READ-PROBE-CAP FS-ABI:PROBE-BYTES <> if E-FS-BAND throw then
   PATHZ-CAP FS-ABI:PATHZ-BYTES <> if E-FS-BAND throw then
   TEXT-CAP FS-ABI:TEXT-BYTES <> if E-FS-BAND throw then ;
BAND-AGREE

: IO-FD ( -- ^n )
   data-base FS-ABI:FD-OFF + ;

: IO-LEN ( -- ^n )
   data-base FS-ABI:LEN-OFF + ;

: IO-RD ( -- ^n )
   data-base FS-ABI:RD-OFF + ;

: IO-OFF ( -- ^n )
   data-base FS-ABI:OFF-OFF + ;

: IO-WR ( -- ^n )
   data-base FS-ABI:WR-OFF + ;

: STAT-BUF ( -- ^u8 )
   data-base FS-ABI:STAT-OFF + BYTE-VIEW ;

: READ-PROBE ( -- ^u8 )
   data-base FS-ABI:PROBE-OFF + BYTE-VIEW ;

: PATHZ-BUF ( -- SPAN:span<u8> )
   data-base FS-ABI:PATHZ-OFF + BYTE-VIEW PATHZ-CAP SPAN:MAKE ;

\ THE HANDLE is WALK-HANDLE-CELLS cells and does not move for the whole call:
\   FRAMES  cell 0   the frames mapping, a ptr-field cell, the one raw storage
\                    that may hold an address
\   CAP     cell 1   the frames the mapping holds
\   DEPTH   cell 2   the depth being walked
\
\ THE FRAMES are one mapping of CAP records, one per depth, FRAME-BYTES each:
\   BASE    cell 0   getdirentries64 base cookie
\   FD      cell 1   the directory descriptor, -1 when closed
\   N       cell 2   bytes the last block read answered
\   OFF     cell 3   offset of the current record in that block
\   REC     cell 4   length of that record
\   PATH-U  cell 5   length of the path in this depth's slot
\   PATH             PATH-CAP bytes, rounded up to a cell: this depth's path
\   DIR              DIR-CAP bytes: this depth's dirent block
\ WALK-FRAMES0 is where the capacity starts, not a bound: ENSURE-DEPTH doubles
\ the mapping before a descent needs a frame past it, and a tree is as deep as
\ PATH-CAP lets its paths be (E-FS-CAPACITY at the join). Growth MOVES the
\ frames, so no helper holds an address into them across a descent. A frame and
\ its slots are narrowings of the whole frames span, so a depth reaches no
\ further than its own record (E-SPAN-RANGE).
0 constant WALK-FRAMES-IDX
1 constant WALK-CAP-CELL
2 constant WALK-DEPTH-CELL
3 constant WALK-HANDLE-CELLS
WALK-HANDLE-CELLS cells constant WALK-HANDLE-BYTES
8 constant WALK-FRAMES0
0 constant FRAME-BASE-CELL
1 constant FRAME-FD-CELL
2 constant FRAME-N-CELL
3 constant FRAME-OFF-CELL
4 constant FRAME-REC-CELL
5 constant FRAME-PATH-U-CELL
6 cells constant FRAME-PATH-OFF
PATH-CAP 1 cells + 1 - 1 cells / cells constant FRAME-PATH-BYTES
FRAME-PATH-OFF FRAME-PATH-BYTES + constant FRAME-DIR-OFF
FRAME-DIR-OFF DIR-CAP + constant FRAME-BYTES

: WALK-FRAMES ( ^n -- ^^u8 )
   WALK-FRAMES-IDX ptr-field ;

: WALK-CAP ( ^n -- ^n )
   WALK-CAP-CELL cells + ;

: WALK-DEPTH@ ( ^n -- n )
   WALK-DEPTH-CELL cells + @ ;

: WALK-DEPTH! ( n ^n -- )
   WALK-DEPTH-CELL cells + ! ;

: WALK-SPAN ( ^n -- SPAN:span<u8> ) {: ctx:^ :}
   ctx WALK-FRAMES @ ctx WALK-CAP @ FRAME-BYTES * SPAN:MAKE ;

: FRAME ( ^n n -- SPAN:span<u8> ) {: ctx:^ d :}
   ctx WALK-SPAN d FRAME-BYTES * FRAME-BYTES SPAN:SUB ;

: FRAME-FIELD ( ^n n n -- ^n ) {: ctx:^ d k :}
   ctx d FRAME SPAN:$ STR:DATA CELL-VIEW k cells + ;

: CUR-FIELD ( ^n n -- ^n ) {: ctx:^ k :}
   ctx ctx WALK-DEPTH@ k FRAME-FIELD ;

: PATH-SLOT ( ^n n -- SPAN:span<u8> )
   FRAME FRAME-PATH-OFF PATH-CAP SPAN:SUB ;

: PATH-U ( ^n n -- ^n )
   FRAME-PATH-U-CELL FRAME-FIELD ;

: CUR-PATH$ ( ^n -- str ) {: ctx:^ :}
   ctx WALK-DEPTH@ {: d :}
   ctx d PATH-SLOT ctx d PATH-U @ SPAN:TAKE SPAN:$ ;

: CUR-DIR ( ^n -- SPAN:span<u8> ) {: ctx:^ :}
   ctx ctx WALK-DEPTH@ FRAME FRAME-DIR-OFF DIR-CAP SPAN:SUB ;

: BASE@ ( ^n -- ^n )
   FRAME-BASE-CELL CUR-FIELD ;

: FD-PTR ( ^n n -- ^n )
   FRAME-FD-CELL FRAME-FIELD ;

: FD@ ( ^n -- n ) {: ctx:^ :}
   ctx ctx WALK-DEPTH@ FD-PTR @ ;

: FD! ( n ^n -- ) {: fd ctx:^ :}
   fd ctx ctx WALK-DEPTH@ FD-PTR ! ;

: FD-CLEAR-AT ( ^n n -- ) {: ctx:^ d :}
   -1 ctx d FD-PTR ! ;

\ A fresh mapping is zeroed, and zero is a descriptor: every frame a mapping
\ adds is marked closed before the walk's depth can reach it.
: FDS-CLEAR ( ^n n n -- ) {: ctx:^ from to :}
   from begin dup to < while
      ctx over FD-CLEAR-AT
      1+
   repeat drop ;

: CLOSE-FD-AT ( ^n n -- ) {: ctx:^ d :}
   ctx d FD-PTR @ dup 0 >= if
      close ctx d FD-CLEAR-AT
   else
      drop
   then ;

\ The one cleanup site: WALK-UNMAP runs it whether the walk ended or threw.
: CLOSE-WALK ( ^n -- ) {: ctx:^ :}
   ctx WALK-DEPTH@ begin dup 0 >= while
      ctx over CLOSE-FD-AT
      1-
   repeat drop ;

: N@ ( ^n -- n )
   FRAME-N-CELL CUR-FIELD @ ;

: N! ( n ^n -- )
   FRAME-N-CELL CUR-FIELD ! ;

: OFF@ ( ^n -- n )
   FRAME-OFF-CELL CUR-FIELD @ ;

: OFF! ( n ^n -- )
   FRAME-OFF-CELL CUR-FIELD ! ;

: REC@ ( ^n -- n )
   FRAME-REC-CELL CUR-FIELD @ ;

: REC! ( n ^n -- )
   FRAME-REC-CELL CUR-FIELD ! ;

\ Nothing else is allocated yet, so a failed mapping leaves nothing to release.
: WALK-MAP ( ^n -- ) {: ctx:^ :}
   WALK-FRAMES0 FRAME-BYTES * MEM-ALLOC-BYTES drop ctx WALK-FRAMES !
   WALK-FRAMES0 ctx WALK-CAP !
   0 ctx WALK-DEPTH!
   ctx 0 WALK-FRAMES0 FDS-CLEAR ;

: WALK-UNMAP ( ^n -- ) {: ctx:^ :}
   ctx CLOSE-WALK
   ctx WALK-SPAN MEM:FREE-SPAN ;

\ Make frame d exist before a descent writes it; a descent asks for one frame
\ past the deepest, so doubling always suffices. The handle names the new
\ mapping first and its new frames are closed before the depth reaches them; a
\ failed mapping leaves the walk on its old frames, where CLOSE-WALK still
\ finds every descriptor.
: ENSURE-DEPTH ( ^n n -- ) {: ctx:^ d :}
   ctx WALK-CAP @ {: cap :}
   d cap < if exit then
   cap 2 * {: grown :}
   grown FRAME-BYTES * MEM-ALLOC-BYTES SPAN:MAKE {: fresh :}
   ctx WALK-SPAN {: old :}
   old SPAN:$ fresh SPAN:COPY
   fresh SPAN:$ STR:DATA ctx WALK-FRAMES !
   grown ctx WALK-CAP !
   ctx cap grown FDS-CLEAR
   old MEM:FREE-SPAN ;

public
\ FS owns the length gate: a caller's path longer than PATH-CAP, stored or
\ opened as given, is E-FS-PATH, the rejection every fs consumer tests for.
\ FFI:CSTR owns the copy, the terminator and the 0-byte rejection (E-FFI-NUL),
\ bounded by the destination's reach.
: PATHZ-INTO ( str SPAN:span<u8> -- ^u8 )
   {: s:str dst :}
   CAUSE-CLEAR
   s STR:LEN 0 < if E-FS-PATH throw then
   s STR:LEN PATH-CAP > if E-FS-PATH throw then
   s dst FFI:CSTR ;

: >PATHZ ( str -- ^u8 )
   PATHZ-BUF PATHZ-INTO ;

\ Whether >PATHZ can spell the path: 0 to PATH-CAP bytes, none of them 0.
: PATHZ? ( str -- bool ) {: s:str :}
   s STR:LEN 0 < if false exit then
   s STR:LEN PATH-CAP > if false exit then
   s 0 STR:INDEX-OF MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH ;

private
\ >PATHZ of a path whose file a word reads, the read recorded
\ (COMPILE-READS:FILE+).
: READ-PATHZ ( str -- ^u8 ) {: s:str :}
   s >PATHZ {: z:^ :}
   s z COMPILE-READS:FILE+
   z ;

\ >PATHZ of a path whose final symlink a word reads, not its target, the read
\ recorded.
: READ-LPATHZ ( str -- ^u8 ) {: s:str :}
   s >PATHZ {: z:^ :}
   s z COMPILE-READS:LFILE+
   z ;

\ Whether stat answers for the NUL-terminated path, into STAT-BUF; it records no
\ read.
: STAT-Z? ( ^u8 -- bool )
   STAT-BUF stat64 0 < if false exit then
   true ;

\ Every target's struct stat holds st_ino at 8 as 64 bits and st_dev from 0,
\ where the stat64 rows leave them; Linux's st_dev keeps the kernel's 32-bit
\ encoding in the low half, which is all of it the rows leave.
8 constant STAT-INO-OFF
0 constant STAT-DEV-OFF

public
: EXISTS? ( str -- bool )
   READ-PATHZ 0 access 0= ;

: STAT-MODE@ ( -- n )
   STAT-BUF FS-ABI:STAT-MODE-OFF + LE:U16@ ;

: STAT-SIZE@ ( -- n )
   STAT-BUF FS-ABI:STAT-SIZE-OFF + LE:U64@ ;

: STAT-MTIME-SEC@ ( -- n )
   STAT-BUF FS-ABI:STAT-MTIME-SEC-OFF + LE:U64@ ;

: STAT-MTIME-NS@ ( -- n )
   STAT-BUF FS-ABI:STAT-MTIME-NS-OFF + LE:U64@ ;

: STAT-CTIME-SEC@ ( -- n )
   STAT-BUF FS-ABI:STAT-CTIME-SEC-OFF + LE:U64@ ;

: STAT-CTIME-NS@ ( -- n )
   STAT-BUF FS-ABI:STAT-CTIME-NS-OFF + LE:U64@ ;

: STAT-INO@ ( -- n )
   STAT-BUF STAT-INO-OFF + LE:U64@ ;

: STAT-DEV@ ( -- n )
   STAT-BUF STAT-DEV-OFF + LE:U32@ ;

: TRY-STAT ( str -- bool ) READ-PATHZ STAT-Z? ;

: TRY-LSTAT ( str -- bool )
   READ-LPATHZ STAT-BUF lstat64 0 < if false exit then
   true ;

private
\ fstatat(2): the NUL-terminated name relative to a directory descriptor (or
\ AT-CWD) into a buffer of STAT-CAP bytes, in libc's struct stat layout,
\ not lstat64's, so its mode is LIBC-MODE@'s to read.
PROCESS-SYMBOLS
FUNCTION: STAT-AT-CALL fstatat ( n ^u8 ^u8 n -- i32 ) ;FUNCTION

\ AT_SYMLINK_NOFOLLOW, fstatat's flag for the link itself, never its target.
\ Darwin: xnu bsd/sys/fcntl.h. Linux: include/uapi/linux/fcntl.h, every arch.
: AT-NOFOLLOW ( -- n )
   HB-TARGET-MACOS? if $20 else $100 then ;

\ The mode field of libc's struct stat. STAT-MODE@ reads lstat64's layout
\ instead, whose mode sits elsewhere on Linux.
: LIBC-MODE@ ( ^u8 -- n )
   HB-TARGET-MACOS? if 4 + LE:U16@ exit then
   HB-TARGET-LINUX-X86-64? if 24 + LE:U16@ exit then
   16 + LE:U16@ ;

: TRY-STAT-MODE ( str -- option<n> )
   TRY-STAT if STAT-MODE@ OPTION:SOME else OPTION:NONE then ;

: TRY-LSTAT-MODE ( str -- option<n> )
   TRY-LSTAT if STAT-MODE@ OPTION:SOME else OPTION:NONE then ;

public
: STAT-MODE ( str -- n )
   TRY-STAT-MODE MATCH option
     none OF E-FS-STAT throw ENDOF
     some OF ENDOF
   ;MATCH ;

: FILE-SIZE ( str -- n )
   TRY-STAT 0= if E-FS-STAT throw then
   STAT-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then
   STAT-SIZE@ ;

\ The regular file at the path: size, modification time seconds and
\ nanoseconds, change time seconds and nanoseconds, then true; five zeros and
\ false when stat fails (the file is missing or a directory above it cannot be
\ searched) or it is not a regular file. A malformed path
\ throws, as FILE-META's does: E-FS-PATH, or E-FFI-NUL for a 0 byte.
: TRY-FILE-META ( str -- n n n n n bool )
   TRY-STAT 0= if 0 0 0 0 0 false exit then
   STAT-MODE@ S-IFMT and S-IFREG <> if 0 0 0 0 0 false exit then
   STAT-SIZE@ STAT-MTIME-SEC@ STAT-MTIME-NS@
   STAT-CTIME-SEC@ STAT-CTIME-NS@ true ;

: FILE-META ( str -- n n n n n )
   TRY-FILE-META 0= if E-FS-STAT throw then ;

: FILE? ( str -- bool )
   TRY-STAT-MODE MATCH option
     none OF false ENDOF
     some OF S-IFMT and S-IFREG = ENDOF
   ;MATCH ;

: DIR? ( str -- bool )
   TRY-STAT-MODE MATCH option
     none OF false ENDOF
     some OF S-IFMT and S-IFDIR = ENDOF
   ;MATCH ;

: FIFO? ( str -- bool )
   TRY-STAT-MODE MATCH option
     none OF false ENDOF
     some OF S-IFMT and S-IFIFO = ENDOF
   ;MATCH ;

: SYMLINK? ( str -- bool )
   TRY-LSTAT-MODE MATCH option
     none OF false ENDOF
     some OF S-IFMT and S-IFLNK = ENDOF
   ;MATCH ;

: EXECUTABLE? ( str -- bool )
   READ-PATHZ X-OK access 0= ;

: BASENAME ( str -- str ) {: s:str :}
   s STR:LEN begin dup 0 > while
      dup 1 - s STR:DATA + c@ SLASH = if
         s STR:DATA over + s STR:LEN rot - >str exit
      then
      1-
   repeat
   drop s ;

\ A parent that already ends in a separator takes no second one; the measure and
\ the write share this one answer.
: JOIN-SEPARATED? ( str -- bool ) {: ps:str :}
   ps STR:LEN 0 <= if false exit then
   ps STR:DATA ps STR:LEN 1 - + c@ SLASH = ;

private
: JOIN-LEN ( str n -- n ) {: ps:str nu :}
   ps JOIN-SEPARATED? if ps STR:LEN nu + exit then
   ps STR:LEN 1 + nu + ;

public
\ A join that does not fit dst is E-FS-CAPACITY, with nothing written. Each
\ length meets dst's on its own first, so a huge one cannot wrap the sum.
: JOIN-PATH-INTO ( str str SPAN:span<u8> -- n ) {: ps:str ns:str dst :}
   CAUSE-CLEAR
   ps STR:LEN 0 < if E-FS-PATH throw then
   ns STR:LEN 0 < if E-FS-PATH throw then
   dst SPAN:LEN {: cap :}
   ps STR:LEN cap > ns STR:LEN cap > or if E-FS-CAPACITY throw then
   ps ns STR:LEN JOIN-LEN cap > if E-FS-CAPACITY throw then
   ps dst SPAN:COPY
   ps JOIN-SEPARATED? if
      ns dst ps STR:LEN SPAN:SKIP SPAN:COPY
      ps STR:LEN ns STR:LEN + exit
   then
   SLASH dst ps STR:LEN SPAN:U8!
   ns dst ps STR:LEN 1 + SPAN:SKIP SPAN:COPY
   ps STR:LEN 1 + ns STR:LEN + ;

\ dst holds PATH-CAP bytes: the reach minted here is the caller's promise.
: JOIN-PATH ( str str ^u8 -- n )
   PATH-CAP SPAN:MAKE JOIN-PATH-INTO ;

private
: PATH= ( str str -- bool )
   STR:= ;

: IO-FAIL ( -- )
   IO-FD @ close E-FS-IO throw ;

\ A FUNCTION: syscall saves no cause of its own.
: ERRNO-FAIL ( n -- )
   FFI:ERRNO data-base FS-ABI:ERRNO-OFF + ! throw ;

\ Throw n1 with n2 as this task's FS:ERRNO: a cause read before cleanup whose
\ own primitives replaced it.
: CAUSE-FAIL ( n n -- )
   data-base FS-ABI:ERRNO-OFF + ! throw ;

: READ-FILL ( ^u8 n -- n ) {: dst:^ cap :}
   0 IO-LEN !
   begin IO-LEN @ cap < while
      IO-FD @ dst IO-LEN @ + cap IO-LEN @ - read IO-RD !
      IO-RD @ 0 < if IO-FAIL then
      IO-RD @ cap IO-LEN @ - > if IO-FAIL then
      IO-RD @ 0= if IO-LEN @ exit then
      IO-LEN @ IO-RD @ + IO-LEN !
   repeat
   IO-LEN @ ;

: READ-MORE? ( -- bool )
   IO-FD @ READ-PROBE READ-PROBE-CAP read IO-RD !
   IO-RD @ 0 < if IO-FAIL then
   IO-RD @ 0 > ;

public
: READ-ALL ( str ^u8 n -- n ) {: ps:str dst:^ cap :}
   cap 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   ps READ-PATHZ open-rd IO-FD !
   IO-FD @ 0 < if E-FS-OPEN throw then
   dst cap READ-FILL {: got :}
   got cap = if READ-MORE? if IO-FD @ close E-FS-CAPACITY throw then then
   IO-FD @ close
   got ;

private
\ Close file, then throw E-FS-IO with cause as this task's FS:ERRNO.
: PREAD-FAIL ( n n -- )
   {: file cause :}
   file close
   E-FS-IO cause CAUSE-FAIL ;

\ One pread of at most PREAD-MAX bytes of the rest of dst[0,len) into dst+done,
\ from off+done; answers done grown by the bytes it read. A failed pread is
\ E-FS-IO with its errno; an end of file before len, or an answer past the
\ count asked, is E-FS-IO with no cause. Each throw closes file first.
: PREAD-STEP ( n n n ^u8 n -- n )
   {: done file off dst:^ len :}
   len done - PREAD-MAX min {: want :}
   file dst done + want off done + PREAD-CALL {: got :}
   got 0 < if file FFI:ERRNO PREAD-FAIL then
   got 0= got want > or if file 0 PREAD-FAIL then
   done got + ;

: PREAD-FILL ( n n ^u8 n -- )
   {: file off dst:^ len :}
   0 begin dup len < while file off dst len PREAD-STEP repeat drop ;

public
\ Read exactly len bytes of the file at the path, from byte offset off, into
\ dst, which holds at least len. The open is READ-ALL's: a failed one is
\ E-FS-OPEN. pread reads on until len bytes are in dst; an end of file
\ before then is E-FS-IO with no cause, a failed pread E-FS-IO with its
\ errno. A negative off or len is E-FS-CAPACITY with no cause; a len of 0
\ reads nothing once the open succeeds. The descriptor is closed on every
\ path. Its state is locals and the task's path buffer, so tasks may call it
\ at once.
: READ-AT ( str n ^u8 n -- )
   {: ps:str off dst:^ len :}
   off 0 < len 0 < or if CAUSE-CLEAR E-FS-CAPACITY throw then
   ps READ-PATHZ open-rd {: file :}
   file 0 < if E-FS-OPEN throw then
   file off dst len PREAD-FILL
   file close ;

private
\ The size field of libc's struct stat.
: LIBC-SIZE@ ( ^u8 -- n )
   HB-TARGET-MACOS? if 96 + LE:U64@ exit then
   48 + LE:U64@ ;

\ The change time of libc's struct stat, seconds then nanoseconds.
: LIBC-CTIME@ ( ^u8 -- n n ) {: buf:^ :}
   HB-TARGET-MACOS? if buf 64 + LE:U64@ buf 72 + LE:U64@ exit then
   buf 104 + LE:U64@ buf 112 + LE:U64@ ;

\ The link count of libc's struct stat; the last branch is the generic layout
\ Linux arm64 uses.
: LIBC-NLINK@ ( ^u8 -- n )
   HB-TARGET-MACOS? if 6 + LE:U16@ exit then
   HB-TARGET-LINUX-X86-64? if 16 + LE:U64@ exit then
   20 + LE:U32@ ;

: FSTAT-FAIL ( n n -- ) {: file cause :}
   file close
   cause CAUSE!
   E-FS-STAT throw ;

\ fstat into STAT-BUF. A failure is E-FS-STAT with errno its cause, and the
\ descriptor stays open.
: FD-FSTAT ( fd -- ) {: file:fd :}
   file FD>N STAT-BUF FSTAT-CALL
   0<> if FFI:ERRNO CAUSE! E-FS-STAT throw then ;

\ The modification time of libc's struct stat, seconds then nanoseconds.
: LIBC-MTIME@ ( ^u8 -- n n ) {: buf:^ :}
   HB-TARGET-MACOS? if buf 48 + LE:U64@ buf 56 + LE:U64@ exit then
   buf 88 + LE:U64@ buf 96 + LE:U64@ ;

\ Rewrites libc's struct stat in STAT-BUF into the layout the stat64 rows
\ leave, which macOS's struct stat already is. Every field is read before one
\ is written: on Linux the size and the modification time trade places.
: LIBC>STAT ( -- )
   HB-TARGET-MACOS? if exit then
   STAT-BUF {: buf:^ :}
   buf LIBC-MODE@ buf LIBC-SIZE@ buf LIBC-MTIME@ buf LIBC-CTIME@
   {: mode size msec mns csec cns :}
   mode buf FS-ABI:STAT-MODE-OFF + LE:U32!
   size buf FS-ABI:STAT-SIZE-OFF + LE:U64!
   msec buf FS-ABI:STAT-MTIME-SEC-OFF + LE:U64!
   mns buf FS-ABI:STAT-MTIME-NS-OFF + LE:U64!
   csec buf FS-ABI:STAT-CTIME-SEC-OFF + LE:U64!
   cns buf FS-ABI:STAT-CTIME-NS-OFF + LE:U64! ;

\ fpathconf(2) and its _PC_NAME_MAX: Darwin sys/unistd.h, glibc
\ bits/confname.h.
PROCESS-SYMBOLS
FUNCTION: PATHCONF-CALL fpathconf ( n n -- n ) ;FUNCTION
: PC-NAME-MAX ( -- n )
   HB-TARGET-MACOS? if 4 else 3 then ;

: NAME-MAX-FAIL ( n -- ) {: limit:n :}
   limit 0 < if FFI:ERRNO else 0 then CAUSE!
   E-FS-STAT throw ;

public
\ Open a regular file for reading once and size it from that descriptor, so a
\ writer renaming a new file over the path or a remover unlinking it cannot
\ change what READ-SIZED reads. A missing file (ENOENT) is `fd 0 false` with no
\ descriptor open; any other open failure is E-FS-OPEN, and a descriptor that
\ is not a regular file is closed and E-FS-STAT. The open does not block: a
\ FIFO at the path opens at once and is E-FS-STAT, as any file not regular is.
\ A true flag hands the caller an open descriptor that READ-SIZED closes.
: OPEN-SIZED ( str -- fd n bool ) {: ps:str :}
   ps READ-PATHZ O-NONBLOCK 0 open {: file :}
   file 0 < if
      ERRNO ENOENT = if -1 >FD 0 false exit then
      E-FS-OPEN throw
   then
   file STAT-BUF FSTAT-CALL 0<> if file FFI:ERRNO FSTAT-FAIL then
   STAT-BUF LIBC-MODE@ S-IFMT and S-IFREG <> if file 0 FSTAT-FAIL then
   file >FD STAT-BUF LIBC-SIZE@ true ;

\ Read exactly the size OPEN-SIZED gave into dst, which holds at least that
\ many bytes, then close the descriptor. Fewer bytes or more than that size
\ mean the open file changed underneath: E-FS-IO, the descriptor closed.
: READ-SIZED ( fd ^u8 n -- ) {: file:fd dst:^ size :}
   file FD>N IO-FD !
   dst size READ-FILL size <> if IO-FAIL then
   READ-MORE? if IO-FAIL then
   IO-FD @ close ;

\ The open descriptor's file: change time, seconds then nanoseconds, and size.
: FD-STAMP ( fd -- n n n )
   FD-FSTAT STAT-BUF LIBC-CTIME@  STAT-BUF LIBC-SIZE@ ;

\ The open descriptor's file's link count, 0 once every name of it is removed.
: FD-LINKS ( fd -- n )
   FD-FSTAT STAT-BUF LIBC-NLINK@ ;

\ fstat into the buffer the STAT- accessors read. A failure is E-FS-STAT with
\ errno its cause, and the descriptor stays open.
: FD-STAT ( fd -- )
   FD-FSTAT LIBC>STAT ;

\ The longest name in bytes the descriptor's file system takes; the descriptor
\ stays open. E-FS-STAT with its errno, or with 0 for no limit or a zero one.
: FD-NAME-MAX ( fd -- n ) {: file:fd :}
   FFI:ERRNO-CLEAR
   file FD>N PC-NAME-MAX PATHCONF-CALL {: limit:n :}
   limit 0 <= if limit NAME-MAX-FAIL then
   limit ;

private
: WRITE-BY-FLAGS ( str str n -- ) {: ps:str src:str flags :}
   src STR:LEN 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   ps EXISTS? if ps FILE? 0= if E-FS-OPEN throw then then
   ps >PATHZ flags MODE-0644 open IO-FD !
   IO-FD @ 0 < if E-FS-OPEN throw then
   ps FILE? 0= if IO-FD @ close E-FS-OPEN throw then
   0 IO-OFF !
   begin IO-OFF @ src STR:LEN < while
      IO-FD @ src STR:DATA IO-OFF @ + src STR:LEN IO-OFF @ - >str write IO-WR !
      IO-WR @ 0 <= if IO-FD @ close E-FS-IO throw then
      IO-WR @ src STR:LEN IO-OFF @ - > if IO-FD @ close E-FS-IO throw then
      IO-OFF @ IO-WR @ + IO-OFF !
   repeat
   IO-FD @ close ;

public
: WRITE-ALL ( str str -- )
   O-WRONLY O-CREAT or O-TRUNC or WRITE-BY-FLAGS ;

: APPEND-FILE ( str str -- )
   O-WRONLY O-CREAT or O-APPEND or WRITE-BY-FLAGS ;

: OPEN-APPEND-FD ( str -- n ) {: ps:str :}
   ps EXISTS? if ps FILE? 0= if E-FS-OPEN throw then then
   ps >PATHZ O-WRONLY O-CREAT or O-APPEND or MODE-0644 open {: fd :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

private
: DOT-ENTRY? ( str -- bool ) {: s:str :}
   s STR:LEN 1 = if s STR:DATA c@ DOT = else false then ;

: DOTDOT-ENTRY? ( str -- bool ) {: s:str :}
   s STR:LEN 2 = if s STR:DATA c@ DOT = s STR:DATA 1 + c@ DOT = and else false then ;

public
: SKIP-SELF-ENTRY? ( str -- bool )
   dup DOT-ENTRY? if drop true exit then
   DOTDOT-ENTRY? ;

: OPEN-DIR-READ ( str -- n )
   READ-PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then ;

private
\ openat(2) and unlinkat(2): the NUL-terminated name relative to a directory
\ descriptor, or to the working directory for AT-CWD.
PROCESS-SYMBOLS
FUNCTION: OPEN-AT-CALL openat ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: UNLINK-AT-CALL unlinkat ( n ^u8 n -- i32 ) ;FUNCTION

\ AT_FDCWD, the descriptor that names the working directory.
: AT-CWD ( -- n )
   HB-TARGET-MACOS? if -2 else -100 then ;

\ AT_REMOVEDIR, unlinkat's flag to remove an empty directory as rmdir does.
\ Darwin: xnu bsd/sys/fcntl.h. Linux: include/uapi/linux/fcntl.h, every arch.
: AT-REMOVEDIR ( -- n )
   HB-TARGET-MACOS? if $80 else $200 then ;

public
\ openat's flags for a directory: O_RDONLY|O_DIRECTORY|O_NOFOLLOW|O_CLOEXEC,
\ so a symlink in the last component is rejected, never followed. Darwin: xnu
\ bsd/sys/fcntl.h. Linux arm64: arch/arm64 asm/fcntl.h overrides the generic
\ directory/nofollow bits; x86-64 uses the generic bits.
: OPEN-DIR-FLAGS ( -- n )
   HB-TARGET-MACOS? if $100000 $100 or $1000000 or exit then
   HB-TARGET-LINUX-X86-64? if $10000 $20000 or $80000 or exit then
   $4000 $8000 or $80000 or ;

private

\ openat's flags for a directory used only as later calls' directory, which
\ needs search permission, not read: OPEN-DIR-FLAGS with Darwin's O_SEARCH
\ (O_EXEC $40000000 with O_DIRECTORY, xnu bsd/sys/fcntl.h) or Linux's O_PATH
\ $200000 (include/uapi/asm-generic/fcntl.h, which arm64 does not override).
: SEARCH-DIR-FLAGS ( -- n )
   HB-TARGET-MACOS? if $40000000 OPEN-DIR-FLAGS or exit then
   $200000 OPEN-DIR-FLAGS or ;

public
: DIRENT-RECLEN ( ^u8 -- n )
   DIRENT-RECLEN-OFF BYTE+ LE:U16@ ;

private
: TARGET-UNKNOWN ( -- )
   CAUSE-CLEAR E-FS-DIR throw ;

public
: DIRENT-NAME-OFFSET ( -- n )
   HB-TARGET-LINUX? if
      LINUX-DIRENT-NAME-OFF exit
   then
   HB-TARGET-MACOS? if
      DIRENT-NAME-OFF exit
   then
   TARGET-UNKNOWN ;

\ Keep the kernel byte, including zero for DT_UNKNOWN.
: DIRENT-TYPE ( ^u8 -- u8 )
   HB-TARGET-LINUX? if 18 + c@ exit then
   HB-TARGET-MACOS? if 20 + c@ exit then
   TARGET-UNKNOWN ;

private
\ A malformed record throws plainly here: closing descriptors belongs to
\ whoever opened them.
: LINUX-DIRENT-NAMELEN-SCAN ( str -- n ) {: ent:str :}
   0 begin dup LINUX-DIRENT-NAME-OFF + ent STR:LEN < while
      ent STR:DATA over LINUX-DIRENT-NAME-OFF + BYTE@ 0= if exit then
      1+
   repeat
   CAUSE-CLEAR E-FS-DIR throw ;

: LINUX-DIRENT-NAMELEN ( ^u8 -- n ) {: ent:^ :}
   ent ent DIRENT-RECLEN >str LINUX-DIRENT-NAMELEN-SCAN ;

: DIRENT-NAMELEN ( ^u8 -- n )
   HB-TARGET-LINUX? if LINUX-DIRENT-NAMELEN exit then
   HB-TARGET-MACOS? if DIRENT-NAMELEN-OFF BYTE+ LE:U16@ exit then
   TARGET-UNKNOWN ;

public
: DIRENT-NAME ( ^u8 -- str )
   dup DIRENT-NAME-OFFSET BYTE+ swap DIRENT-NAMELEN >str ;

private
: DIRENT-NAME-END ( ^u8 -- n ) {: ent:^ :}
   DIRENT-NAME-OFFSET ent DIRENT-NAMELEN + ;

: CHECK-RECORD ( ^n ^u8 -- ) {: ctx:^ ent:^ :}
   ctx REC@ 0 <= if CAUSE-CLEAR E-FS-DIR throw then
   ctx OFF@ ctx REC@ + ctx N@ > if CAUSE-CLEAR E-FS-DIR throw then
   ent DIRENT-NAME-END ctx REC@ > if
      CAUSE-CLEAR E-FS-DIR throw
   then ;

: READ-DIR ( ^n -- bool ) {: ctx:^ :}
   ctx FD@ ctx CUR-DIR ctx BASE@ getdirentries64
   dup 0 < if drop E-FS-DIR throw then
   dup ctx N! 0 > ;

\ The root reaches the depth-0 slot before any >PATHZ judges it, so the length
\ gate's rejection is made here.
: WALK-ROOT! ( ^n str -- ) {: ctx:^ s:str :}
   CAUSE-CLEAR
   s STR:LEN 0 < if E-FS-PATH throw then
   s STR:LEN PATH-CAP > if E-FS-PATH throw then
   s ctx 0 PATH-SLOT SPAN:COPY
   s STR:LEN ctx 0 PATH-U ! ;

: OPEN-WALK-DIR ( ^n str -- ) {: ctx:^ s:str :}
   s OPEN-DIR-READ ctx FD!
   0 ctx BASE@ ! ;

: CLOSE-CUR-DIR ( ^n -- ) {: ctx:^ :}
   ctx FD@ close
   -1 ctx FD! ;

: DIR-BLOCK-BEGIN ( ^n -- )
   0 swap OFF! ;

: DIR-MORE? ( ^n -- bool ) {: ctx:^ :}
   ctx OFF@ ctx N@ < ;

\ The block offset is a running sum of kernel-supplied record lengths, so the
\ entry address is taken through the dir span: an offset past the block is
\ E-SPAN-RANGE. The cursor is the answer, not a context cell: an address never
\ lands in raw storage.
: CUR-ENTRY ( ^n -- ^u8 ) {: ctx:^ :}
   ctx CUR-DIR ctx OFF@ SPAN:AT ;

: LOAD-ENTRY ( ^n -- ^u8 ) {: ctx:^ :}
   ctx CUR-ENTRY {: ent:^ :}
   ent DIRENT-RECLEN ctx REC!
   ctx ent CHECK-RECORD
   ent ;

: ADVANCE-ENTRY ( ^n -- ) {: ctx:^ :}
   ctx OFF@ ctx REC@ + ctx OFF! ;

\ The child frame exists before anything is read, because making it may move
\ the frames: the parent path and the entry name come from the frames as they
\ stand after ENSURE-DEPTH.
: DESCEND-PATH ( ^n -- ^n str ) {: ctx:^ :}
   ctx WALK-DEPTH@ 1 + {: d :}
   ctx d ENSURE-DEPTH
   ctx CUR-PATH$ {: pa:str :}
   ctx CUR-ENTRY DIRENT-NAME {: na:str :}
   pa na ctx d PATH-SLOT JOIN-PATH-INTO ctx d PATH-U !
   d ctx WALK-DEPTH!
   ctx ctx CUR-PATH$ ;

: ASCEND-PATH ( ^n -- ) {: ctx:^ :}
   ctx WALK-DEPTH@ 1 - ctx WALK-DEPTH! ;

\ The entry's S-IFREG or S-IFDIR, or 0 for one below the root the walk skips.
\ stat leaves errno in ERRNO's slot, not libc's; SYMLINK?'s lstat overwrites it.
: WALK-TYPE ( ^n str -- n ) {: ctx:^ s:str :}
   s TRY-STAT-MODE MATCH option
     none OF -1 ENDOF
     some OF S-IFMT and ENDOF
   ;MATCH ERRNO {: fmt e :}
   fmt S-IFREG = fmt S-IFDIR = or if fmt exit then
   ctx WALK-DEPTH@ 0<> if
      fmt 0 >= if 0 exit then
      e ENOENT = if 0 exit then
      s SYMLINK? if 0 exit then
   then
   fmt 0< if e CAUSE! then
   E-FS-STAT throw ;

\ The callback is executed straight off the stack: it needs no cell, and its
\ throw needs no catch here because WALK-UNMAP closes this context's
\ descriptors for every way out of WALK-RUN. The path `a u` lies in the
\ frames, which a descent may move, so it is read only before the first one;
\ the loop carries nothing but the handle across its recursion.
: WALK-PATH ( ^n str [ str -- bool ] [ str -- ] -- )
   {: ctx:^ s:str allow q :}
   s allow execute 0= if exit then
   ctx s WALK-TYPE {: fmt :}
   fmt S-IFREG = if s q execute exit then
   fmt S-IFDIR <> if exit then
   ctx s OPEN-WALK-DIR
   begin ctx READ-DIR while
      ctx DIR-BLOCK-BEGIN
      begin ctx DIR-MORE? while
         ctx LOAD-ENTRY DIRENT-NAME SKIP-SELF-ENTRY? 0= if
            ctx DESCEND-PATH allow q recurse
            ctx ASCEND-PATH
         then
         ctx ADVANCE-ENTRY
      repeat
   repeat
   ctx CLOSE-CUR-DIR ;

\ A quotation-typed LOCAL cannot be caught - the checker rejects
\ `{: a u q :} a u q catch` - so the walk travels on the stack into this
\ preserving word and the literal quotation below catches THAT.
: WALK-RUN ( ^n str [ str -- bool ] [ str -- ] -- ^n str [ str -- bool ] [ str -- ] )
   {: ctx:^ s:str allow q :}
   ctx s WALK-ROOT!
   ctx ctx CUR-PATH$ allow q WALK-PATH
   ctx s allow q ;

\ The WITH-BYTES body: the handle is that scope's bytes, so WITH-BYTES frees it
\ however the walk ends, and the frames are mapped and released here, around
\ the one catch.
: WALK-SCOPE ( str [ str -- bool ] [ str -- ] ^u8 NUM:alloc-byte-len -- )
   drop CELL-VIEW {: s:str allow q ctx:^ :}
   ctx WALK-MAP
   ctx s allow q [: WALK-RUN ;] catch {: code:n :}
   drop drop drop drop
   ctx WALK-UNMAP
   code 0<> if code throw then ;

public
: WALK-FILES-IF ( str [ str -- bool ] [ str -- ] -- )
   WALK-HANDLE-BYTES MEM:BYTES-ALLOC-LEN [: WALK-SCOPE ;] MEM:WITH-BYTES ;

: WALK-FILES ( str [ str -- ] -- ) {: s:str q :}
   s [: drop true ;] q WALK-FILES-IF ;

;package
