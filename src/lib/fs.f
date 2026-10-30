\ fs.f - checked filesystem helpers.
\
\ STORAGE CLASS. TASK-LOCAL for everything one call threads: the per-call slots
\ (IO-FD, IO-LEN, IO-RD, IO-OFF, IO-WR), the last FS cause, the NUL-padded path
\ buffer PATHZ-BUF, the stat buffer STAT-BUF and the read probe are the
\ FS-ABI band of the per-task DATA region, so two tasks in READ-ALL, WRITE-ALL,
\ FILE-SIZE, FILE-META or any FS predicate at once share nothing. The data
\ buffers READ-ALL and WRITE-ALL take are CALLER-OWNED. A WALK OWNS ITS STATE
\ FOR ONE CALL: WALK-FILES, WALK-FILES-IF and REMOVE-TREE (src/lib/fs-mutate.f)
\ each map a context of their own and release it on return or throw, so a
\ callback walks or removes another tree, any number of tasks walk at once, and
\ a tree is as deep as PATH-CAP lets its paths be.
\
\ FS:ERRNO IS THE CAUSE. It answers the positive host errno the last
\ filesystem primitive in the running task saved; read it at once after
\ catching an E-FS-* or E-FS-LIST-* throw, since any later primitive, even a
\ successful one, replaces it, and a nested call replaces the outer call's.
\ A semantic refusal (path capacity, a malformed path) leaves zero. The
\ guarantee covers fs.f, fs-list.f, fs-anchored.f and fs-mutate.f's
\ REMOVE-TREE, MKDIR-MODE, MAKE-DIR, MAKE-DIRS, ATOMIC-WRITE-FILE,
\ ATOMIC-WRITE-PRIVATE, FS:RESERVE-SIBLING and REPLACE-STAGED, whose cleanup
\ after a failure keeps the first failure's cause; the identity, lock and other
\ mutation helpers do not join it. The throws stay the primary contract.
\
\ THE WALK visits regular files only, depth-first, in the order the platform
\ directory stream returns (a caller that needs lexical order sorts); it skips
\ .git, .jj, .jj-ws, .dots and .out (a tree's build outputs and scratch,
\ tools/build-out.f) and closes every descriptor it opened however it ends.
\ The path a callback gets is valid only during that callback. Two walks in
\ flight cover disjoint trees: getdirentries64 on a directory removed
\ meanwhile answers -1 and the walk reports E-FS-DIR.
\
\ READ-ALL refuses a file larger than the caller's cap with E-FS-CAPACITY.
\ OPEN-SIZED and READ-SIZED read one regular file through one descriptor, so a
\ rename or unlink of the path between them cannot change what is read.
\
\ THE BUFFERS THIS MODULE OWNS ARE SPANS (src/lib/span.f): the NUL-padded path
\ buffer, the per-depth walk path slot and the per-depth dirent block publish a
\ base AND a reach, so every copy into them goes through SPAN:COPY and every
\ indexed write through SPAN:U8! / SPAN:AT - a wrong length cannot reach past
\ the buffer even where the length came from the kernel (a dirent reclen) or
\ from a caller. Two hand-written checks stay on purpose and both are
\ caller-facing contracts rather than buffer extents: PATHZ-INTO keeps
\ `u PATH-CAP > -> E-FS-PATH`, the one path-length limit every fs consumer's
\ refusal test names, and JOIN-PATH keeps CHECK-JOIN-CAP because its
\ destination is still a bare `ptr u8` the caller supplies (294 call sites in
\ this repository; the parameter becomes a span in the consumer band of
\ habu-bound-pointers). READ-ALL's destination is the same deferred case.

require src/lib/errors.f
require src/lib/string.f
require src/lib/adt/option.f                        \ option<n> for TRY-*STAT-MODE (switchover wave A)
require src/lib/le.f                                \ stat and dirent fields
require src/lib/span.f                              \ the module's own buffers carry their reach
require src/lib/ffi-abi.f                           \ fstat on an open descriptor
require src/lib/memory.f                            \ each walk's handle scope and frames

package FS

public
PATH-CAP 1 + constant PATHZ-CAP

private
4096 constant DIR-CAP
256 constant STAT-CAP
4 constant STAT-MODE-OFF
48 constant STAT-MTIME-SEC-OFF
56 constant STAT-MTIME-NS-OFF
64 constant STAT-CTIME-SEC-OFF
72 constant STAT-CTIME-NS-OFF
96 constant STAT-SIZE-OFF
16 constant DIRENT-RECLEN-OFF
18 constant DIRENT-NAMELEN-OFF
21 constant DIRENT-NAME-OFF
19 constant LINUX-DIRENT-NAME-OFF

public
1 constant O-WRONLY
2 constant O-RDWR
$8 constant O-APPEND
$200 constant O-CREAT
$400 constant O-TRUNC
420 constant MODE-0644

private
1 constant X-OK
1 constant READ-PROBE-CAP

public
$F000 constant S-IFMT

private
$1000 constant S-IFIFO
$4000 constant S-IFDIR

public
$8000 constant S-IFREG

private
$A000 constant S-IFLNK

public
$2E constant DOT
$2F constant SLASH

private
2 constant ENOENT

public
13 constant EACCES
: ENAMETOOLONG ( -- n )
   HB-TARGET-MACOS? if 63 else 36 then ;

private
20 constant ENOTDIR
: ELOOP ( -- n )
   HB-TARGET-MACOS? if 62 else 40 then ;

public
\ Read immediately after the failed FS operation: another filesystem primitive
\ replaces this task's cause, including on success. Zero means a semantic refusal.
: ERRNO ( -- n )
   data-base FS-ABI:ERRNO-OFF + @ ;

private
: CAUSE! ( n -- )
   data-base FS-ABI:ERRNO-OFF + ! ;

\ The one fstat binding: libc's struct stat, not the normalized stat64 layout.
PROCESS-SYMBOLS
FUNCTION: FSTAT-CALL fstat ( n ptr u8 -- i32 )
   1 STAT-CAP WRITES-BYTES
;FUNCTION

public
: CAUSE-CLEAR ( -- )
   0 data-base FS-ABI:ERRNO-OFF + ! ;

private
\ THE PER-CALL SLOTS ARE TASK-LOCAL, in the FS-ABI band of the per-task DATA
\ region: every accessor reads `data-base`, which is the RUNNING task's region,
\ so two tasks in READ-ALL, WRITE-ALL, FILE-SIZE, FILE-META or any FS
\ predicate at once share nothing. The band is declared in src/habu/layout.f
\ below FMT-ABI and asserted there against every other DATA claim. It is a
\ declared band and not TASK:+USER rows although this module is not baked:
\ tools/native-build-core.f requires it to build the engine, so a
\ `require src/lib/task.f` here would load the task runtime into the build tool.
\ A region is a fresh zeroed mapping, so a new task's cells start at zero
\ exactly as the old `variable`s did.
\
\ layout.f is loaded before this file and cannot see STAT-CAP,
\ READ-PROBE-CAP or PATHZ-CAP, so the band states those widths and this
\ executes the agreement once at load.
: BAND-AGREE ( -- )
   STAT-CAP FS-ABI:STAT-BYTES <> if E-FS-BAND throw then
   READ-PROBE-CAP FS-ABI:PROBE-BYTES <> if E-FS-BAND throw then
   PATHZ-CAP FS-ABI:PATHZ-BYTES <> if E-FS-BAND throw then ;
BAND-AGREE

: IO-FD ( -- ptr n )
   data-base FS-ABI:FD-OFF + ;

: IO-LEN ( -- ptr n )
   data-base FS-ABI:LEN-OFF + ;

: IO-RD ( -- ptr n )
   data-base FS-ABI:RD-OFF + ;

: IO-OFF ( -- ptr n )
   data-base FS-ABI:OFF-OFF + ;

: IO-WR ( -- ptr n )
   data-base FS-ABI:WR-OFF + ;

: STAT-BUF ( -- ptr u8 )
   data-base FS-ABI:STAT-OFF + BYTE-VIEW ;

: READ-PROBE ( -- ptr u8 )
   data-base FS-ABI:PROBE-OFF + BYTE-VIEW ;

\ The reach is the band width BAND-AGREE asserted against src/habu/layout.f,
\ so the span says exactly what this task's region holds.
: PATHZ-BUF ( -- SPAN:span<u8> )
   data-base FS-ABI:PATHZ-OFF + BYTE-VIEW PATHZ-CAP SPAN:MAKE ;

\ A WALK OWNS ITS CONTEXT FOR ONE CALL. WALK-FILES-IF and REMOVE-TREE
\ (src/lib/fs-mutate.f) take the handle from a WITH-BYTES scope and map the
\ frames inside it, and release both however the call ends, so nothing on the
\ walk's path is shared: a callback walks or removes another tree through a
\ context of its own, and two tasks walk at once.
\
\ THE HANDLE is WALK-HANDLE-CELLS cells and does not move for the whole
\ call, so every helper's handle local stays valid:
\   FRAMES  cell 0   the frames mapping, a ptr-field cell: a declared pointer
\                    cell is the one raw storage that may hold an address
\                    (docs/forth-card.md § 5)
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
\ WALK-FRAMES0 is where the capacity starts, not a bound: ENSURE-DEPTH
\ doubles the mapping before a descent needs a frame past it, and a tree is as
\ deep as PATH-CAP lets its paths be (E-FS-CAPACITY at the join). Growth
\ MOVES the frames, so no helper holds an address into them across a descent:
\ DESCEND-PATH reads the parent path and the entry name from the frames
\ after growing them, and a walk loop carries only the handle past its
\ recursion. A frame and its slots are narrowings of the whole frames span, so
\ a depth reaches no further than its own record (E-SPAN-RANGE).
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

\ ---- the handle --------------------------------------------------------------
: WALK-FRAMES ( ptr n -- ptr ptr u8 )
   WALK-FRAMES-IDX ptr-field ;

: WALK-CAP ( ptr n -- ptr n )
   WALK-CAP-CELL cells + ;

: WALK-DEPTH@ ( ptr n -- n )
   WALK-DEPTH-CELL cells + @ ;

: WALK-DEPTH! ( n ptr n -- )
   WALK-DEPTH-CELL cells + ! ;

\ ---- the frames --------------------------------------------------------------
: WALK-SPAN ( ptr n -- SPAN:span<u8> ) {: ctx:ptr :}
   ctx WALK-FRAMES @ ctx WALK-CAP @ FRAME-BYTES * SPAN:MAKE ;

: FRAME ( ptr n n -- SPAN:span<u8> ) {: ctx:ptr d :}
   ctx WALK-SPAN d FRAME-BYTES * FRAME-BYTES SPAN:SUB ;

: FRAME-FIELD ( ptr n n n -- ptr n ) {: ctx:ptr d k :}
   ctx d FRAME SPAN:$ drop CELL-VIEW k cells + ;

: CUR-FIELD ( ptr n n -- ptr n ) {: ctx:ptr k :}
   ctx ctx WALK-DEPTH@ k FRAME-FIELD ;

: PATH-SLOT ( ptr n n -- SPAN:span<u8> )
   FRAME FRAME-PATH-OFF PATH-CAP SPAN:SUB ;

: PATH-U ( ptr n n -- ptr n )
   FRAME-PATH-U-CELL FRAME-FIELD ;

: CUR-PATH$ ( ptr n -- ptr u8 n ) {: ctx:ptr :}
   ctx WALK-DEPTH@ {: d :}
   ctx d PATH-SLOT ctx d PATH-U @ SPAN:TAKE SPAN:$ ;

: CUR-DIR ( ptr n -- SPAN:span<u8> ) {: ctx:ptr :}
   ctx ctx WALK-DEPTH@ FRAME FRAME-DIR-OFF DIR-CAP SPAN:SUB ;

: BASE@ ( ptr n -- ptr n )
   FRAME-BASE-CELL CUR-FIELD ;

: FD-PTR ( ptr n n -- ptr n )
   FRAME-FD-CELL FRAME-FIELD ;

: FD@ ( ptr n -- n ) {: ctx:ptr :}
   ctx ctx WALK-DEPTH@ FD-PTR @ ;

: FD! ( n ptr n -- ) {: fd ctx:ptr :}
   fd ctx ctx WALK-DEPTH@ FD-PTR ! ;

: FD-CLEAR-AT ( ptr n n -- ) {: ctx:ptr d :}
   -1 ctx d FD-PTR ! ;

\ A fresh mapping is zeroed, and zero is a descriptor: every frame a mapping
\ adds is marked closed before the walk's depth can reach it.
: FDS-CLEAR ( ptr n n n -- ) {: ctx:ptr from to :}
   from begin dup to < while
      ctx over FD-CLEAR-AT
      1+
   repeat drop ;

: CLOSE-FD-AT ( ptr n n -- ) {: ctx:ptr d :}
   ctx d FD-PTR @ dup 0 >= if
      close ctx d FD-CLEAR-AT
   else
      drop
   then ;

\ Every descriptor this context still holds, from the depth it stopped at back
\ to the root. This is the one cleanup site: WALK-UNMAP runs it whether the
\ walk ended or threw, so no error path of its own has to close anything.
: CLOSE-WALK ( ptr n -- ) {: ctx:ptr :}
   ctx WALK-DEPTH@ begin dup 0 >= while
      ctx over CLOSE-FD-AT
      1-
   repeat drop ;

: N@ ( ptr n -- n )
   FRAME-N-CELL CUR-FIELD @ ;

: N! ( n ptr n -- )
   FRAME-N-CELL CUR-FIELD ! ;

: OFF@ ( ptr n -- n )
   FRAME-OFF-CELL CUR-FIELD @ ;

: OFF! ( n ptr n -- )
   FRAME-OFF-CELL CUR-FIELD ! ;

: REC@ ( ptr n -- n )
   FRAME-REC-CELL CUR-FIELD @ ;

: REC! ( n ptr n -- )
   FRAME-REC-CELL CUR-FIELD ! ;

\ The frames a walk starts on, depth 0, every descriptor closed. Nothing else
\ is allocated yet, so a refused mapping leaves nothing to release.
: WALK-MAP ( ptr n -- ) {: ctx:ptr :}
   WALK-FRAMES0 FRAME-BYTES * MEM-ALLOC-BYTES drop ctx WALK-FRAMES !
   WALK-FRAMES0 ctx WALK-CAP !
   0 ctx WALK-DEPTH!
   ctx 0 WALK-FRAMES0 FDS-CLEAR ;

: WALK-UNMAP ( ptr n -- ) {: ctx:ptr :}
   ctx CLOSE-WALK
   ctx WALK-SPAN MEM:FREE-SPAN ;

\ Make frame d exist before a descent writes it; a descent asks for one frame
\ past the deepest, so doubling always suffices. The handle names the new
\ mapping first and its new frames are closed before the depth reaches them; a
\ refused mapping leaves the walk on its old frames, where CLOSE-WALK still
\ finds every descriptor.
: ENSURE-DEPTH ( ptr n n -- ) {: ctx:ptr d :}
   ctx WALK-CAP @ {: cap :}
   d cap < if exit then
   cap 2 * {: grown :}
   grown FRAME-BYTES * MEM-ALLOC-BYTES SPAN:MAKE {: fresh :}
   ctx WALK-SPAN {: old :}
   old SPAN:$ fresh SPAN:COPY
   fresh SPAN:$ drop ctx WALK-FRAMES !
   grown ctx WALK-CAP !
   ctx cap grown FDS-CLEAR
   old MEM:FREE-SPAN ;

: CHECK-JOIN-CAP ( n -- )
   dup PATH-CAP > if CAUSE-CLEAR E-FS-CAPACITY throw then drop ;

\ FS owns the length gate: a path longer than PATH-CAP is E-FS-PATH, the
\ refusal every fs consumer tests for, whatever buffer it is copied into.
\ FFI:CSTR owns the copy, the terminator and the 0-byte refusal (E-FFI-NUL),
\ bounded by the destination's real reach.
: PATHZ-INTO ( ptr u8 n SPAN:span<u8> -- ptr u8 )
   {: a:ptr u dst :}
   CAUSE-CLEAR
   u 0 < if E-FS-PATH throw then
   u PATH-CAP > if E-FS-PATH throw then
   a u dst FFI:CSTR ;

public
: >PATHZ ( ptr u8 n -- ptr u8 )
   PATHZ-BUF PATHZ-INTO ;

: EXISTS? ( ptr u8 n -- bool )
   >PATHZ 0 access 0= ;

: STAT-MODE@ ( -- n )
   STAT-BUF STAT-MODE-OFF + LE:U16@ ;

: STAT-SIZE@ ( -- n )
   STAT-BUF STAT-SIZE-OFF + LE:U64@ ;

: STAT-MTIME-SEC@ ( -- n )
   STAT-BUF STAT-MTIME-SEC-OFF + LE:U64@ ;

: STAT-MTIME-NS@ ( -- n )
   STAT-BUF STAT-MTIME-NS-OFF + LE:U64@ ;

: STAT-CTIME-SEC@ ( -- n )
   STAT-BUF STAT-CTIME-SEC-OFF + LE:U64@ ;

: STAT-CTIME-NS@ ( -- n )
   STAT-BUF STAT-CTIME-NS-OFF + LE:U64@ ;

: TRY-STAT ( ptr u8 n -- bool )
   >PATHZ STAT-BUF stat64 0 < if false exit then
   true ;

: TRY-LSTAT ( ptr u8 n -- bool )
   >PATHZ STAT-BUF lstat64 0 < if false exit then
   true ;

private
\ fstatat(2): the NUL-terminated name relative to a directory descriptor (or
\ AT-CWD) into a buffer of STAT-CAP bytes, in libc's struct stat layout,
\ not lstat64's, so its mode is LIBC-MODE@'s to read. Zero on success, else
\ -1 with the cause in FFI:ERRNO.
PROCESS-SYMBOLS
FUNCTION: STAT-AT-CALL fstatat ( n ptr u8 ptr u8 n -- i32 )
   2 STAT-CAP WRITES-BYTES
;FUNCTION

\ AT_SYMLINK_NOFOLLOW, fstatat's flag for the link itself, never its target.
\ Darwin: xnu bsd/sys/fcntl.h. Linux: include/uapi/linux/fcntl.h, every arch.
: AT-NOFOLLOW ( -- n )
   HB-TARGET-MACOS? if $20 else $100 then ;

\ The mode field of libc's struct stat, which FSTAT-CALL and STAT-AT-CALL
\ write; fs-mutate.f and fs-anchored.f read their stat buffers through it.
\ STAT-MODE@ reads lstat64's layout instead, whose mode sits elsewhere on Linux.
: LIBC-MODE@ ( ptr u8 -- n )
   HB-TARGET-MACOS? if 4 + LE:U16@ exit then
   HB-TARGET-LINUX-X86-64? if 24 + LE:U16@ exit then
   16 + LE:U16@ ;

: TRY-STAT-MODE ( ptr u8 n -- option<n> )   \ SOME stat mode, NONE if missing/unstatable
   TRY-STAT if STAT-MODE@ OPTION:SOME else OPTION:NONE then ;

: TRY-LSTAT-MODE ( ptr u8 n -- option<n> )   \ SOME lstat mode, NONE if missing/unstatable
   TRY-LSTAT if STAT-MODE@ OPTION:SOME else OPTION:NONE then ;

public
: STAT-MODE ( ptr u8 n -- n )
   TRY-STAT-MODE MATCH option
     none OF E-FS-STAT throw ENDOF
     some OF ENDOF
   ;MATCH ;

: FILE-SIZE ( ptr u8 n -- n )
   TRY-STAT 0= if E-FS-STAT throw then
   STAT-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then
   STAT-SIZE@ ;

: FILE-META ( ptr u8 n -- n n n n n )
   TRY-STAT 0= if E-FS-STAT throw then
   STAT-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then
   STAT-SIZE@
   STAT-MTIME-SEC@ STAT-MTIME-NS@
   STAT-CTIME-SEC@ STAT-CTIME-NS@ ;

: FILE? ( ptr u8 n -- bool )
   TRY-STAT-MODE MATCH option
     none OF false ENDOF
     some OF S-IFMT and S-IFREG = ENDOF
   ;MATCH ;

: DIR? ( ptr u8 n -- bool )
   TRY-STAT-MODE MATCH option
     none OF false ENDOF
     some OF S-IFMT and S-IFDIR = ENDOF
   ;MATCH ;

: FIFO? ( ptr u8 n -- bool )
   TRY-STAT-MODE MATCH option
     none OF false ENDOF
     some OF S-IFMT and S-IFIFO = ENDOF
   ;MATCH ;

: SYMLINK? ( ptr u8 n -- bool )
   TRY-LSTAT-MODE MATCH option
     none OF false ENDOF
     some OF S-IFMT and S-IFLNK = ENDOF
   ;MATCH ;

: EXECUTABLE? ( ptr u8 n -- bool )
   >PATHZ X-OK access 0= ;

: BASENAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u begin dup 0 > while
      dup 1 - a + c@ SLASH = if
         a over + u rot - exit
      then
      1-
   repeat
   drop a u ;

\ A parent that already ends in a separator takes no second one; the measure and
\ the write share this one answer.
: JOIN-SEPARATED? ( ptr u8 n -- bool ) {: pa:ptr pu :}
   pu 0 <= if false exit then
   pa pu 1 - + c@ SLASH = ;

private
: JOIN-LEN ( ptr u8 n n -- n ) {: pa:ptr pu nu :}
   pa pu JOIN-SEPARATED? if pu nu + exit then
   pu 1 + nu + ;

public
: JOIN-PATH-INTO ( ptr u8 n ptr u8 n SPAN:span<u8> -- n ) {: pa:ptr pu na:ptr nu dst :}
   CAUSE-CLEAR
   pu 0 < if E-FS-PATH throw then
   nu 0 < if E-FS-PATH throw then
   pa pu dst SPAN:COPY
   pa pu JOIN-SEPARATED? if
      na nu dst pu SPAN:SKIP SPAN:COPY
      pu nu + exit
   then
   SLASH dst pu SPAN:U8!
   na nu dst pu 1 + SPAN:SKIP SPAN:COPY
   pu 1 + nu + ;

\ The public join still takes a bare destination, so CHECK-JOIN-CAP stays:
\ E-FS-CAPACITY is what this word promises its 294 call sites, and the reach
\ minted below is the PATH-CAP destination those callers already owe (the
\ check above proved the join fits it). The consumer band of
\ habu-bound-pointers replaces the parameter with the caller's own span and
\ this adapter disappears; every destination this module owns already passes
\ JOIN-PATH-INTO a span from its producer.
\ A negative length is a malformed path whatever the other length is, so both
\ signs are refused first. Each length then meets the capacity on its own before
\ the two are summed: a length near the maximum cell would wrap the join back
\ under PATH-CAP, and the parent's last byte is read only once the parent is
\ known to fit.
: JOIN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n ) {: pa:ptr pu na:ptr nu dst:ptr :}
   CAUSE-CLEAR
   pu 0 < if E-FS-PATH throw then
   nu 0 < if E-FS-PATH throw then
   pu CHECK-JOIN-CAP
   nu CHECK-JOIN-CAP
   pa pu nu JOIN-LEN CHECK-JOIN-CAP
   pa pu na nu  dst PATH-CAP SPAN:MAKE  JOIN-PATH-INTO ;

private
: PATH= ( ptr u8 n ptr u8 n -- bool )
   STR= ;

\ The destination is the caller's span: `cap 0 <` is gone because a reach is
\ nonnegative by construction, and readlink is handed the span's own base and
\ reach so the kernel cannot write past it. The remaining E-FS-CAPACITY is a
\ SEMANTIC refusal - a link that does not fit would be silently truncated - and
\ stays the caller-facing code its test names.
: READ-LINK ( ptr u8 n SPAN:span<u8> -- n ) {: pa:ptr pu dst :}
   pa pu TRY-LSTAT 0= if E-FS-STAT throw then
   STAT-MODE@ S-IFMT and S-IFLNK <> if E-FS-STAT throw then
   STAT-SIZE@ dst SPAN:LEN > if E-FS-CAPACITY throw then
   pa pu >PATHZ dst SPAN:$ readlink {: n :}
   n 0 < if E-FS-IO throw then
   n ;

: IO-FAIL ( -- )
   IO-FD @ close E-FS-IO throw ;

\ A FUNCTION: syscall saves no cause of its own: record libc's errno as this
\ task's FS:ERRNO, then throw the code.
: ERRNO-FAIL ( n -- )
   FFI:ERRNO data-base FS-ABI:ERRNO-OFF + ! throw ;

\ Throw n1 with n2 as this task's FS:ERRNO: a cause read before cleanup whose
\ own primitives replaced it.
: CAUSE-FAIL ( n n -- )
   data-base FS-ABI:ERRNO-OFF + ! throw ;

\ Read IO-FD into dst until cap bytes or end of file; the count read. A read
\ error closes the descriptor and throws E-FS-IO.
: READ-FILL ( ptr u8 n -- n ) {: dst:ptr cap :}
   0 IO-LEN !
   begin IO-LEN @ cap < while
      IO-FD @ dst IO-LEN @ + cap IO-LEN @ - read IO-RD !
      IO-RD @ 0 < if IO-FAIL then
      IO-RD @ cap IO-LEN @ - > if IO-FAIL then
      IO-RD @ 0= if IO-LEN @ exit then
      IO-LEN @ IO-RD @ + IO-LEN !
   repeat
   IO-LEN @ ;

\ True when IO-FD holds a byte past what READ-FILL read.
: READ-MORE? ( -- bool )
   IO-FD @ READ-PROBE READ-PROBE-CAP read IO-RD !
   IO-RD @ 0 < if IO-FAIL then
   IO-RD @ 0 > ;

public
: READ-ALL ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu dst:ptr cap :}
   cap 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   pa pu >PATHZ open-rd IO-FD !
   IO-FD @ 0 < if E-FS-OPEN throw then
   dst cap READ-FILL {: got :}
   got cap = if READ-MORE? if IO-FD @ close E-FS-CAPACITY throw then then
   IO-FD @ close
   got ;

private
\ The size field of libc's struct stat, which FSTAT-CALL writes.
: LIBC-SIZE@ ( ptr u8 -- n )
   HB-TARGET-MACOS? if 96 + LE:U64@ exit then
   48 + LE:U64@ ;

: FSTAT-FAIL ( n n -- ) {: file cause :}
   file close
   cause CAUSE!
   E-FS-STAT throw ;

public
\ Open a regular file for reading once and size it from that descriptor, so a
\ writer renaming a new file over the path or a remover unlinking it cannot
\ change what READ-SIZED reads. A missing file (ENOENT) is `fd 0 false` with no
\ descriptor open; any other open failure is E-FS-OPEN, and a descriptor that
\ is not a regular file is closed and E-FS-STAT. The open blocks, as READ-ALL's
\ does, so a FIFO at the path waits for a writer: name files the caller owns.
\ A true flag hands the caller an open descriptor that READ-SIZED closes.
: OPEN-SIZED ( ptr u8 n -- fd n bool ) {: pa:ptr pu :}
   pa pu >PATHZ open-rd {: file :}
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
: READ-SIZED ( fd ptr u8 n -- ) {: file:fd dst:ptr size :}
   file FD>N IO-FD !
   dst size READ-FILL size <> if IO-FAIL then
   READ-MORE? if IO-FAIL then
   IO-FD @ close ;

private
: WRITE-BY-FLAGS ( ptr u8 n ptr u8 n n -- ) {: pa:ptr pu src:ptr u flags :}
   u 0 < if CAUSE-CLEAR E-FS-CAPACITY throw then
   pa pu EXISTS? if pa pu FILE? 0= if E-FS-OPEN throw then then
   pa pu >PATHZ flags MODE-0644 open IO-FD !
   IO-FD @ 0 < if E-FS-OPEN throw then
   pa pu FILE? 0= if IO-FD @ close E-FS-OPEN throw then
   0 IO-OFF !
   begin IO-OFF @ u < while
      IO-FD @ src IO-OFF @ + u IO-OFF @ - write IO-WR !
      IO-WR @ 0 <= if IO-FD @ close E-FS-IO throw then
      IO-WR @ u IO-OFF @ - > if IO-FD @ close E-FS-IO throw then
      IO-OFF @ IO-WR @ + IO-OFF !
   repeat
   IO-FD @ close ;

public
: WRITE-ALL ( ptr u8 n ptr u8 n -- )
   O-WRONLY O-CREAT or O-TRUNC or WRITE-BY-FLAGS ;

: APPEND-FILE ( ptr u8 n ptr u8 n -- )
   O-WRONLY O-CREAT or O-APPEND or WRITE-BY-FLAGS ;

: OPEN-APPEND-FD ( ptr u8 n -- n ) {: pa:ptr pu :}
   pa pu EXISTS? if pa pu FILE? 0= if E-FS-OPEN throw then then
   pa pu >PATHZ O-WRONLY O-CREAT or O-APPEND or MODE-0644 open {: fd :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

private
: DOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 = if a c@ DOT = else false then ;

: DOTDOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 2 = if a c@ DOT = a 1 + c@ DOT = and else false then ;

: SKIP-DIR? ( ptr u8 n -- bool )
   BASENAME
   2dup s" .jj" PATH= if 2drop true exit then
   2dup s" .jj-ws" PATH= if 2drop true exit then
   2dup s" .git" PATH= if 2drop true exit then
   2dup s" .out" PATH= if 2drop true exit then
   s" .dots" PATH= ;

public
: SKIP-SELF-ENTRY? ( ptr u8 n -- bool )
   2dup DOT-ENTRY? if 2drop true exit then
   DOTDOT-ENTRY? ;

private
: SKIP-ENTRY? ( ptr u8 n -- bool )
   2dup SKIP-SELF-ENTRY? if 2drop true exit then
   SKIP-DIR? ;

public
: OPEN-DIR-READ ( ptr u8 n -- n )
   >PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then ;

private
\ openat(2) and unlinkat(2): the NUL-terminated name relative to a directory
\ descriptor, or to the working directory for AT-CWD. openat answers the
\ new descriptor, unlinkat zero; either answers -1 with the cause in FFI:ERRNO.
PROCESS-SYMBOLS
FUNCTION: OPEN-AT-CALL openat ( n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: UNLINK-AT-CALL unlinkat ( n ptr u8 n -- i32 ) ;FUNCTION

\ AT_FDCWD, the descriptor that names the working directory.
: AT-CWD ( -- n )
   HB-TARGET-MACOS? if -2 else -100 then ;

\ AT_REMOVEDIR, unlinkat's flag to remove an empty directory as rmdir does.
\ Darwin: xnu bsd/sys/fcntl.h. Linux: include/uapi/linux/fcntl.h, every arch.
: AT-REMOVEDIR ( -- n )
   HB-TARGET-MACOS? if $80 else $200 then ;

\ openat's flags for a directory: O_RDONLY|O_DIRECTORY|O_NOFOLLOW|O_CLOEXEC,
\ so a symlink in the last component is refused, never followed. Darwin: xnu
\ bsd/sys/fcntl.h. Linux arm64: arch/arm64 asm/fcntl.h overrides the generic
\ directory/nofollow bits; x86-64 uses the generic bits.
: OPEN-DIR-FLAGS ( -- n )
   HB-TARGET-MACOS? if $100000 $100 or $1000000 or exit then
   HB-TARGET-LINUX-X86-64? if $10000 $20000 or $80000 or exit then
   $4000 $8000 or $80000 or ;

\ openat's flags for a directory used only as later calls' directory, which
\ needs search permission, not read: OPEN-DIR-FLAGS with Darwin's O_SEARCH
\ (O_EXEC $40000000 with O_DIRECTORY, xnu bsd/sys/fcntl.h) or Linux's O_PATH
\ $200000 (include/uapi/asm-generic/fcntl.h, which arm64 does not override).
: SEARCH-DIR-FLAGS ( -- n )
   HB-TARGET-MACOS? if $40000000 OPEN-DIR-FLAGS or exit then
   $200000 OPEN-DIR-FLAGS or ;

public
: DIRENT-RECLEN ( ptr u8 -- n )
   DIRENT-RECLEN-OFF BYTE+ LE:U16@ ;

private
: TARGET-UNKNOWN ( -- )
   CAUSE-CLEAR E-FS-DIR throw ;

public
: DIRENT-NAME-OFFSET ( -- n )
   HB-TARGET-LINUX-KERNEL? if
      LINUX-DIRENT-NAME-OFF exit
   then
   HB-TARGET-MACOS? if
      DIRENT-NAME-OFF exit
   then
   TARGET-UNKNOWN ;

\ Darwin dirent64 stores d_type at byte 20; Linux getdents64 stores it at 18.
\ Keep the kernel byte, including zero for DT_UNKNOWN.
: DIRENT-TYPE ( ptr u8 -- u8 )
   HB-TARGET-LINUX-KERNEL? if 18 + c@ exit then
   HB-TARGET-MACOS? if 20 + c@ exit then
   TARGET-UNKNOWN ;

private
\ The dirent decoders below are shared with src/lib/fs-list.f, which lists one
\ directory with no walk context at all, so a malformed record throws plainly
\ here: closing descriptors belongs to whoever opened them (WALK-UNMAP in
\ this file, CLOSE in fs-list.f).
: LINUX-DIRENT-NAMELEN-SCAN ( ptr u8 n -- n ) {: ent:ptr rec :}
   0 begin dup LINUX-DIRENT-NAME-OFF + rec < while
      ent over LINUX-DIRENT-NAME-OFF + BYTE@ 0= if exit then
      1+
   repeat
   CAUSE-CLEAR E-FS-DIR throw ;

: LINUX-DIRENT-NAMELEN ( ptr u8 -- n ) {: ent:ptr :}
   ent ent DIRENT-RECLEN LINUX-DIRENT-NAMELEN-SCAN ;

: DIRENT-NAMELEN ( ptr u8 -- n )
   HB-TARGET-LINUX-KERNEL? if LINUX-DIRENT-NAMELEN exit then
   HB-TARGET-MACOS? if DIRENT-NAMELEN-OFF BYTE+ LE:U16@ exit then
   TARGET-UNKNOWN ;

public
: DIRENT-NAME ( ptr u8 -- ptr u8 n )
   dup DIRENT-NAME-OFFSET BYTE+ swap DIRENT-NAMELEN ;

private
: DIRENT-NAME-END ( ptr u8 -- n ) {: ent:ptr :}
   DIRENT-NAME-OFFSET ent DIRENT-NAMELEN + ;

: CHECK-RECORD ( ptr n ptr u8 -- ) {: ctx:ptr ent:ptr :}
   ctx REC@ 0 <= if CAUSE-CLEAR E-FS-DIR throw then
   ctx OFF@ ctx REC@ + ctx N@ > if CAUSE-CLEAR E-FS-DIR throw then
   ent DIRENT-NAME-END ctx REC@ > if
      CAUSE-CLEAR E-FS-DIR throw
   then ;

: READ-DIR ( ptr n -- bool ) {: ctx:ptr :}
   ctx FD@ ctx CUR-DIR SPAN:$ ctx BASE@ getdirentries64
   dup 0 < if drop E-FS-DIR throw then
   dup ctx N! 0 > ;

\ The root goes into the depth-0 walk slot, whose reach the span carries, so the
\ hand-written PATH-CAP check is gone: a root too long for the slot is
\ refused by the copy itself (E-SPAN-CAPACITY).
: WALK-ROOT! ( ptr n ptr u8 n -- ) {: ctx:ptr a:ptr u :}
   CAUSE-CLEAR
   u 0 < if E-FS-PATH throw then
   a u ctx 0 PATH-SLOT SPAN:COPY
   u ctx 0 PATH-U ! ;

: WALK-JOIN-LEN ( ptr u8 n n -- n ) {: pa:ptr pu nu :}
   pu 0 < if CAUSE-CLEAR E-FS-PATH throw then
   nu 0 < if CAUSE-CLEAR E-FS-PATH throw then
   pa pu nu JOIN-LEN ;

: CHECK-WALK-JOIN-CAP ( ptr u8 n n -- )
   WALK-JOIN-LEN PATH-CAP > if CAUSE-CLEAR E-FS-CAPACITY throw then ;

: OPEN-WALK-DIR ( ptr n ptr u8 n -- ) {: ctx:ptr a:ptr u :}
   a u OPEN-DIR-READ ctx FD!
   0 ctx BASE@ ! ;

: CLOSE-CUR-DIR ( ptr n -- ) {: ctx:ptr :}
   ctx FD@ close
   -1 ctx FD! ;

: DIR-BLOCK-BEGIN ( ptr n -- )
   0 swap OFF! ;

: DIR-MORE? ( ptr n -- bool ) {: ctx:ptr :}
   ctx OFF@ ctx N@ < ;

\ The block offset is a running sum of kernel-supplied record lengths, so the
\ entry address is taken through the dir span: an offset past the block refuses
\ with E-SPAN-RANGE instead of naming memory beyond the buffer. The cursor is
\ the ANSWER and not a cell of the context: an address never lands in raw
\ storage, and the block loop holds it only until it takes the name.
: CUR-ENTRY ( ptr n -- ptr u8 ) {: ctx:ptr :}
   ctx CUR-DIR ctx OFF@ SPAN:AT ;

: LOAD-ENTRY ( ptr n -- ptr u8 ) {: ctx:ptr :}
   ctx CUR-ENTRY {: ent:ptr :}
   ent DIRENT-RECLEN ctx REC!
   ctx ent CHECK-RECORD
   ent ;

: ADVANCE-ENTRY ( ptr n -- ) {: ctx:ptr :}
   ctx OFF@ ctx REC@ + ctx OFF! ;

\ The child frame exists before anything is read, because making it may move
\ the frames: the parent path and the entry name come from the frames as they
\ stand after ENSURE-DEPTH. The context comes back under the child path so
\ the recursive call reads ( ctx path ).
: DESCEND-PATH ( ptr n -- ptr n ptr u8 n ) {: ctx:ptr :}
   ctx WALK-DEPTH@ 1 + {: d :}
   ctx d ENSURE-DEPTH
   ctx CUR-PATH$ {: pa:ptr pu :}
   ctx CUR-ENTRY DIRENT-NAME {: na:ptr nu :}
   pa pu nu CHECK-WALK-JOIN-CAP
   pa pu na nu ctx d PATH-SLOT JOIN-PATH-INTO ctx d PATH-U !
   d ctx WALK-DEPTH!
   ctx ctx CUR-PATH$ ;

: ASCEND-PATH ( ptr n -- ) {: ctx:ptr :}
   ctx WALK-DEPTH@ 1 - ctx WALK-DEPTH! ;

\ The callback is executed straight off the stack: it needs no cell, and its
\ throw needs no catch here because WALK-UNMAP closes this context's
\ descriptors for every way out of WALK-RUN. The path `a u` lies in the
\ frames, which a descent may move, so it is read only before the first one;
\ the loop carries nothing but the handle across its recursion.
: WALK-PATH ( ptr n ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] -- )
   {: ctx:ptr a:ptr u allow q :}
   a u SKIP-DIR? if exit then
   a u allow execute 0= if exit then
   a u FILE? if a u q execute exit then
   a u DIR? 0= if E-FS-STAT throw then
   ctx a u OPEN-WALK-DIR
   begin ctx READ-DIR while
      ctx DIR-BLOCK-BEGIN
      begin ctx DIR-MORE? while
         ctx LOAD-ENTRY DIRENT-NAME SKIP-ENTRY? 0= if
            ctx DESCEND-PATH allow q recurse
            ctx ASCEND-PATH
         then
         ctx ADVANCE-ENTRY
      repeat
   repeat
   ctx CLOSE-CUR-DIR ;

\ A quotation-typed LOCAL cannot be caught - `{: a u q :} a u q catch` is
\ "non-certified definition ... at 'catch'" - so the walk travels on the stack
\ into this preserving word and the literal quotation below catches THAT.
: WALK-RUN ( ptr n ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] -- ptr n ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] )
   {: ctx:ptr a:ptr u allow q :}
   ctx a u WALK-ROOT!
   ctx ctx CUR-PATH$ allow q WALK-PATH
   ctx a u allow q ;

\ The WITH-BYTES body: the handle is that scope's bytes, so WITH-BYTES frees it
\ however the walk ends, and the frames are mapped and released here, around
\ the one catch.
: WALK-SCOPE ( ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] ptr u8 NUM:alloc-byte-len -- )
   drop CELL-VIEW {: a:ptr u allow q ctx:ptr :}
   ctx WALK-MAP
   ctx a u allow q [: WALK-RUN ;] catch {: code:n :}
   drop drop 2drop drop
   ctx WALK-UNMAP
   code 0<> if code throw then ;

public
: WALK-FILES-IF ( ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] -- )
   WALK-HANDLE-BYTES MEM:BYTES-ALLOC-LEN [: WALK-SCOPE ;] MEM:WITH-BYTES ;

: WALK-FILES ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   a u [: 2drop true ;] q WALK-FILES-IF ;

;package
