\ fs.f - checked filesystem helpers.
\
\ STORAGE CLASS. TASK-LOCAL for everything one call threads: the per-call slots
\ (FS-IO-FD, FS-IO-LEN, FS-IO-RD, FS-IO-OFF, FS-IO-WR), the last FS cause, the NUL-padded path
\ buffer FS-PATHZ-BUF, the stat buffer FS-STAT-BUF and the read probe are the
\ FS-ABI band of the per-task DATA region, so two tasks in READ-ALL, WRITE-ALL,
\ FILE-SIZE, FILE-META or any FS-* predicate at once share nothing. The data
\ buffers READ-ALL and WRITE-ALL take are CALLER-OWNED. A WALK OWNS ITS STATE
\ FOR ONE CALL: WALK-FILES, WALK-FILES-IF and REMOVE-TREE (src/lib/fs-mutate.f)
\ each map a context of their own and release it on return or throw, so a
\ callback walks or removes another tree, any number of tasks walk at once, and
\ a tree is as deep as FS-PATH-CAP lets its paths be.
\
\ FS:ERRNO IS THE CAUSE. It answers the positive host errno the last
\ filesystem primitive in the running task saved; read it at once after
\ catching an E-FS-* or E-FS-LIST-* throw, since any later primitive, even a
\ successful one, replaces it, and a nested call replaces the outer call's.
\ A semantic refusal (path capacity, a malformed path) leaves zero. The
\ guarantee covers fs.f, fs-list.f, fs-anchored.f and fs-mutate.f's
\ REMOVE-TREE; the identity, lock and other mutation
\ helpers do not join it. The throws stay the primary contract.
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
\ caller-facing contracts rather than buffer extents: FS-PATHZ-INTO keeps
\ `u FS-PATH-CAP > -> E-FS-PATH`, the one path-length limit every fs consumer's
\ refusal test names, and JOIN-PATH keeps FS-CHECK-JOIN-CAP because its
\ destination is still a bare `ptr u8` the caller supplies (294 call sites in
\ this repository; the parameter becomes a span in the consumer band of
\ habu-bound-pointers). READ-ALL's destination is the same deferred case.

require src/lib/errors.f
require src/lib/string.f
require src/lib/adt/option.f                        \ option<n> for FS-TRY-*STAT-MODE (switchover wave A)
require src/lib/span.f                              \ the module's own buffers carry their reach
require src/lib/ffi-abi.f                           \ fstat on an open descriptor
require src/lib/memory.f                            \ each walk's handle scope and frames

PATH-CAP constant FS-PATH-CAP                   \ the core's one path capacity (src/core/util.f)
FS-PATH-CAP 1 + constant FS-PATHZ-CAP
4096 constant FS-DIR-CAP
256 constant FS-STAT-CAP
8 constant FS-BYTE-BITS
16 constant FS-BYTE-BITS-2
24 constant FS-BYTE-BITS-3
32 constant FS-BYTE-BITS-4
40 constant FS-BYTE-BITS-5
48 constant FS-BYTE-BITS-6
56 constant FS-BYTE-BITS-7
4 constant FS-STAT-MODE-OFF
48 constant FS-STAT-MTIME-SEC-OFF
56 constant FS-STAT-MTIME-NS-OFF
64 constant FS-STAT-CTIME-SEC-OFF
72 constant FS-STAT-CTIME-NS-OFF
96 constant FS-STAT-SIZE-OFF
16 constant FS-DIRENT-RECLEN-OFF
18 constant FS-DIRENT-NAMELEN-OFF
21 constant FS-DIRENT-NAME-OFF
19 constant FS-LINUX-DIRENT-NAME-OFF
1 constant FS-O-WRONLY
2 constant FS-O-RDWR
$8 constant FS-O-APPEND
$200 constant FS-O-CREAT
$400 constant FS-O-TRUNC
420 constant FS-MODE-0644
1 constant FS-X-OK
1 constant FS-READ-PROBE-CAP

$F000 constant S-IFMT
$1000 constant S-IFIFO
$4000 constant S-IFDIR
$8000 constant S-IFREG
$A000 constant S-IFLNK

$2E constant FS-DOT
$2F constant FS-SLASH

package FS
public
2 constant ENOENT
13 constant EACCES
20 constant ENOTDIR
: ELOOP ( -- n )
   HB-TARGET-MACOS? if 62 else 40 then ;
: ENAMETOOLONG ( -- n )
   HB-TARGET-MACOS? if 63 else 36 then ;

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
   1 FS-STAT-CAP WRITES-BYTES
;FUNCTION
;package

: FS-CAUSE-CLEAR ( -- )
   0 data-base FS-ABI:ERRNO-OFF + ! ;

\ THE PER-CALL SLOTS ARE TASK-LOCAL, in the FS-ABI band of the per-task DATA
\ region: every accessor reads `data-base`, which is the RUNNING task's region,
\ so two tasks in READ-ALL, WRITE-ALL, FILE-SIZE, FILE-META or any FS-*
\ predicate at once share nothing. The band is declared in src/habu/layout.f
\ below FMT-ABI and asserted there against every other DATA claim. It is a
\ declared band and not TASK:+USER rows although this module is not baked:
\ tools/native-build-core.f requires it to build the engine, so a
\ `require src/lib/task.f` here would load the task runtime into the build tool.
\ A region is a fresh zeroed mapping, so a new task's cells start at zero
\ exactly as the old `variable`s did.
\
\ layout.f is loaded before this file and cannot see FS-STAT-CAP,
\ FS-READ-PROBE-CAP or FS-PATHZ-CAP, so the band states those widths and this
\ executes the agreement once at load.
: FS-BAND-AGREE ( -- )
   FS-STAT-CAP FS-ABI:STAT-BYTES <> if E-FS-BAND throw then
   FS-READ-PROBE-CAP FS-ABI:PROBE-BYTES <> if E-FS-BAND throw then
   FS-PATHZ-CAP FS-ABI:PATHZ-BYTES <> if E-FS-BAND throw then ;
FS-BAND-AGREE

: FS-IO-FD ( -- ptr n )
   data-base FS-ABI:FD-OFF + ;

: FS-IO-LEN ( -- ptr n )
   data-base FS-ABI:LEN-OFF + ;

: FS-IO-RD ( -- ptr n )
   data-base FS-ABI:RD-OFF + ;

: FS-IO-OFF ( -- ptr n )
   data-base FS-ABI:OFF-OFF + ;

: FS-IO-WR ( -- ptr n )
   data-base FS-ABI:WR-OFF + ;

: FS-STAT-BUF ( -- ptr u8 )
   data-base FS-ABI:STAT-OFF + BYTE-VIEW ;

: FS-READ-PROBE ( -- ptr u8 )
   data-base FS-ABI:PROBE-OFF + BYTE-VIEW ;

\ The reach is the band width FS-BAND-AGREE asserted against src/habu/layout.f,
\ so the span says exactly what this task's region holds.
: FS-PATHZ-BUF ( -- SPAN:span<u8> )
   data-base FS-ABI:PATHZ-OFF + BYTE-VIEW FS-PATHZ-CAP SPAN:MAKE ;

\ A WALK OWNS ITS CONTEXT FOR ONE CALL. WALK-FILES-IF and REMOVE-TREE
\ (src/lib/fs-mutate.f) take the handle from a WITH-BYTES scope and map the
\ frames inside it, and release both however the call ends, so nothing on the
\ walk's path is shared: a callback walks or removes another tree through a
\ context of its own, and two tasks walk at once.
\
\ THE HANDLE is FS-WALK-HANDLE-CELLS cells and does not move for the whole
\ call, so every helper's handle local stays valid:
\   FRAMES  cell 0   the frames mapping, a ptr-field cell: a declared pointer
\                    cell is the one raw storage that may hold an address
\                    (docs/forth-card.md § 5)
\   CAP     cell 1   the frames the mapping holds
\   DEPTH   cell 2   the depth being walked
\
\ THE FRAMES are one mapping of CAP records, one per depth, FS-FRAME-BYTES each:
\   BASE    cell 0   getdirentries64 base cookie
\   FD      cell 1   the directory descriptor, -1 when closed
\   N       cell 2   bytes the last block read answered
\   OFF     cell 3   offset of the current record in that block
\   REC     cell 4   length of that record
\   PATH-U  cell 5   length of the path in this depth's slot
\   PATH             FS-PATH-CAP bytes, rounded up to a cell: this depth's path
\   DIR              FS-DIR-CAP bytes: this depth's dirent block
\ FS-WALK-FRAMES0 is where the capacity starts, not a bound: FS-ENSURE-DEPTH
\ doubles the mapping before a descent needs a frame past it, and a tree is as
\ deep as FS-PATH-CAP lets its paths be (E-FS-CAPACITY at the join). Growth
\ MOVES the frames, so no helper holds an address into them across a descent:
\ FS-DESCEND-PATH reads the parent path and the entry name from the frames
\ after growing them, and a walk loop carries only the handle past its
\ recursion. A frame and its slots are narrowings of the whole frames span, so
\ a depth reaches no further than its own record (E-SPAN-RANGE).
0 constant FS-WALK-FRAMES-IDX
1 constant FS-WALK-CAP-CELL
2 constant FS-WALK-DEPTH-CELL
3 constant FS-WALK-HANDLE-CELLS
FS-WALK-HANDLE-CELLS cells constant FS-WALK-HANDLE-BYTES
8 constant FS-WALK-FRAMES0
0 constant FS-FRAME-BASE-CELL
1 constant FS-FRAME-FD-CELL
2 constant FS-FRAME-N-CELL
3 constant FS-FRAME-OFF-CELL
4 constant FS-FRAME-REC-CELL
5 constant FS-FRAME-PATH-U-CELL
6 cells constant FS-FRAME-PATH-OFF
FS-PATH-CAP 1 cells + 1 - 1 cells / cells constant FS-FRAME-PATH-BYTES
FS-FRAME-PATH-OFF FS-FRAME-PATH-BYTES + constant FS-FRAME-DIR-OFF
FS-FRAME-DIR-OFF FS-DIR-CAP + constant FS-FRAME-BYTES

: FS-FALSE ( -- bool )
   0 0= 0= ;

: FS-TRUE ( -- bool )
   0 0= ;

: FS-BYTE@ ( ptr u8 n -- n )
   BYTE+ c@ ;

: FS-U16@ ( ptr u8 -- n ) {: a:ptr :}
   a 0 FS-BYTE@ a 1 FS-BYTE@ FS-BYTE-BITS lshift or ;

: FS-U64@ ( ptr u8 -- n ) {: a:ptr :}
   a 0 FS-BYTE@
   a 1 FS-BYTE@ FS-BYTE-BITS lshift or
   a 2 FS-BYTE@ FS-BYTE-BITS-2 lshift or
   a 3 FS-BYTE@ FS-BYTE-BITS-3 lshift or
   a 4 FS-BYTE@ FS-BYTE-BITS-4 lshift or
   a 5 FS-BYTE@ FS-BYTE-BITS-5 lshift or
   a 6 FS-BYTE@ FS-BYTE-BITS-6 lshift or
   a 7 FS-BYTE@ FS-BYTE-BITS-7 lshift or ;

\ ---- the handle --------------------------------------------------------------
: FS-WALK-FRAMES ( ptr n -- ptr ptr u8 )
   FS-WALK-FRAMES-IDX ptr-field ;

: FS-WALK-CAP ( ptr n -- ptr n )
   FS-WALK-CAP-CELL cells + ;

: FS-WALK-DEPTH@ ( ptr n -- n )
   FS-WALK-DEPTH-CELL cells + @ ;

: FS-WALK-DEPTH! ( n ptr n -- )
   FS-WALK-DEPTH-CELL cells + ! ;

\ ---- the frames --------------------------------------------------------------
: FS-WALK-SPAN ( ptr n -- SPAN:span<u8> ) {: ctx:ptr :}
   ctx FS-WALK-FRAMES @ ctx FS-WALK-CAP @ FS-FRAME-BYTES * SPAN:MAKE ;

: FS-FRAME ( ptr n n -- SPAN:span<u8> ) {: ctx:ptr d :}
   ctx FS-WALK-SPAN d FS-FRAME-BYTES * FS-FRAME-BYTES SPAN:SUB ;

: FS-FRAME-CELL ( ptr n n n -- ptr n ) {: ctx:ptr d k :}
   ctx d FS-FRAME SPAN:$ drop CELL-VIEW k cells + ;

: FS-CUR-CELL ( ptr n n -- ptr n ) {: ctx:ptr k :}
   ctx ctx FS-WALK-DEPTH@ k FS-FRAME-CELL ;

: FS-PATH-SLOT ( ptr n n -- SPAN:span<u8> )
   FS-FRAME FS-FRAME-PATH-OFF FS-PATH-CAP SPAN:SUB ;

: FS-PATH-U ( ptr n n -- ptr n )
   FS-FRAME-PATH-U-CELL FS-FRAME-CELL ;

: FS-CUR-PATH$ ( ptr n -- ptr u8 n ) {: ctx:ptr :}
   ctx FS-WALK-DEPTH@ {: d :}
   ctx d FS-PATH-SLOT ctx d FS-PATH-U @ SPAN:TAKE SPAN:$ ;

: FS-CUR-DIR ( ptr n -- SPAN:span<u8> ) {: ctx:ptr :}
   ctx ctx FS-WALK-DEPTH@ FS-FRAME FS-FRAME-DIR-OFF FS-DIR-CAP SPAN:SUB ;

: FS-BASE@ ( ptr n -- ptr n )
   FS-FRAME-BASE-CELL FS-CUR-CELL ;

: FS-FD-PTR ( ptr n n -- ptr n )
   FS-FRAME-FD-CELL FS-FRAME-CELL ;

: FS-FD@ ( ptr n -- n ) {: ctx:ptr :}
   ctx ctx FS-WALK-DEPTH@ FS-FD-PTR @ ;

: FS-FD! ( n ptr n -- ) {: fd ctx:ptr :}
   fd ctx ctx FS-WALK-DEPTH@ FS-FD-PTR ! ;

: FS-FD-CLEAR-AT ( ptr n n -- ) {: ctx:ptr d :}
   -1 ctx d FS-FD-PTR ! ;

\ A fresh mapping is zeroed, and zero is a descriptor: every frame a mapping
\ adds is marked closed before the walk's depth can reach it.
: FS-FDS-CLEAR ( ptr n n n -- ) {: ctx:ptr from to :}
   from begin dup to < while
      ctx over FS-FD-CLEAR-AT
      1+
   repeat drop ;

: FS-CLOSE-FD-AT ( ptr n n -- ) {: ctx:ptr d :}
   ctx d FS-FD-PTR @ dup 0 >= if
      close ctx d FS-FD-CLEAR-AT
   else
      drop
   then ;

\ Every descriptor this context still holds, from the depth it stopped at back
\ to the root. This is the one cleanup site: FS-WALK-UNMAP runs it whether the
\ walk ended or threw, so no error path of its own has to close anything.
: FS-CLOSE-WALK ( ptr n -- ) {: ctx:ptr :}
   ctx FS-WALK-DEPTH@ begin dup 0 >= while
      ctx over FS-CLOSE-FD-AT
      1-
   repeat drop ;

: FS-N@ ( ptr n -- n )
   FS-FRAME-N-CELL FS-CUR-CELL @ ;

: FS-N! ( n ptr n -- )
   FS-FRAME-N-CELL FS-CUR-CELL ! ;

: FS-OFF@ ( ptr n -- n )
   FS-FRAME-OFF-CELL FS-CUR-CELL @ ;

: FS-OFF! ( n ptr n -- )
   FS-FRAME-OFF-CELL FS-CUR-CELL ! ;

: FS-REC@ ( ptr n -- n )
   FS-FRAME-REC-CELL FS-CUR-CELL @ ;

: FS-REC! ( n ptr n -- )
   FS-FRAME-REC-CELL FS-CUR-CELL ! ;

\ The frames a walk starts on, depth 0, every descriptor closed. Nothing else
\ is allocated yet, so a refused mapping leaves nothing to release.
: FS-WALK-MAP ( ptr n -- ) {: ctx:ptr :}
   FS-WALK-FRAMES0 FS-FRAME-BYTES * MEM-ALLOC-BYTES drop ctx FS-WALK-FRAMES !
   FS-WALK-FRAMES0 ctx FS-WALK-CAP !
   0 ctx FS-WALK-DEPTH!
   ctx 0 FS-WALK-FRAMES0 FS-FDS-CLEAR ;

: FS-WALK-UNMAP ( ptr n -- ) {: ctx:ptr :}
   ctx FS-CLOSE-WALK
   ctx FS-WALK-SPAN MEM:FREE-SPAN ;

\ Make frame d exist before a descent writes it; a descent asks for one frame
\ past the deepest, so doubling always suffices. The handle names the new
\ mapping first and its new frames are closed before the depth reaches them; a
\ refused mapping leaves the walk on its old frames, where FS-CLOSE-WALK still
\ finds every descriptor.
: FS-ENSURE-DEPTH ( ptr n n -- ) {: ctx:ptr d :}
   ctx FS-WALK-CAP @ {: cap :}
   d cap < if exit then
   cap 2 * {: grown :}
   grown FS-FRAME-BYTES * MEM-ALLOC-BYTES SPAN:MAKE {: fresh :}
   ctx FS-WALK-SPAN {: old :}
   old SPAN:$ fresh SPAN:COPY
   fresh SPAN:$ drop ctx FS-WALK-FRAMES !
   grown ctx FS-WALK-CAP !
   ctx cap grown FS-FDS-CLEAR
   old MEM:FREE-SPAN ;

: FS-CHECK-JOIN-CAP ( n -- )
   dup FS-PATH-CAP > if FS-CAUSE-CLEAR E-FS-CAPACITY throw then drop ;

: FS-PATH-UNSAFE? ( ptr u8 n -- bool ) {: a:ptr u :}
   0 begin dup u < while
      a over + c@ 0= if drop FS-TRUE exit then
      1+
   repeat drop FS-FALSE ;

\ The length gate stays E-FS-PATH: a path longer than FS-PATH-CAP is a bad PATH,
\ the refusal every fs consumer tests for, and it is the same limit whatever
\ buffer it is copied into. What the span adds is that the copy and the NUL
\ terminator are bounds-checked against the destination's real reach, so the
\ gate is no longer the only thing between a wrong length and an overrun.
: FS-PATHZ-INTO ( ptr u8 n SPAN:span<u8> -- ptr u8 ) {: a:ptr u dst :}
   FS-CAUSE-CLEAR
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a u FS-PATH-UNSAFE? if E-FS-PATH-UNSAFE throw then
   a u dst SPAN:COPY
   0 dst u SPAN:U8!
   dst SPAN:$ drop ;

: FS-PATHZ ( ptr u8 n -- ptr u8 )
   FS-PATHZ-BUF FS-PATHZ-INTO ;

: EXISTS? ( ptr u8 n -- bool )
   FS-PATHZ 0 access 0= ;

: FS-STAT-MODE@ ( -- n )
   FS-STAT-BUF FS-STAT-MODE-OFF + FS-U16@ ;

: FS-STAT-SIZE@ ( -- n )
   FS-STAT-BUF FS-STAT-SIZE-OFF + FS-U64@ ;

: FS-STAT-MTIME-SEC@ ( -- n )
   FS-STAT-BUF FS-STAT-MTIME-SEC-OFF + FS-U64@ ;

: FS-STAT-MTIME-NS@ ( -- n )
   FS-STAT-BUF FS-STAT-MTIME-NS-OFF + FS-U64@ ;

: FS-STAT-CTIME-SEC@ ( -- n )
   FS-STAT-BUF FS-STAT-CTIME-SEC-OFF + FS-U64@ ;

: FS-STAT-CTIME-NS@ ( -- n )
   FS-STAT-BUF FS-STAT-CTIME-NS-OFF + FS-U64@ ;

: FS-TRY-STAT ( ptr u8 n -- bool )
   FS-PATHZ FS-STAT-BUF stat64 0 < if FS-FALSE exit then
   FS-TRUE ;

: FS-TRY-LSTAT ( ptr u8 n -- bool )
   FS-PATHZ FS-STAT-BUF lstat64 0 < if FS-FALSE exit then
   FS-TRUE ;

\ fstatat(2): the NUL-terminated name relative to a directory descriptor (or
\ FS-AT-CWD) into a buffer of FS-STAT-CAP bytes, in libc's struct stat layout,
\ not lstat64's, so its mode is FS-LIBC-MODE@'s to read. Zero on success, else
\ -1 with the cause in FFI:ERRNO.
PROCESS-SYMBOLS
FUNCTION: FS-STAT-AT-CALL fstatat ( n ptr u8 ptr u8 n -- i32 )
   2 FS-STAT-CAP WRITES-BYTES
;FUNCTION

\ AT_SYMLINK_NOFOLLOW, fstatat's flag for the link itself, never its target.
\ Darwin: xnu bsd/sys/fcntl.h. Linux: include/uapi/linux/fcntl.h, every arch.
: FS-AT-NOFOLLOW ( -- n )
   HB-TARGET-MACOS? if $20 else $100 then ;

\ The mode field of libc's struct stat, which FSTAT-CALL and FS-STAT-AT-CALL
\ write. Global because fs-mutate.f reads the mode FS-STAT-AT-CALL writes, and
\ FS-STAT-MODE@ reads lstat64's layout, whose mode sits elsewhere on Linux.
: FS-LIBC-MODE@ ( ptr u8 -- n )
   HB-TARGET-MACOS? if 4 + FS-U16@ exit then
   HB-TARGET-LINUX-X86-64? if 24 + FS-U16@ exit then
   16 + FS-U16@ ;

: FS-TRY-STAT-MODE ( ptr u8 n -- option<n> )   \ SOME stat mode, NONE if missing/unstatable
   FS-TRY-STAT if FS-STAT-MODE@ OPTION:SOME else OPTION:NONE then ;

: FS-TRY-LSTAT-MODE ( ptr u8 n -- option<n> )   \ SOME lstat mode, NONE if missing/unstatable
   FS-TRY-LSTAT if FS-STAT-MODE@ OPTION:SOME else OPTION:NONE then ;

: STAT-MODE ( ptr u8 n -- n )
   FS-TRY-STAT-MODE MATCH option
     none OF E-FS-STAT throw ENDOF
     some OF ENDOF
   ;MATCH ;

: FILE-SIZE ( ptr u8 n -- n )
   FS-TRY-STAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then
   FS-STAT-SIZE@ ;

: FILE-META ( ptr u8 n -- n n n n n )
   FS-TRY-STAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then
   FS-STAT-SIZE@
   FS-STAT-MTIME-SEC@ FS-STAT-MTIME-NS@
   FS-STAT-CTIME-SEC@ FS-STAT-CTIME-NS@ ;

: FILE? ( ptr u8 n -- bool )
   FS-TRY-STAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFREG = ENDOF
   ;MATCH ;

: DIR? ( ptr u8 n -- bool )
   FS-TRY-STAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFDIR = ENDOF
   ;MATCH ;

: FIFO? ( ptr u8 n -- bool )
   FS-TRY-STAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFIFO = ENDOF
   ;MATCH ;

: SYMLINK? ( ptr u8 n -- bool )
   FS-TRY-LSTAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFLNK = ENDOF
   ;MATCH ;

: EXECUTABLE? ( ptr u8 n -- bool )
   FS-PATHZ FS-X-OK access 0= ;

: BASENAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u begin dup 0 > while
      dup 1 - a + c@ FS-SLASH = if
         a over + u rot - exit
      then
      1-
   repeat
   drop a u ;

\ A parent that already ends in a separator takes no second one; the measure and
\ the write share this one answer.
: FS-JOIN-SEPARATED? ( ptr u8 n -- bool ) {: pa:ptr pu :}
   pu 0 <= if FS-FALSE exit then
   pa pu 1 - + c@ FS-SLASH = ;

: FS-JOIN-LEN ( ptr u8 n n -- n ) {: pa:ptr pu nu :}
   pa pu FS-JOIN-SEPARATED? if pu nu + exit then
   pu 1 + nu + ;

: JOIN-PATH-INTO ( ptr u8 n ptr u8 n SPAN:span<u8> -- n ) {: pa:ptr pu na:ptr nu dst :}
   FS-CAUSE-CLEAR
   pu 0 < if E-FS-PATH throw then
   nu 0 < if E-FS-PATH throw then
   pa pu dst SPAN:COPY
   pa pu FS-JOIN-SEPARATED? if
      na nu dst pu SPAN:SKIP SPAN:COPY
      pu nu + exit
   then
   FS-SLASH dst pu SPAN:U8!
   na nu dst pu 1 + SPAN:SKIP SPAN:COPY
   pu 1 + nu + ;

\ The public join still takes a bare destination, so FS-CHECK-JOIN-CAP stays:
\ E-FS-CAPACITY is what this word promises its 294 call sites, and the reach
\ minted below is the FS-PATH-CAP destination those callers already owe (the
\ check above proved the join fits it). The consumer band of
\ habu-bound-pointers replaces the parameter with the caller's own span and
\ this adapter disappears; every destination this module owns already passes
\ JOIN-PATH-INTO a span from its producer.
\ A negative length is a malformed path whatever the other length is, so both
\ signs are refused first. Each length then meets the capacity on its own before
\ the two are summed: a length near the maximum cell would wrap the join back
\ under FS-PATH-CAP, and the parent's last byte is read only once the parent is
\ known to fit.
: JOIN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n ) {: pa:ptr pu na:ptr nu dst:ptr :}
   FS-CAUSE-CLEAR
   pu 0 < if E-FS-PATH throw then
   nu 0 < if E-FS-PATH throw then
   pu FS-CHECK-JOIN-CAP
   nu FS-CHECK-JOIN-CAP
   pa pu nu FS-JOIN-LEN FS-CHECK-JOIN-CAP
   pa pu na nu  dst FS-PATH-CAP SPAN:MAKE  JOIN-PATH-INTO ;

: FS-PATH= ( ptr u8 n ptr u8 n -- bool )
   STR= ;

\ The destination is the caller's span: `cap 0 <` is gone because a reach is
\ nonnegative by construction, and readlink is handed the span's own base and
\ reach so the kernel cannot write past it. The remaining E-FS-CAPACITY is a
\ SEMANTIC refusal - a link that does not fit would be silently truncated - and
\ stays the caller-facing code its test names.
: READ-LINK ( ptr u8 n SPAN:span<u8> -- n ) {: pa:ptr pu dst :}
   pa pu FS-TRY-LSTAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and S-IFLNK <> if E-FS-STAT throw then
   FS-STAT-SIZE@ dst SPAN:LEN > if E-FS-CAPACITY throw then
   pa pu FS-PATHZ dst SPAN:$ readlink {: n :}
   n 0 < if E-FS-IO throw then
   n ;

: FS-IO-FAIL ( -- )
   FS-IO-FD @ close E-FS-IO throw ;

\ A FUNCTION: syscall saves no cause of its own: record libc's errno as this
\ task's FS:ERRNO, then throw the code.
: FS-ERRNO-FAIL ( n -- )
   FFI:ERRNO data-base FS-ABI:ERRNO-OFF + ! throw ;

\ Read FS-IO-FD into dst until cap bytes or end of file; the count read. A read
\ error closes the descriptor and throws E-FS-IO.
: FS-READ-FILL ( ptr u8 n -- n ) {: dst:ptr cap :}
   0 FS-IO-LEN !
   begin FS-IO-LEN @ cap < while
      FS-IO-FD @ dst FS-IO-LEN @ + cap FS-IO-LEN @ - read FS-IO-RD !
      FS-IO-RD @ 0 < if FS-IO-FAIL then
      FS-IO-RD @ cap FS-IO-LEN @ - > if FS-IO-FAIL then
      FS-IO-RD @ 0= if FS-IO-LEN @ exit then
      FS-IO-LEN @ FS-IO-RD @ + FS-IO-LEN !
   repeat
   FS-IO-LEN @ ;

\ True when FS-IO-FD holds a byte past what FS-READ-FILL read.
: FS-READ-MORE? ( -- bool )
   FS-IO-FD @ FS-READ-PROBE FS-READ-PROBE-CAP read FS-IO-RD !
   FS-IO-RD @ 0 < if FS-IO-FAIL then
   FS-IO-RD @ 0 > ;

: READ-ALL ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu dst:ptr cap :}
   cap 0 < if FS-CAUSE-CLEAR E-FS-CAPACITY throw then
   pa pu FS-PATHZ open-rd FS-IO-FD !
   FS-IO-FD @ 0 < if E-FS-OPEN throw then
   dst cap FS-READ-FILL {: got :}
   got cap = if FS-READ-MORE? if FS-IO-FD @ close E-FS-CAPACITY throw then then
   FS-IO-FD @ close
   got ;

package FS
private
\ The size field of libc's struct stat, which FSTAT-CALL writes.
: LIBC-SIZE@ ( ptr u8 -- n )
   HB-TARGET-MACOS? if 96 + FS-U64@ exit then
   48 + FS-U64@ ;

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
   pa pu FS-PATHZ open-rd {: file :}
   file 0 < if
      ERRNO ENOENT = if -1 >FD 0 FS-FALSE exit then
      E-FS-OPEN throw
   then
   file FS-STAT-BUF FSTAT-CALL 0<> if file FFI:ERRNO FSTAT-FAIL then
   FS-STAT-BUF FS-LIBC-MODE@ S-IFMT and S-IFREG <> if file 0 FSTAT-FAIL then
   file >FD FS-STAT-BUF LIBC-SIZE@ FS-TRUE ;

\ Read exactly the size OPEN-SIZED gave into dst, which holds at least that
\ many bytes, then close the descriptor. Fewer bytes or more than that size
\ mean the open file changed underneath: E-FS-IO, the descriptor closed.
: READ-SIZED ( fd ptr u8 n -- ) {: file:fd dst:ptr size :}
   file FD>N FS-IO-FD !
   dst size FS-READ-FILL size <> if FS-IO-FAIL then
   FS-READ-MORE? if FS-IO-FAIL then
   FS-IO-FD @ close ;
;package

: FS-WRITE-BY-FLAGS ( ptr u8 n ptr u8 n n -- ) {: pa:ptr pu src:ptr u flags :}
   u 0 < if FS-CAUSE-CLEAR E-FS-CAPACITY throw then
   pa pu EXISTS? if pa pu FILE? 0= if E-FS-OPEN throw then then
   pa pu FS-PATHZ flags FS-MODE-0644 open FS-IO-FD !
   FS-IO-FD @ 0 < if E-FS-OPEN throw then
   pa pu FILE? 0= if FS-IO-FD @ close E-FS-OPEN throw then
   0 FS-IO-OFF !
   begin FS-IO-OFF @ u < while
      FS-IO-FD @ src FS-IO-OFF @ + u FS-IO-OFF @ - write FS-IO-WR !
      FS-IO-WR @ 0 <= if FS-IO-FD @ close E-FS-IO throw then
      FS-IO-WR @ u FS-IO-OFF @ - > if FS-IO-FD @ close E-FS-IO throw then
      FS-IO-OFF @ FS-IO-WR @ + FS-IO-OFF !
   repeat
   FS-IO-FD @ close ;

: WRITE-ALL ( ptr u8 n ptr u8 n -- )
   FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-WRITE-BY-FLAGS ;

: APPEND-FILE ( ptr u8 n ptr u8 n -- )
   FS-O-WRONLY FS-O-CREAT or FS-O-APPEND or FS-WRITE-BY-FLAGS ;

: OPEN-APPEND-FD ( ptr u8 n -- n ) {: pa:ptr pu :}
   pa pu EXISTS? if pa pu FILE? 0= if E-FS-OPEN throw then then
   pa pu FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-APPEND or FS-MODE-0644 open {: fd :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: FS-DOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 = if a c@ FS-DOT = else FS-FALSE then ;

: FS-DOTDOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 2 = if a c@ FS-DOT = a 1 + c@ FS-DOT = and else FS-FALSE then ;

: FS-SKIP-DIR? ( ptr u8 n -- bool )
   BASENAME
   2dup s" .jj" FS-PATH= if 2drop FS-TRUE exit then
   2dup s" .jj-ws" FS-PATH= if 2drop FS-TRUE exit then
   2dup s" .git" FS-PATH= if 2drop FS-TRUE exit then
   2dup s" .out" FS-PATH= if 2drop FS-TRUE exit then
   s" .dots" FS-PATH= ;

: FS-SKIP-SELF-ENTRY? ( ptr u8 n -- bool )
   2dup FS-DOT-ENTRY? if 2drop FS-TRUE exit then
   FS-DOTDOT-ENTRY? ;

: FS-SKIP-ENTRY? ( ptr u8 n -- bool )
   2dup FS-SKIP-SELF-ENTRY? if 2drop FS-TRUE exit then
   FS-SKIP-DIR? ;

: FS-OPEN-DIR ( ptr u8 n -- n )
   FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then ;

\ openat(2) and unlinkat(2): the NUL-terminated name relative to a directory
\ descriptor, or to the working directory for FS-AT-CWD. openat answers the
\ new descriptor, unlinkat zero; either answers -1 with the cause in FFI:ERRNO.
PROCESS-SYMBOLS
FUNCTION: FS-OPEN-AT-CALL openat ( n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: FS-UNLINK-AT-CALL unlinkat ( n ptr u8 n -- i32 ) ;FUNCTION

\ AT_FDCWD, the descriptor that names the working directory.
: FS-AT-CWD ( -- n )
   HB-TARGET-MACOS? if -2 else -100 then ;

\ AT_REMOVEDIR, unlinkat's flag to remove an empty directory as rmdir does.
\ Darwin: xnu bsd/sys/fcntl.h. Linux: include/uapi/linux/fcntl.h, every arch.
: FS-AT-REMOVEDIR ( -- n )
   HB-TARGET-MACOS? if $80 else $200 then ;

\ openat's flags for a directory: O_RDONLY|O_DIRECTORY|O_NOFOLLOW|O_CLOEXEC,
\ so a symlink in the last component is refused, never followed. Darwin: xnu
\ bsd/sys/fcntl.h. Linux arm64: arch/arm64 asm/fcntl.h overrides the generic
\ directory/nofollow bits; x86-64 uses the generic bits.
: FS-OPEN-DIR-FLAGS ( -- n )
   HB-TARGET-MACOS? if $100000 $100 or $1000000 or exit then
   HB-TARGET-LINUX-X86-64? if $10000 $20000 or $80000 or exit then
   $4000 $8000 or $80000 or ;

: FS-DIRENT-RECLEN ( ptr u8 -- n )
   FS-DIRENT-RECLEN-OFF BYTE+ FS-U16@ ;

: FS-TARGET-UNKNOWN ( -- )
   FS-CAUSE-CLEAR E-FS-DIR throw ;

: FS-DIRENT-NAME-OFFSET ( -- n )
   HB-TARGET-LINUX-KERNEL? if
      FS-LINUX-DIRENT-NAME-OFF exit
   then
   HB-TARGET-MACOS? if
      FS-DIRENT-NAME-OFF exit
   then
   FS-TARGET-UNKNOWN ;

\ Darwin dirent64 stores d_type at byte 20; Linux getdents64 stores it at 18.
\ Keep the kernel byte, including zero for DT_UNKNOWN.
: FS-DIRENT-TYPE ( ptr u8 -- u8 )
   HB-TARGET-LINUX-KERNEL? if 18 + c@ exit then
   HB-TARGET-MACOS? if 20 + c@ exit then
   FS-TARGET-UNKNOWN ;

\ The dirent decoders below are shared with src/lib/fs-list.f, which lists one
\ directory with no walk context at all, so a malformed record throws plainly
\ here: closing descriptors belongs to whoever opened them (FS-WALK-UNMAP in
\ this file, CLOSE in fs-list.f).
: FS-LINUX-DIRENT-NAMELEN-SCAN ( ptr u8 n -- n ) {: ent:ptr rec :}
   0 begin dup FS-LINUX-DIRENT-NAME-OFF + rec < while
      ent over FS-LINUX-DIRENT-NAME-OFF + FS-BYTE@ 0= if exit then
      1+
   repeat
   FS-CAUSE-CLEAR E-FS-DIR throw ;

: FS-LINUX-DIRENT-NAMELEN ( ptr u8 -- n ) {: ent:ptr :}
   ent ent FS-DIRENT-RECLEN FS-LINUX-DIRENT-NAMELEN-SCAN ;

: FS-DIRENT-NAMELEN ( ptr u8 -- n )
   HB-TARGET-LINUX-KERNEL? if FS-LINUX-DIRENT-NAMELEN exit then
   HB-TARGET-MACOS? if FS-DIRENT-NAMELEN-OFF BYTE+ FS-U16@ exit then
   FS-TARGET-UNKNOWN ;

: FS-DIRENT-NAME ( ptr u8 -- ptr u8 n )
   dup FS-DIRENT-NAME-OFFSET BYTE+ swap FS-DIRENT-NAMELEN ;

: FS-DIRENT-NAME-END ( ptr u8 -- n ) {: ent:ptr :}
   FS-DIRENT-NAME-OFFSET ent FS-DIRENT-NAMELEN + ;

: FS-CHECK-RECORD ( ptr n ptr u8 -- ) {: ctx:ptr ent:ptr :}
   ctx FS-REC@ 0 <= if FS-CAUSE-CLEAR E-FS-DIR throw then
   ctx FS-OFF@ ctx FS-REC@ + ctx FS-N@ > if FS-CAUSE-CLEAR E-FS-DIR throw then
   ent FS-DIRENT-NAME-END ctx FS-REC@ > if
      FS-CAUSE-CLEAR E-FS-DIR throw
   then ;

: FS-READ-DIR ( ptr n -- bool ) {: ctx:ptr :}
   ctx FS-FD@ ctx FS-CUR-DIR SPAN:$ ctx FS-BASE@ getdirentries64
   dup 0 < if drop E-FS-DIR throw then
   dup ctx FS-N! 0 > ;

\ The root goes into the depth-0 walk slot, whose reach the span carries, so the
\ hand-written FS-PATH-CAP check is gone: a root too long for the slot is
\ refused by the copy itself (E-SPAN-CAPACITY).
: FS-WALK-ROOT! ( ptr n ptr u8 n -- ) {: ctx:ptr a:ptr u :}
   FS-CAUSE-CLEAR
   u 0 < if E-FS-PATH throw then
   a u ctx 0 FS-PATH-SLOT SPAN:COPY
   u ctx 0 FS-PATH-U ! ;

: FS-WALK-JOIN-LEN ( ptr u8 n n -- n ) {: pa:ptr pu nu :}
   pu 0 < if FS-CAUSE-CLEAR E-FS-PATH throw then
   nu 0 < if FS-CAUSE-CLEAR E-FS-PATH throw then
   pa pu nu FS-JOIN-LEN ;

: FS-CHECK-WALK-JOIN-CAP ( ptr u8 n n -- )
   FS-WALK-JOIN-LEN FS-PATH-CAP > if FS-CAUSE-CLEAR E-FS-CAPACITY throw then ;

: FS-OPEN-WALK-DIR ( ptr n ptr u8 n -- ) {: ctx:ptr a:ptr u :}
   a u FS-OPEN-DIR ctx FS-FD!
   0 ctx FS-BASE@ ! ;

: FS-CLOSE-CUR-DIR ( ptr n -- ) {: ctx:ptr :}
   ctx FS-FD@ close
   -1 ctx FS-FD! ;

: FS-DIR-BLOCK-BEGIN ( ptr n -- )
   0 swap FS-OFF! ;

: FS-DIR-MORE? ( ptr n -- bool ) {: ctx:ptr :}
   ctx FS-OFF@ ctx FS-N@ < ;

\ The block offset is a running sum of kernel-supplied record lengths, so the
\ entry address is taken through the dir span: an offset past the block refuses
\ with E-SPAN-RANGE instead of naming memory beyond the buffer. The cursor is
\ the ANSWER and not a cell of the context: an address never lands in raw
\ storage, and the block loop holds it only until it takes the name.
: FS-CUR-ENTRY ( ptr n -- ptr u8 ) {: ctx:ptr :}
   ctx FS-CUR-DIR ctx FS-OFF@ SPAN:AT ;

: FS-LOAD-ENTRY ( ptr n -- ptr u8 ) {: ctx:ptr :}
   ctx FS-CUR-ENTRY {: ent:ptr :}
   ent FS-DIRENT-RECLEN ctx FS-REC!
   ctx ent FS-CHECK-RECORD
   ent ;

: FS-ADVANCE-ENTRY ( ptr n -- ) {: ctx:ptr :}
   ctx FS-OFF@ ctx FS-REC@ + ctx FS-OFF! ;

\ The child frame exists before anything is read, because making it may move
\ the frames: the parent path and the entry name come from the frames as they
\ stand after FS-ENSURE-DEPTH. The context comes back under the child path so
\ the recursive call reads ( ctx path ).
: FS-DESCEND-PATH ( ptr n -- ptr n ptr u8 n ) {: ctx:ptr :}
   ctx FS-WALK-DEPTH@ 1 + {: d :}
   ctx d FS-ENSURE-DEPTH
   ctx FS-CUR-PATH$ {: pa:ptr pu :}
   ctx FS-CUR-ENTRY FS-DIRENT-NAME {: na:ptr nu :}
   pa pu nu FS-CHECK-WALK-JOIN-CAP
   pa pu na nu ctx d FS-PATH-SLOT JOIN-PATH-INTO ctx d FS-PATH-U !
   d ctx FS-WALK-DEPTH!
   ctx ctx FS-CUR-PATH$ ;

: FS-ASCEND-PATH ( ptr n -- ) {: ctx:ptr :}
   ctx FS-WALK-DEPTH@ 1 - ctx FS-WALK-DEPTH! ;

\ The callback is executed straight off the stack: it needs no cell, and its
\ throw needs no catch here because FS-WALK-UNMAP closes this context's
\ descriptors for every way out of FS-WALK-RUN. The path `a u` lies in the
\ frames, which a descent may move, so it is read only before the first one;
\ the loop carries nothing but the handle across its recursion.
: FS-WALK-PATH ( ptr n ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] -- )
   {: ctx:ptr a:ptr u allow q :}
   a u FS-SKIP-DIR? if exit then
   a u allow execute 0= if exit then
   a u FILE? if a u q execute exit then
   a u DIR? 0= if E-FS-STAT throw then
   ctx a u FS-OPEN-WALK-DIR
   begin ctx FS-READ-DIR while
      ctx FS-DIR-BLOCK-BEGIN
      begin ctx FS-DIR-MORE? while
         ctx FS-LOAD-ENTRY FS-DIRENT-NAME FS-SKIP-ENTRY? 0= if
            ctx FS-DESCEND-PATH allow q recurse
            ctx FS-ASCEND-PATH
         then
         ctx FS-ADVANCE-ENTRY
      repeat
   repeat
   ctx FS-CLOSE-CUR-DIR ;

\ A quotation-typed LOCAL cannot be caught - `{: a u q :} a u q catch` is
\ "non-certified definition ... at 'catch'" - so the walk travels on the stack
\ into this preserving word and the literal quotation below catches THAT.
: FS-WALK-RUN ( ptr n ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] -- ptr n ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] )
   {: ctx:ptr a:ptr u allow q :}
   ctx a u FS-WALK-ROOT!
   ctx ctx FS-CUR-PATH$ allow q FS-WALK-PATH
   ctx a u allow q ;

\ The WITH-BYTES body: the handle is that scope's bytes, so WITH-BYTES frees it
\ however the walk ends, and the frames are mapped and released here, around
\ the one catch.
: FS-WALK-SCOPE ( ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] ptr u8 NUM:alloc-byte-len -- )
   drop CELL-VIEW {: a:ptr u allow q ctx:ptr :}
   ctx FS-WALK-MAP
   ctx a u allow q [: FS-WALK-RUN ;] catch {: code:n :}
   drop drop 2drop drop
   ctx FS-WALK-UNMAP
   code 0<> if code throw then ;

: WALK-FILES-IF ( ptr u8 n [ ptr u8 n -- bool ] [ ptr u8 n -- ] -- )
   FS-WALK-HANDLE-BYTES MEM:BYTES-ALLOC-LEN [: FS-WALK-SCOPE ;] MEM:WITH-BYTES ;

: WALK-FILES ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   a u [: 2drop true ;] q WALK-FILES-IF ;
