\ fs.f - checked filesystem helpers.
\
\ STORAGE CLASS. TASK-LOCAL for everything one call threads: the per-call slots
\ (FS-IO-FD, FS-IO-LEN, FS-IO-RD, FS-IO-OFF, FS-IO-WR), the NUL-padded path
\ buffer FS-PATHZ-BUF, the stat buffer FS-STAT-BUF and the read probe are the
\ FS-ABI band of the per-task DATA region, so two tasks in READ-ALL, WRITE-ALL,
\ FILE-SIZE, FILE-META or any FS-* predicate at once share nothing. The data
\ buffers READ-ALL and WRITE-ALL take are CALLER-OWNED, and so is the WALK:
\ WALK-FILES-IN takes the caller's context, a span of FS-WALK-BYTES holding the
\ whole per-depth state, so any number of tasks walk at once by holding one
\ each. WALK-FILES is the one-context form over the static FS-WALK-CTX0 and is
\ therefore SINGLE-TASK, as is REMOVE-TREE (lib/fs-mutate.f), which stands on
\ the same context; a task that walks or removes a tree beside another one
\ takes WALK-FILES-IN / REMOVE-TREE-IN with a context of its own. See
\ docs/threads.md.
\
\ THE BUFFERS THIS MODULE OWNS ARE SPANS (lib/span.f): the NUL-padded path
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

require lib/errors.f
require lib/string.f
require lib/adt/option.f                        \ option<n> for FS-TRY-*STAT-MODE (switchover wave A)
require lib/span.f                              \ the module's own buffers carry their reach

PATH-CAP constant FS-PATH-CAP                   \ the core's one path capacity (src/core/util.f)
FS-PATH-CAP 1 + constant FS-PATHZ-CAP
4096 constant FS-DIR-CAP
32 constant FS-MAX-DEPTH
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
$4000 constant S-IFDIR
$8000 constant S-IFREG
$A000 constant S-IFLNK

$2E constant FS-DOT
$2F constant FS-SLASH

\ THE PER-CALL SLOTS ARE TASK-LOCAL, in the FS-ABI band of the per-task DATA
\ region: every accessor reads `data-base`, which is the RUNNING task's region,
\ so two tasks in READ-ALL, WRITE-ALL, FILE-SIZE, FILE-META or any FS-*
\ predicate at once share nothing. The band is declared in src/habu/layout.f
\ below FMT-ABI and asserted there against every other DATA claim. It is a
\ declared band and not TASK:+USER rows although this module is not baked:
\ tools/native-build-core.f requires it to build the engine, so a
\ `require lib/task.f` here would load the task runtime into the build tool.
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

\ A WALK IN PROGRESS IS THE CALLER'S BYTES. WALK-FILES-IN and REMOVE-TREE-IN
\ (lib/fs-mutate.f) work on a context the caller owns: any writable span of
\ FS-WALK-BYTES, one per walk. Nothing on that path is process-wide, so two
\ tasks walk or remove trees at the same time by holding a context each, and a
\ callback walks a second tree - or removes a subtree - through a SECOND
\ context while the first walk stands on its own. The state is 161 KiB,
\ fifteen times the whole per-task DATA region, so caller-owned is the only
\ class it can have (src/habu/layout.f, docs/threads.md).
\
\ THE LAYOUT, by byte offset into the span:
\   DEPTH       8 at $000     the depth being walked
\   ACTIVE      8 at $008     nonzero while a walk owns this context
\   CHILD-U     8 at $010     length of the child path FS-DESCEND-PATH built
\   BASES    $100 at $018     per-depth getdirentries64 base cookie
\   FDS      $100 at $118     per-depth directory descriptor, -1 when closed
\   NS       $100 at $218     per-depth bytes the last block read answered
\   OFFS     $100 at $318     per-depth offset into that block
\   RECS     $100 at $418     per-depth length of the record at that offset
\   PATHS  $8000 at $518     FS-MAX-DEPTH path slots of FS-PATH-CAP
\   DIRS  $20000 at $8518    FS-MAX-DEPTH dirent blocks of FS-DIR-CAP
\ THE SCALARS ARE CELLS, reached through CELL-VIEW (the declared view, as in
\ src/core/sha256.f and src/core/checker.f SYM.PKG-U); the path and dirent
\ slots stay byte regions, taken with ZPTR+ and narrowed with SPAN:SUB, so a
\ depth reaches no further than its own slot (E-SPAN-RANGE).
\
\ NOTHING IN A CONTEXT IS A POINTER OR AN XT: the dirent cursor is the value
\ FS-LOAD-ENTRY answers and the callback travels on the stack into the
\ quotation FS-WALK-RUN is caught in, because raw storage never holds an
\ address (docs/forth-card.md § 5). That is what makes a context a plain span
\ the caller may allot, map or embed.
\
\ A CONTEXT MUST THEREFORE BE CELL-ALIGNED. `create` aligns whatever was
\ allotted before it and FS-WALK-BYTES is a multiple of the cell, so both
\ `create C FS-WALK-BYTES allot` and a MEM-ALLOC-BYTES mapping are aligned. A
\ misaligned span is NOT diagnosed - the checker sees a `ptr u8` and AArch64
\ user code does not fault on an unaligned cell access - so the rule is stated
\ here because it can only be stated.
0 constant FS-WALK-DEPTH-CELL
1 constant FS-WALK-ACTIVE-CELL
2 constant FS-WALK-CHILD-U-CELL
3 constant FS-WALK-BASES-CELL
FS-WALK-BASES-CELL FS-MAX-DEPTH + constant FS-WALK-FDS-CELL
FS-WALK-FDS-CELL FS-MAX-DEPTH + constant FS-WALK-NS-CELL
FS-WALK-NS-CELL FS-MAX-DEPTH + constant FS-WALK-OFFS-CELL
FS-WALK-OFFS-CELL FS-MAX-DEPTH + constant FS-WALK-RECS-CELL
FS-WALK-RECS-CELL FS-MAX-DEPTH + constant FS-WALK-CELLS
FS-WALK-CELLS cells constant FS-WALK-PATHS-OFF
FS-MAX-DEPTH FS-PATH-CAP * constant FS-WALK-PATHS-BYTES
FS-WALK-PATHS-OFF FS-WALK-PATHS-BYTES + constant FS-WALK-DIRS-OFF
FS-MAX-DEPTH FS-DIR-CAP * constant FS-WALK-DIRS-BYTES
FS-WALK-DIRS-OFF FS-WALK-DIRS-BYTES + constant FS-WALK-BYTES

\ The one static context WALK-FILES and REMOVE-TREE stand on, for a caller that
\ walks one tree at a time in one task.
create FS-WALK-CTX0 FS-WALK-BYTES allot

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

: FS-CHECK-DEPTH ( n -- ) {: d :}
   d 0 < if E-FS-DEPTH throw then
   d FS-MAX-DEPTH >= if E-FS-DEPTH throw then ;

\ ---- the context's scalar cells ----------------------------------------------
: FS-WALK-DEPTH@ ( ptr u8 -- n )
   CELL-VIEW FS-WALK-DEPTH-CELL cells + @ ;

: FS-WALK-DEPTH! ( n ptr u8 -- )
   CELL-VIEW FS-WALK-DEPTH-CELL cells + ! ;

: FS-WALK-ACTIVE@ ( ptr u8 -- n )
   CELL-VIEW FS-WALK-ACTIVE-CELL cells + @ ;

: FS-WALK-ACTIVE! ( n ptr u8 -- )
   CELL-VIEW FS-WALK-ACTIVE-CELL cells + ! ;

: FS-CHILD-U@ ( ptr u8 -- n )
   CELL-VIEW FS-WALK-CHILD-U-CELL cells + @ ;

: FS-CHILD-U! ( n ptr u8 -- )
   CELL-VIEW FS-WALK-CHILD-U-CELL cells + ! ;

\ ---- the five per-depth stacks ------------------------------------------------
\ FS-CHECK-DEPTH is what keeps a stack index inside the context: the deepest
\ cell of the last stack is the one before FS-WALK-PATHS-OFF.
: FS-WALK-STACK ( ptr u8 n n -- ptr n ) {: ctx:ptr base d :}
   d FS-CHECK-DEPTH
   ctx CELL-VIEW base d + cells + ;

\ A slot is a narrowing of the context's whole path (or dirent) region, so the
\ depth arithmetic is bounded by the region itself (E-SPAN-RANGE) and not only
\ by FS-CHECK-DEPTH.
: FS-CTX-PATHS ( ptr u8 -- SPAN:span<u8> )
   FS-WALK-PATHS-OFF ZPTR+ FS-WALK-PATHS-BYTES SPAN:MAKE ;

: FS-CTX-DIRS ( ptr u8 -- SPAN:span<u8> )
   FS-WALK-DIRS-OFF ZPTR+ FS-WALK-DIRS-BYTES SPAN:MAKE ;

: FS-PATH-SLOT ( ptr u8 n -- SPAN:span<u8> ) {: ctx:ptr d :}
   d FS-CHECK-DEPTH
   ctx FS-CTX-PATHS d FS-PATH-CAP * FS-PATH-CAP SPAN:SUB ;

: FS-DIR-SLOT ( ptr u8 n -- SPAN:span<u8> ) {: ctx:ptr d :}
   d FS-CHECK-DEPTH
   ctx FS-CTX-DIRS d FS-DIR-CAP * FS-DIR-CAP SPAN:SUB ;

: FS-CUR-PATH ( ptr u8 -- SPAN:span<u8> ) {: ctx:ptr :}
   ctx ctx FS-WALK-DEPTH@ FS-PATH-SLOT ;

: FS-NEXT-PATH ( ptr u8 -- SPAN:span<u8> ) {: ctx:ptr :}
   ctx ctx FS-WALK-DEPTH@ 1 + FS-PATH-SLOT ;

: FS-CUR-DIR ( ptr u8 -- SPAN:span<u8> ) {: ctx:ptr :}
   ctx ctx FS-WALK-DEPTH@ FS-DIR-SLOT ;

: FS-BASE@ ( ptr u8 -- ptr n ) {: ctx:ptr :}
   ctx FS-WALK-BASES-CELL ctx FS-WALK-DEPTH@ FS-WALK-STACK ;

: FS-FD-PTR ( ptr u8 n -- ptr n ) {: ctx:ptr d :}
   ctx FS-WALK-FDS-CELL d FS-WALK-STACK ;

: FS-FD@ ( ptr u8 -- n ) {: ctx:ptr :}
   ctx ctx FS-WALK-DEPTH@ FS-FD-PTR @ ;

: FS-FD! ( n ptr u8 -- ) {: fd ctx:ptr :}
   fd ctx ctx FS-WALK-DEPTH@ FS-FD-PTR ! ;

: FS-FD-CLEAR-AT ( ptr u8 n -- ) {: ctx:ptr d :}
   -1 ctx d FS-FD-PTR ! ;

: FS-FDS-RESET ( ptr u8 -- ) {: ctx:ptr :}
   0 begin dup FS-MAX-DEPTH < while
      ctx over FS-FD-CLEAR-AT
      1+
   repeat drop ;

: FS-CLOSE-FD-AT ( ptr u8 n -- ) {: ctx:ptr d :}
   ctx d FS-FD-PTR @ dup 0 >= if
      close ctx d FS-FD-CLEAR-AT
   else
      drop
   then ;

\ Every descriptor this context still holds, from the depth it stopped at back
\ to the root. This is the one cleanup site: FS-WALK-FINISH runs it whether the
\ walk ended or threw, so no error path of its own has to close anything.
: FS-CLOSE-WALK ( ptr u8 -- ) {: ctx:ptr :}
   ctx FS-WALK-DEPTH@ begin dup 0 >= while
      ctx over FS-CLOSE-FD-AT
      1-
   repeat drop ;

: FS-WALK-FINISH ( ptr u8 -- ) {: ctx:ptr :}
   ctx FS-CLOSE-WALK
   0 ctx FS-WALK-ACTIVE! ;

: FS-N@ ( ptr u8 -- n ) {: ctx:ptr :}
   ctx FS-WALK-NS-CELL ctx FS-WALK-DEPTH@ FS-WALK-STACK @ ;

: FS-N! ( n ptr u8 -- ) {: n ctx:ptr :}
   n ctx FS-WALK-NS-CELL ctx FS-WALK-DEPTH@ FS-WALK-STACK ! ;

: FS-OFF@ ( ptr u8 -- n ) {: ctx:ptr :}
   ctx FS-WALK-OFFS-CELL ctx FS-WALK-DEPTH@ FS-WALK-STACK @ ;

: FS-OFF! ( n ptr u8 -- ) {: off ctx:ptr :}
   off ctx FS-WALK-OFFS-CELL ctx FS-WALK-DEPTH@ FS-WALK-STACK ! ;

: FS-REC@ ( ptr u8 -- n ) {: ctx:ptr :}
   ctx FS-WALK-RECS-CELL ctx FS-WALK-DEPTH@ FS-WALK-STACK @ ;

: FS-REC! ( n ptr u8 -- ) {: rec ctx:ptr :}
   rec ctx FS-WALK-RECS-CELL ctx FS-WALK-DEPTH@ FS-WALK-STACK ! ;

: FS-CHECK-JOIN-CAP ( n -- )
   dup FS-PATH-CAP > if E-FS-CAPACITY throw then drop ;

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
: JOIN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n ) {: pa:ptr pu na:ptr nu dst:ptr :}
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

: READ-ALL ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu dst:ptr cap :}
   cap 0 < if E-FS-CAPACITY throw then
   pa pu FS-PATHZ open-rd FS-IO-FD !
   FS-IO-FD @ 0 < if E-FS-OPEN throw then
   0 FS-IO-LEN !
   begin FS-IO-LEN @ cap < while
      FS-IO-FD @ dst FS-IO-LEN @ + cap FS-IO-LEN @ - read FS-IO-RD !
      FS-IO-RD @ 0 < if FS-IO-FD @ close E-FS-IO throw then
      FS-IO-RD @ cap FS-IO-LEN @ - > if FS-IO-FD @ close E-FS-IO throw then
      FS-IO-RD @ 0= if FS-IO-FD @ close FS-IO-LEN @ exit then
      FS-IO-LEN @ FS-IO-RD @ + FS-IO-LEN !
   repeat
   FS-IO-FD @ FS-READ-PROBE FS-READ-PROBE-CAP read FS-IO-RD !
   FS-IO-RD @ 0 < if FS-IO-FD @ close E-FS-IO throw then
   FS-IO-RD @ 0 > if FS-IO-FD @ close E-FS-CAPACITY throw then
   FS-IO-FD @ close
   FS-IO-LEN @ ;

: FS-WRITE-BY-FLAGS ( ptr u8 n ptr u8 n n -- ) {: pa:ptr pu src:ptr u flags :}
   u 0 < if E-FS-CAPACITY throw then
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
   s" .dots" FS-PATH= ;

: FS-SKIP-SELF-ENTRY? ( ptr u8 n -- bool )
   2dup FS-DOT-ENTRY? if 2drop FS-TRUE exit then
   FS-DOTDOT-ENTRY? ;

: FS-SKIP-ENTRY? ( ptr u8 n -- bool )
   2dup FS-SKIP-SELF-ENTRY? if 2drop FS-TRUE exit then
   FS-SKIP-DIR? ;

: FS-OPEN-DIR ( ptr u8 n -- n )
   FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then ;

: FS-DIRENT-RECLEN ( ptr u8 -- n )
   FS-DIRENT-RECLEN-OFF BYTE+ FS-U16@ ;

: FS-TARGET-UNKNOWN ( -- )
   E-FS-DIR throw ;

: FS-DIRENT-NAME-OFFSET ( -- n )
   HB-TARGET-LINUX? if
      FS-LINUX-DIRENT-NAME-OFF exit
   then
   HB-TARGET-MACOS? if
      FS-DIRENT-NAME-OFF exit
   then
   FS-TARGET-UNKNOWN ;

\ The dirent decoders below are shared with lib/fs-list.f, which lists one
\ directory with no walk context at all, so a malformed record throws plainly
\ here: closing descriptors belongs to whoever opened them (FS-WALK-FINISH in
\ this file, CLOSE in fs-list.f).
: FS-LINUX-DIRENT-NAMELEN-SCAN ( ptr u8 n -- n ) {: ent:ptr rec :}
   0 begin dup FS-LINUX-DIRENT-NAME-OFF + rec < while
      ent over FS-LINUX-DIRENT-NAME-OFF + FS-BYTE@ 0= if exit then
      1+
   repeat
   E-FS-DIR throw ;

: FS-LINUX-DIRENT-NAMELEN ( ptr u8 -- n ) {: ent:ptr :}
   ent ent FS-DIRENT-RECLEN FS-LINUX-DIRENT-NAMELEN-SCAN ;

: FS-DIRENT-NAMELEN ( ptr u8 -- n )
   HB-TARGET-LINUX? if FS-LINUX-DIRENT-NAMELEN exit then
   HB-TARGET-MACOS? if FS-DIRENT-NAMELEN-OFF BYTE+ FS-U16@ exit then
   FS-TARGET-UNKNOWN ;

: FS-DIRENT-NAME ( ptr u8 -- ptr u8 n )
   dup FS-DIRENT-NAME-OFFSET BYTE+ swap FS-DIRENT-NAMELEN ;

: FS-DIRENT-NAME-END ( ptr u8 -- n ) {: ent:ptr :}
   FS-DIRENT-NAME-OFFSET ent FS-DIRENT-NAMELEN + ;

: FS-CHECK-RECORD ( ptr u8 ptr u8 -- ) {: ctx:ptr ent:ptr :}
   ctx FS-REC@ 0 <= if E-FS-DIR throw then
   ctx FS-OFF@ ctx FS-REC@ + ctx FS-N@ > if E-FS-DIR throw then
   ent FS-DIRENT-NAME-END ctx FS-REC@ > if
      E-FS-DIR throw
   then ;

: FS-READ-DIR ( ptr u8 -- bool ) {: ctx:ptr :}
   ctx FS-FD@ ctx FS-CUR-DIR SPAN:$ ctx FS-BASE@ getdirentries64
   dup 0 < if drop E-FS-DIR throw then
   dup ctx FS-N! 0 > ;

\ The root goes into the depth-0 walk slot, whose reach the span carries, so the
\ hand-written FS-PATH-CAP check is gone: a root too long for the slot is
\ refused by the copy itself (E-SPAN-CAPACITY).
: FS-WALK-ROOT! ( ptr u8 ptr u8 n -- ) {: ctx:ptr a:ptr u :}
   u 0 < if E-FS-PATH throw then
   a u ctx FS-CUR-PATH SPAN:COPY ;

: FS-WALK-JOIN-LEN ( ptr u8 n n -- n ) {: pa:ptr pu nu :}
   pu 0 < if E-FS-PATH throw then
   nu 0 < if E-FS-PATH throw then
   pa pu nu FS-JOIN-LEN ;

: FS-CHECK-WALK-JOIN-CAP ( ptr u8 n n -- )
   FS-WALK-JOIN-LEN FS-PATH-CAP > if E-FS-CAPACITY throw then ;

: FS-CHECK-WALK-DESCEND ( ptr u8 -- ) {: ctx:ptr :}
   ctx FS-WALK-DEPTH@ 1 + FS-MAX-DEPTH >= if E-FS-DEPTH throw then ;

: FS-OPEN-WALK-DIR ( ptr u8 ptr u8 n -- ) {: ctx:ptr a:ptr u :}
   ctx FS-CHECK-WALK-DESCEND
   a u FS-OPEN-DIR ctx FS-FD!
   0 ctx FS-BASE@ ! ;

: FS-CLOSE-CUR-DIR ( ptr u8 -- ) {: ctx:ptr :}
   ctx FS-FD@ close
   -1 ctx FS-FD! ;

: FS-DIR-BLOCK-BEGIN ( ptr u8 -- )
   0 swap FS-OFF! ;

: FS-DIR-MORE? ( ptr u8 -- bool ) {: ctx:ptr :}
   ctx FS-OFF@ ctx FS-N@ < ;

\ The block offset is a running sum of kernel-supplied record lengths, so the
\ entry address is taken through the dir span: an offset past the block refuses
\ with E-SPAN-RANGE instead of naming memory beyond the buffer. The cursor is
\ the ANSWER and not a cell of the context: an address never lands in raw
\ storage, and the block loop holds it only until it takes the name.
: FS-LOAD-ENTRY ( ptr u8 -- ptr u8 ) {: ctx:ptr :}
   ctx FS-CUR-DIR ctx FS-OFF@ SPAN:AT {: ent:ptr :}
   ent FS-DIRENT-RECLEN ctx FS-REC!
   ctx ent FS-CHECK-RECORD
   ent ;

: FS-ADVANCE-ENTRY ( ptr u8 -- ) {: ctx:ptr :}
   ctx FS-OFF@ ctx FS-REC@ + ctx FS-OFF! ;

\ The child name comes first because the block loop already holds it; the
\ context comes back on top so the recursive call reads ( ctx path ).
: FS-DESCEND-PATH ( ptr u8 n ptr u8 ptr u8 n -- ptr u8 ptr u8 n )
   {: na:ptr nu ctx:ptr pa:ptr pu :}
   pa pu nu FS-CHECK-WALK-JOIN-CAP
   pa pu na nu ctx FS-NEXT-PATH JOIN-PATH-INTO ctx FS-CHILD-U!
   ctx FS-WALK-DEPTH@ 1 + ctx FS-WALK-DEPTH!
   ctx  ctx FS-CUR-PATH ctx FS-CHILD-U@ SPAN:TAKE SPAN:$ ;

: FS-ASCEND-PATH ( ptr u8 -- ) {: ctx:ptr :}
   ctx FS-WALK-DEPTH@ 1 - ctx FS-WALK-DEPTH! ;

\ The callback is executed straight off the stack: it needs no cell, and its
\ throw needs no catch here because FS-WALK-FINISH closes this context's
\ descriptors for every way out of FS-WALK-RUN.
: FS-WALK-PATH ( ptr u8 ptr u8 n [ ptr u8 n -- ] -- ) {: ctx:ptr a:ptr u q :}
   a u FS-SKIP-DIR? if exit then
   a u FILE? if a u q execute exit then
   a u DIR? 0= if E-FS-STAT throw then
   ctx a u FS-OPEN-WALK-DIR
   begin ctx FS-READ-DIR while
      ctx FS-DIR-BLOCK-BEGIN
      begin ctx FS-DIR-MORE? while
         ctx FS-LOAD-ENTRY FS-DIRENT-NAME 2dup FS-SKIP-ENTRY? if
            2drop
         else
            ctx a u FS-DESCEND-PATH q recurse
            ctx FS-ASCEND-PATH
         then
         ctx FS-ADVANCE-ENTRY
      repeat
   repeat
   ctx FS-CLOSE-CUR-DIR ;

\ A quotation-typed LOCAL cannot be caught - `{: a u q :} a u q catch` is
\ "non-certified definition ... at 'catch'" - so the walk travels on the stack
\ into this preserving word and the literal quotation below catches THAT.
: FS-WALK-RUN ( ptr u8 ptr u8 n [ ptr u8 n -- ] -- ptr u8 ptr u8 n [ ptr u8 n -- ] )
   {: ctx:ptr a:ptr u q :}
   ctx a u q FS-WALK-PATH
   ctx a u q ;

: WALK-FILES-IN ( ptr u8 ptr u8 n [ ptr u8 n -- ] -- ) {: ctx:ptr a:ptr u q :}
   ctx FS-WALK-ACTIVE@ 0<> if E-FS-WALK-ACTIVE throw then
   ctx FS-FDS-RESET
   0 ctx FS-WALK-DEPTH!
   ctx a u FS-WALK-ROOT!
   1 ctx FS-WALK-ACTIVE!
   ctx  ctx FS-CUR-PATH u SPAN:TAKE SPAN:$  q  [: FS-WALK-RUN ;] catch {: code:n :}
   drop 2drop drop
   ctx FS-WALK-FINISH
   code 0<> if code throw then ;

: WALK-FILES ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   FS-WALK-CTX0 a u q WALK-FILES-IN ;
