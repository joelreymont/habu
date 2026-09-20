\ fs.f - checked filesystem helpers.
\
\ Load after lib/errors.f and lib/string.f.
\
\ STORAGE CLASS. TASK-LOCAL for everything one call threads: the per-call slots
\ (FS-IO-FD, FS-IO-LEN, FS-IO-RD, FS-IO-OFF, FS-IO-WR), the NUL-padded path
\ buffer FS-PATHZ-BUF, the stat buffer FS-STAT-BUF and the read probe are the
\ FS-ABI band of the per-task DATA region, so two tasks in READ-ALL, WRITE-ALL,
\ FILE-SIZE, FILE-META or any FS-* predicate at once share nothing. The data
\ buffers READ-ALL and WRITE-ALL take are CALLER-OWNED. The walk state
\ (FS-DEPTH and the FS-WALK-BUF / FS-DIR-BUF stacks) is PROCESS-WIDE and does
\ not fit a per-task band, so WALK-FILES is still single-task. See
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

FS-MAX-DEPTH FS-PATH-CAP * SPAN-BUFFER: FS-WALK-BUF
FS-MAX-DEPTH FS-DIR-CAP * SPAN-BUFFER: FS-DIR-BUF
create FS-BASES FS-MAX-DEPTH cells allot
create FS-FDS FS-MAX-DEPTH cells allot
create FS-NS FS-MAX-DEPTH cells allot
create FS-OFFS FS-MAX-DEPTH cells allot
create FS-RECS FS-MAX-DEPTH cells allot

variable FS-DEPTH
variable FS-CHILD-U
variable FS-NAME-A
variable FS-NAME-U

\ The walk cursor holds the address of a dirent record inside FS-DIR-BUF, so it
\ is a declared pointer cell: a plain `variable` publishes an undeclared raw
\ cell, and a pointer stored in or fetched from one is refused
\ (E-RAW-CELL-PTR).
TYPED-VARIABLE FS-ENT ptr u8

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

\ A slot is a narrowing of the whole stack buffer, so the depth arithmetic is
\ bounded by the buffer itself (E-SPAN-RANGE) and not only by FS-CHECK-DEPTH.
: FS-PATH-SLOT ( n -- SPAN:span<u8> ) {: d :}
   d FS-CHECK-DEPTH
   FS-WALK-BUF d FS-PATH-CAP * FS-PATH-CAP SPAN:SUB ;

: FS-DIR-SLOT ( n -- SPAN:span<u8> ) {: d :}
   d FS-CHECK-DEPTH
   FS-DIR-BUF d FS-DIR-CAP * FS-DIR-CAP SPAN:SUB ;

: FS-CUR-PATH ( -- SPAN:span<u8> )
   FS-DEPTH @ FS-PATH-SLOT ;

: FS-NEXT-PATH ( -- SPAN:span<u8> )
   FS-DEPTH @ 1 + FS-PATH-SLOT ;

: FS-CUR-DIR ( -- SPAN:span<u8> )
   FS-DEPTH @ FS-DIR-SLOT ;

: FS-BASE@ ( -- ptr n )
   FS-DEPTH @ cells FS-BASES + ;

: FS-FD-PTR ( n -- ptr n ) {: d :}
   d FS-CHECK-DEPTH
   d cells FS-FDS + ;

: FS-FD@ ( -- n )
   FS-DEPTH @ FS-FD-PTR @ ;

: FS-FD! ( n -- ) {: fd :}
   fd FS-DEPTH @ FS-FD-PTR ! ;

: FS-FDS-RESET ( -- )
   0 begin dup FS-MAX-DEPTH < while
      -1 over FS-FD-PTR !
      1+
   repeat drop ;

: FS-CLOSE-FD-AT ( n -- ) {: d :}
   d FS-FD-PTR @ dup 0 >= if
      close -1 d FS-FD-PTR !
   else
      drop
   then ;

: FS-CLOSE-WALK ( -- )
   FS-DEPTH @ begin dup 0 >= while
      dup FS-CLOSE-FD-AT
      1-
   repeat drop ;

: FS-THROW-WALK ( n -- ) {: code :}
   FS-CLOSE-WALK
   code throw ;

: FS-N@ ( -- n )
   FS-DEPTH @ cells FS-NS + @ ;

: FS-N! ( n -- ) {: n :}
   n FS-DEPTH @ cells FS-NS + ! ;

: FS-OFF@ ( -- n )
   FS-DEPTH @ cells FS-OFFS + @ ;

: FS-OFF! ( n -- ) {: off :}
   off FS-DEPTH @ cells FS-OFFS + ! ;

: FS-REC@ ( -- n )
   FS-DEPTH @ cells FS-RECS + @ ;

: FS-REC! ( n -- ) {: rec :}
   rec FS-DEPTH @ cells FS-RECS + ! ;

: FS-CHECK-JOIN-CAP ( n -- )
   dup FS-PATH-CAP > if E-FS-CAPACITY throw then drop ;

\ The length gate stays E-FS-PATH: a path longer than FS-PATH-CAP is a bad PATH,
\ the refusal every fs consumer tests for, and it is the same limit whatever
\ buffer it is copied into. What the span adds is that the copy and the NUL
\ terminator are bounds-checked against the destination's real reach, so the
\ gate is no longer the only thing between a wrong length and an overrun.
: FS-PATHZ-INTO ( ptr u8 n SPAN:span<u8> -- ptr u8 ) {: a:ptr u dst :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
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
   FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN FS-THROW-WALK then ;

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

: FS-LINUX-DIRENT-NAMELEN-SCAN ( ptr u8 n -- n ) {: ent:ptr rec :}
   0 begin dup FS-LINUX-DIRENT-NAME-OFF + rec < while
      ent over FS-LINUX-DIRENT-NAME-OFF + FS-BYTE@ 0= if exit then
      1+
   repeat
   E-FS-DIR FS-THROW-WALK ;

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

: FS-CHECK-RECORD ( -- )
   FS-REC@ 0 <= if E-FS-DIR FS-THROW-WALK then
   FS-OFF@ FS-REC@ + FS-N@ > if E-FS-DIR FS-THROW-WALK then
   FS-ENT @ FS-DIRENT-NAME-END FS-REC@ > if
      E-FS-DIR FS-THROW-WALK
   then ;

: FS-READ-DIR ( -- bool )
   FS-FD@ FS-CUR-DIR SPAN:$ FS-BASE@ getdirentries64
   dup 0 < if drop E-FS-DIR FS-THROW-WALK then
   dup FS-N! 0 > ;

\ The root goes into the depth-0 walk slot, whose reach the span carries, so the
\ hand-written FS-PATH-CAP check is gone: a root too long for the slot is
\ refused by the copy itself (E-SPAN-CAPACITY).
: FS-WALK-ROOT! ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-FS-PATH throw then
   a u FS-CUR-PATH SPAN:COPY ;

: FS-WALK-JOIN-LEN ( ptr u8 n n -- n ) {: pa:ptr pu nu :}
   pu 0 < if E-FS-PATH FS-THROW-WALK then
   nu 0 < if E-FS-PATH FS-THROW-WALK then
   pa pu nu FS-JOIN-LEN ;

: FS-CHECK-WALK-JOIN-CAP ( ptr u8 n n -- )
   FS-WALK-JOIN-LEN FS-PATH-CAP > if E-FS-CAPACITY FS-THROW-WALK then ;

: FS-CHECK-WALK-DESCEND ( -- )
   FS-DEPTH @ 1 + FS-MAX-DEPTH >= if E-FS-DEPTH FS-THROW-WALK then ;

: FS-OPEN-WALK-DIR ( ptr u8 n -- )
   FS-CHECK-WALK-DESCEND
   FS-OPEN-DIR FS-FD!
   0 FS-BASE@ ! ;

: FS-CLOSE-CUR-DIR ( -- )
   FS-FD@ close
   -1 FS-FD! ;

: FS-DIR-BLOCK-BEGIN ( -- )
   0 FS-OFF! ;

: FS-DIR-MORE? ( -- bool )
   FS-OFF@ FS-N@ < ;

\ The block offset is a running sum of kernel-supplied record lengths, so the
\ entry address is taken through the dir span: an offset past the block refuses
\ with E-SPAN-RANGE instead of naming memory beyond the buffer.
: FS-LOAD-ENTRY ( -- )
   FS-CUR-DIR FS-OFF@ SPAN:AT FS-ENT !
   FS-ENT @ FS-DIRENT-RECLEN FS-REC!
   FS-CHECK-RECORD ;

: FS-ADVANCE-ENTRY ( -- )
   FS-OFF@ FS-REC@ + FS-OFF! ;

: FS-DESCEND-PATH ( ptr u8 n ptr u8 n -- ptr u8 n ) {: pa:ptr pu na:ptr nu :}
   pa pu nu FS-CHECK-WALK-JOIN-CAP
   pa pu na nu FS-NEXT-PATH JOIN-PATH-INTO FS-CHILD-U !
   FS-DEPTH @ 1 + FS-DEPTH !
   FS-CUR-PATH FS-CHILD-U @ SPAN:TAKE SPAN:$ ;

: FS-ASCEND-PATH ( -- )
   FS-DEPTH @ 1 - FS-DEPTH ! ;

: FS-WALK-PATH ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   a u FS-SKIP-DIR? if exit then
   a u FILE? if a u q execute exit then
   a u DIR? 0= if E-FS-STAT FS-THROW-WALK then
   a u FS-OPEN-WALK-DIR
   begin FS-READ-DIR while
      FS-DIR-BLOCK-BEGIN
      begin FS-DIR-MORE? while
         FS-LOAD-ENTRY
         FS-ENT @ FS-DIRENT-NAME 2dup FS-SKIP-ENTRY? if
            2drop
         else
            a u 2swap FS-DESCEND-PATH q recurse
            FS-ASCEND-PATH
         then
         FS-ADVANCE-ENTRY
      repeat
   repeat
   FS-CLOSE-CUR-DIR ;

: WALK-FILES ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   FS-FDS-RESET
   0 FS-DEPTH !
   a u FS-WALK-ROOT!
   FS-CUR-PATH u SPAN:TAKE SPAN:$ q FS-WALK-PATH ;
