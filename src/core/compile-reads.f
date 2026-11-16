\ compile-reads.f - what a compile reads besides its source: the roots it looked
\ up and what they resolved to, the environment variables it read, the texts it
\ evaluated and the files it read.
\ A recording is one mapping of reads. Each read is three cells (kind, first
\ length, second length), the first bytes, the second bytes, then zero bytes to
\ the next cell boundary: ROOT-READ holds an absolute spelling and its canonical
\ directory, ENV-READ a name and its value, ENV-ABSENT a name and no bytes,
\ TEXT-READ a text the compile evaluates and no bytes, FILE-READ a path and the
\ three cells of its file's STAMP, FILE-LREAD a path and the three cells of its
\ LSTAMP. A recording is indexed by a hash of each read, so a repeated read is
\ found without a scan.
package COMPILE-READS
public

1 constant ROOT-READ
2 constant ENV-READ
3 constant ENV-ABSENT
4 constant TEXT-READ
5 constant FILE-READ
6 constant FILE-LREAD

private

$1000 constant ROOM-MIN                    \ a first mapping's bytes
$200 constant SLOTS-MIN                    \ a first index's slots; a power of 2

\ The process's record, by SRCLOC:READS-CELL (hb's in a build): a recording
\ and its index. Only main writes it; other threads MISS.
0 constant REC-STATE
CELL constant REC-MAP
2 cells constant REC-CAP
3 cells constant REC-USED
4 cells constant REC-IDX
5 cells constant REC-SLOTS
6 cells constant REC-FILLED
7 cells constant REC-BYTES
0 constant ST-OFF
1 constant ST-RECORDING
2 constant ST-MISSED
create READS-REC REC-BYTES SCRATCH-ALLOT   \ a saved program starts with it empty
SRCLOC:READS-CELL RESERVED-PTR-U8-CELL READS-CELL
READS-REC READS-CELL ptr-cell!             \ a build's store goes to its image
CAST: REC-CELLS ( ^u8 -- ^^u8 )
: REC ( -- ^u8 ) READS-CELL @ ;
: STATUS ( -- ^n ) REC REC-STATE + CELL-VIEW ;
: MAP ( -- ^^u8 ) REC REC-CELLS REC-MAP CELL / ptr-field ;
: CAP ( -- ^n ) REC REC-CAP + CELL-VIEW ;
: USED ( -- ^n ) REC REC-USED + CELL-VIEW ;
: IDX ( -- ^^u8 ) REC REC-CELLS REC-IDX CELL / ptr-field ;
: SLOTS ( -- ^n ) REC REC-SLOTS + CELL-VIEW ;
: FILLED ( -- ^n ) REC REC-FILLED + CELL-VIEW ;

: SAVED ( -- n ^u8 n n ^u8 n n )
   STATUS @ MAP @ CAP @ USED @ IDX @ SLOTS @ FILLED @ ;

: RESTORE ( n ^u8 n n ^u8 n n -- )
   FILLED ! SLOTS ! IDX ! USED ! CAP ! MAP ! STATUS ! ;

: UNMAP ( ^u8 n -- ) {: a:^ u:n :}
   u 0= if exit then
   a u munmap 0 < if s" compile reads: cannot release a recording" INCLUDE-IO-RC die then ;

: EMPTY ( -- )
   NULL-PTR MAP !  0 CAP !  0 USED !
   NULL-PTR IDX !  0 SLOTS !  0 FILLED ! ;

: RELEASE ( -- )
   MAP @ CAP @ UNMAP  IDX @ SLOTS @ cells UNMAP  EMPTY ;

: PAD ( n -- n ) CELL 1- + CELL / CELL * ;
: AT ( n -- ^u8 ) MAP @ + ;
: CELL@ ( n -- n ) AT CELL-VIEW @ ;

\ This file's own stat buffer, main thread only: FILE+ and LFILE+ MISS on any
\ other, and STAMP and LSTAMP run under a load or a link.
create STAT-BUF FS-ABI:STAT-BYTES allot

: STAT-CELL@ ( n -- n ) STAT-BUF + CELL-VIEW @ ;

: STAT-TYPE ( -- n )
   STAT-BUF FS-ABI:STAT-MODE-OFF + {: m:^ :}
   m c@  m 1 + c@ 256 * or  FS-ABI:S-IFMT and ;

create STAMP-BUF 3 cells SCRATCH-ALLOT   \ STAMP+'s second bytes

\ The bytes from the start of the read at off to the start of the next.
: SPAN ( n -- n ) {: off:n :}
   off CELL + CELL@ off 2 cells + CELL@ + PAD 3 cells + ;

: SAME? ( n n ^u8 n ^u8 n -- bool )
   {: off:n kind:n a:^ u:n b:^ v:n :}
   off CELL@ kind =  off CELL + CELL@ u = and  off 2 cells + CELL@ v = and if
      off 3 cells + AT u a u CORE-STR=
      off 3 cells + u + AT v b v CORE-STR= and exit
   then
   0 0= 0= ;

: FNV+ ( n n -- n ) xor HIDX-FNV-PRIME * ;

: FNV$+ ( n ^u8 n -- n )
   {: a:^ u:n :}
   u 0 ?do a i + c@ FNV+ loop ;

: HASH ( n ^u8 n ^u8 n -- n )
   {: kind:n a:^ u:n b:^ v:n :}
   HIDX-FNV-BASIS kind FNV+  a u FNV$+  b v FNV$+ ;

: HASH-AT ( n -- n )
   {: off:n :}
   off CELL + CELL@ {: u:n :}
   off CELL@  off 3 cells + AT u
   off 3 cells + u + AT  off 2 cells + CELL@ HASH ;

\ Slot i of the index holds a read's offset plus 1, or 0 when empty.
: SLOT ( n -- ^n ) cells IDX @ + CELL-VIEW ;
: HOME ( n -- n ) SLOTS @ 1- and ;
: NEXT ( n -- n ) 1+ HOME ;

: VACANT ( n -- n )
   HOME begin dup SLOT @ 0= 0= while NEXT repeat ;

: STOP? ( n n ^u8 n ^u8 n -- bool )
   {: i:n kind:n a:^ u:n b:^ v:n :}
   i SLOT @ {: s:n :}
   s 0= if 0 0= exit then
   s 1- kind a u b v SAME? ;

\ The slot holding this read, else the empty slot it would take.
: PROBE ( n ^u8 n ^u8 n -- n )
   {: kind:n a:^ u:n b:^ v:n :}
   kind a u b v HASH HOME
   begin dup kind a u b v STOP? 0= while NEXT repeat ;

: INSERT ( n -- )
   {: off:n :}
   off 1+  off HASH-AT VACANT SLOT ! ;

: FRESH ( n ^u8 n -- ^u8 )
   {: bytes:n m:^ mu:n :}
   bytes map-anon 0= 0= if drop m mu INCLUDE-IO-RC die then ;

\ APPEND grows the index before a read would fill over half its slots, so
\ VACANT and PROBE always reach an empty slot.
: GROW ( -- )
   SLOTS @ 2 * SLOTS-MIN max {: want:n :}
   want cells s" compile reads: cannot map an index" FRESH
   {: fresh:^ :}
   IDX @ SLOTS @ cells UNMAP
   fresh IDX !  want SLOTS !
   0 begin dup USED @ < while dup INSERT dup SPAN + repeat drop ;

: ROOM ( n -- ) {: need:n :}
   need CAP @ <= if exit then
   CAP @ 2 * need max ROOM-MIN max {: cap:n :}
   cap s" compile reads: cannot map a recording" FRESH
   {: fresh:^ :}
   MAP @ fresh USED @ BYTE-COPY
   MAP @ CAP @ UNMAP
   fresh MAP !
   cap CAP ! ;

: OFF-MAIN? ( -- bool ) data-base TASK-TCB-CELL + @ 0= 0= ;

public

\ Marks a recording incomplete: a read on a thread whose DATA holds a TCB (a
\ task's or a callback's), one whose path it cannot name, or a process start.
: MISS ( -- )
   STATUS @ ST-RECORDING = if
      ST-RECORDING ST-MISSED STATUS atomic-cas drop then ;

: MISSED? ( -- bool ) STATUS @ ST-MISSED = ;

private

\ Word w, which the program called, gave a span of length n.
: NEGATIVE ( ^u8 n n -- ) {: w:^ wu:n n:n :}
   s" E-PATH-RANGE" REJECTION:OPEN
   96 REJECTION:EMIT1  w wu REJECTION:EMIT-STR
   s" ` got a length of " REJECTION:EMIT-STR  n REJECTION:EMIT-N
   REJECTION:HELP  s" pass the span's own length, 0 or more" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

\ w is the word the program called.
: APPEND ( ^u8 n n ^u8 n ^u8 n -- )
   {: w:^ wu:n kind:n a:^ u:n b:^ v:n :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   u 0 < if w wu u NEGATIVE then
   v 0 < if w wu v NEGATIVE then
   FILLED @ 1+ 2 * SLOTS @ > if GROW then
   kind a u b v PROBE {: place:n :}
   place SLOT @ 0= 0= if exit then
   USED @ {: off:n :}
   u v + PAD 3 cells + {: size:n :}
   off size + ROOM
   kind off AT CELL-VIEW !
   u off CELL + AT CELL-VIEW !
   v off 2 cells + AT CELL-VIEW !
   a off 3 cells + AT u BYTE-COPY
   b off 3 cells + u + AT v BYTE-COPY
   size 3 cells - u v + ?do 0 off 3 cells + i + AT c! loop
   off size + USED !
   off 1+ place SLOT !  FILLED @ 1+ FILLED ! ;

\ Runs q with the record swapped for a fresh one in state st.
\ The saved record stays in locals: a file q loads counts the stack's cells as
\ its own residue (E-EVAL-RESIDUE).
: SCOPE ( n [ -- ] -- )
   {: st:n q :}
   SAVED
   {: old-st:n old-map:^ old-cap:n old-used:n
      old-idx:^ old-slots:n old-filled:n :}
   st STATUS !  EMPTY
   q catch {: rc:n :}
   RELEASE
   old-st old-map old-cap old-used
   old-idx old-slots old-filled RESTORE
   rc 0= 0= if rc throw then ;

public

\ Runs q with a fresh, empty, active recording; recordings nest, and an inner
\ one's reads are not the outer one's.
: RECORD ( [ -- ] -- ) {: q :} ST-RECORDING q SCOPE ;

\ Runs q with no recording active.
: UNRECORDED ( [ -- ] -- ) {: q :} ST-OFF q SCOPE ;

\ The active recording's bytes, valid until its next append or its end.
: READS$ ( -- ^u8 n )
   STATUS @ ST-OFF = if NULL-PTR 0 exit then
   MAP @ USED @ ;

\ A root's absolute spelling and its canonical directory.
: ROOT+ ( ^u8 n ^u8 n -- ) {: a:^ u:n b:^ v:n :}
   s" COMPILE-READS:ROOT+" ROOT-READ a u b v APPEND ;

\ An environment variable's name and value; a NULL-PTR value is an absent one.
: ENV+ ( ^u8 n ^u8 n -- ) {: a:^ u:n b:^ v:n :}
   b NULL-PTR - 0= if s" COMPILE-READS:ENV+" ENV-ABSENT a u b 0 APPEND exit then
   s" COMPILE-READS:ENV+" ENV-READ a u b v APPEND ;

\ A text the compile evaluates. One of length 0 or less records nothing: `evaluate` reads no byte of it.
: TEXT+ ( ^u8 n -- ) {: a:^ u:n :}
   u 0 <= if exit then
   s" COMPILE-READS:TEXT+" TEXT-READ a u a 0 APPEND ;

private

\ The stamp of the stat whose return code is rc, which filled STAT-BUF.
: STAT-STAMP ( n -- n n n ) {: rc:n :}
   rc 0 < if 0 0 -1 exit then
   FS-ABI:STAT-CTIME-SEC-OFF STAT-CELL@
   FS-ABI:STAT-CTIME-NS-OFF STAT-CELL@  STAT-TYPE 32 lshift or
   FS-ABI:STAT-SIZE-OFF STAT-CELL@ ;

public

\ The file at the NUL-terminated path: its change time seconds, its type (mode
\ and S_IFMT) shifted 32 bits over its change time nanoseconds, and its size; 0 0
\ -1 when it is missing. A directory's change time moves when an entry is added,
\ removed or renamed.
: STAMP ( ^u8 -- n n n ) STAT-BUF stat64 STAT-STAMP ;

\ STAMP of the path itself, a symlink's own and never its target's.
: LSTAMP ( ^u8 -- n n n ) STAT-BUF lstat64 STAT-STAMP ;

\ Whether a STAMP or LSTAMP still holding means the file reads the same: true for
\ a missing file, a regular file, a directory or a symlink, whose change time
\ moves with what a read answers; false for any other kind (a FIFO, a device).
: STABLE? ( n n n -- bool )
   drop nip 32 rshift {: t:n :}
   t 0=  t FS-ABI:S-IFREG = or  t FS-ABI:S-IFDIR = or  t FS-ABI:S-IFLNK = or ;

private

: STAMP+ ( ^u8 n n ^u8 n n n n -- )
   {: w:^ wu:n kind:n a:^ u:n sec:n ns:n size:n :}
   sec STAMP-BUF CELL-VIEW !
   ns STAMP-BUF CELL + CELL-VIEW !
   size STAMP-BUF 2 cells + CELL-VIEW !
   w wu kind a u STAMP-BUF 3 cells APPEND ;

public

\ A path a word reads and z, its NUL-terminated copy, recorded with its STAMP
\ while a recording is active; on a thread other than main, MISS.
: FILE+ ( ^u8 n ^u8 -- ) {: a:^ u:n z:^ :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   s" COMPILE-READS:FILE+" FILE-READ a u z STAT-BUF stat64 STAT-STAMP STAMP+ ;

\ FILE+ of a path a word reads without following a symlink at its end, which
\ LSTAMP stats.
: LFILE+ ( ^u8 n ^u8 -- ) {: a:^ u:n z:^ :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   s" COMPILE-READS:LFILE+" FILE-LREAD a u z STAT-BUF lstat64 STAT-STAMP STAMP+ ;

;package
