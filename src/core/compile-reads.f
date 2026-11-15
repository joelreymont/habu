\ compile-reads.f - what a compile reads besides its source: the roots it looked
\ up and what they resolved to, the environment variables it read, the texts it
\ evaluated and the files it read.
\ A recording is one mapping of reads. Each read is three cells (kind, first
\ length, second length), the first bytes, the second bytes, then zero bytes to
\ the next cell boundary: ROOT-READ holds an absolute spelling and its canonical
\ directory, ENV-READ a name and its value, ENV-ABSENT a name and no bytes,
\ TEXT-READ a text the compile evaluates and no bytes, FILE-READ a path and the
\ three cells of its file's STAMP, FILE-LREAD a path and the three cells of its
\ LSTAMP.
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

\ The process's record, by SRCLOC:READS-CELL (hb's in a build): its state,
\ mapping, capacity and bytes used. Only main writes it; other threads MISS.
0 constant REC-STATE
CELL constant REC-MAP
2 cells constant REC-CAP
3 cells constant REC-USED
4 cells constant REC-BYTES
0 constant ST-OFF
1 constant ST-RECORDING
2 constant ST-MISSED
create READS-REC REC-BYTES SCRATCH-ALLOT   \ a saved program starts with it empty
SRCLOC:READS-CELL RESERVED-PTR-U8-CELL READS-CELL
READS-REC READS-CELL ptr-cell!             \ a build's store goes to its image
CAST: REC-CELLS ( ptr u8 -- ptr ptr u8 )
: REC ( -- ptr u8 ) READS-CELL @ ;
: STATUS ( -- ptr n ) REC REC-STATE + CELL-VIEW ;
: MAP ( -- ptr ptr u8 ) REC REC-CELLS REC-MAP CELL / ptr-field ;
: CAP ( -- ptr n ) REC REC-CAP + CELL-VIEW ;
: USED ( -- ptr n ) REC REC-USED + CELL-VIEW ;

: SAVED ( -- n ptr u8 n n ) STATUS @ MAP @ CAP @ USED @ ;
: RESTORE ( n ptr u8 n n -- ) USED ! CAP ! MAP ! STATUS ! ;

: UNMAP ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   a u munmap 0 < if s" compile reads: cannot release a recording" INCLUDE-IO-RC die then ;

: RELEASE ( -- )
   MAP @ CAP @ UNMAP
   NULL-PTR MAP !  0 CAP !  0 USED ! ;

: PAD ( n -- n ) CELL 1- + CELL / CELL * ;
: AT ( n -- ptr u8 ) MAP @ + ;
: CELL@ ( n -- n ) AT CELL-VIEW @ ;

\ This file's own stat buffer, main thread only: FILE+ and LFILE+ MISS on any
\ other, and STAMP and LSTAMP run under a load or a link.
create STAT-BUF FS-ABI:STAT-BYTES allot

: STAT-CELL@ ( n -- n ) STAT-BUF + CELL-VIEW @ ;

: STAT-TYPE ( -- n )
   STAT-BUF FS-ABI:STAT-MODE-OFF + {: m:ptr :}
   m c@  m 1 + c@ 256 * or  FS-ABI:S-IFMT and ;

create STAMP-BUF 3 cells SCRATCH-ALLOT   \ STAMP+'s second bytes

\ The bytes from the start of the read at off to the start of the next.
: SPAN ( n -- n ) {: off:n :}
   off CELL + CELL@ off 2 cells + CELL@ + PAD 3 cells + ;

: SAME? ( n n ptr u8 n ptr u8 n -- bool )
   {: off:n kind:n a:ptr u:n b:ptr v:n :}
   off CELL@ kind =  off CELL + CELL@ u = and  off 2 cells + CELL@ v = and if
      off 3 cells + AT u a u CORE-STR=
      off 3 cells + u + AT v b v CORE-STR= and exit
   then
   0 0= 0= ;

: SEEN? ( n ptr u8 n ptr u8 n -- bool )
   {: kind:n a:ptr u:n b:ptr v:n :}
   0 begin dup USED @ < while
      dup kind a u b v SAME? if drop 0 0= exit then
      dup SPAN +
   repeat drop 0 0= 0= ;

: ROOM ( n -- ) {: need:n :}
   need CAP @ <= if exit then
   CAP @ 2 * need max ROOM-MIN max {: cap:n :}
   cap map-anon 0= 0= if drop s" compile reads: cannot map a recording" INCLUDE-IO-RC die then
   {: fresh:ptr :}
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

: APPEND ( n ptr u8 n ptr u8 n -- )
   {: kind:n a:ptr u:n b:ptr v:n :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   u 0 < v 0 < or if s" compile reads: negative length" INCLUDE-IO-RC die then
   kind a u b v SEEN? if exit then
   USED @ {: off:n :}
   u v + PAD 3 cells + {: size:n :}
   off size + ROOM
   kind off AT CELL-VIEW !
   u off CELL + AT CELL-VIEW !
   v off 2 cells + AT CELL-VIEW !
   a off 3 cells + AT u BYTE-COPY
   b off 3 cells + u + AT v BYTE-COPY
   size 3 cells - u v + ?do 0 off 3 cells + i + AT c! loop
   off size + USED ! ;

\ Runs q with the record swapped for a fresh one in state st.
: SCOPE ( n [ -- ] -- )
   {: st:n q :}
   SAVED {: old-st:n old-map:ptr old-cap:n old-used:n :}
   st STATUS !  NULL-PTR MAP !  0 CAP !  0 USED !
   q catch {: rc:n :}
   RELEASE old-st old-map old-cap old-used RESTORE
   rc 0= 0= if rc throw then ;

public

\ Runs q with a fresh, empty, active recording; recordings nest, and an inner
\ one's reads are not the outer one's.
: RECORD ( [ -- ] -- ) {: q :} ST-RECORDING q SCOPE ;

\ Runs q with no recording active.
: UNRECORDED ( [ -- ] -- ) {: q :} ST-OFF q SCOPE ;

\ The active recording's bytes, valid until its next append or its end.
: READS$ ( -- ptr u8 n )
   STATUS @ ST-OFF = if NULL-PTR 0 exit then
   MAP @ USED @ ;

\ A root's absolute spelling and its canonical directory.
: ROOT+ ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   ROOT-READ a u b v APPEND ;

\ An environment variable's name and value; a NULL-PTR value is an absent one.
: ENV+ ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   b NULL-PTR - 0= if ENV-ABSENT a u b 0 APPEND exit then
   ENV-READ a u b v APPEND ;

\ A text the compile evaluates. One of length 0 or less records nothing: `evaluate` reads no byte of it.
: TEXT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if exit then
   TEXT-READ a u a 0 APPEND ;

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
: STAMP ( ptr u8 -- n n n ) STAT-BUF stat64 STAT-STAMP ;

\ STAMP of the path itself, a symlink's own and never its target's.
: LSTAMP ( ptr u8 -- n n n ) STAT-BUF lstat64 STAT-STAMP ;

\ Whether a STAMP or LSTAMP still holding means the file reads the same: true for
\ a missing file, a regular file, a directory or a symlink, whose change time
\ moves with what a read answers; false for any other kind (a FIFO, a device).
: STABLE? ( n n n -- bool )
   drop nip 32 rshift {: t:n :}
   t 0=  t FS-ABI:S-IFREG = or  t FS-ABI:S-IFDIR = or  t FS-ABI:S-IFLNK = or ;

private

: STAMP+ ( n ptr u8 n n n n -- ) {: kind:n a:ptr u:n sec:n ns:n size:n :}
   sec STAMP-BUF CELL-VIEW !
   ns STAMP-BUF CELL + CELL-VIEW !
   size STAMP-BUF 2 cells + CELL-VIEW !
   kind a u STAMP-BUF 3 cells APPEND ;

public

\ A path a word reads and z, its NUL-terminated copy, recorded with its STAMP
\ while a recording is active; on a thread other than main, MISS.
: FILE+ ( ptr u8 n ptr u8 -- ) {: a:ptr u:n z:ptr :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   FILE-READ a u z STAT-BUF stat64 STAT-STAMP STAMP+ ;

\ FILE+ of a path a word reads without following a symlink at its end, which
\ LSTAMP stats.
: LFILE+ ( ptr u8 n ptr u8 -- ) {: a:ptr u:n z:ptr :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   FILE-LREAD a u z STAT-BUF lstat64 STAT-STAMP STAMP+ ;

;package
