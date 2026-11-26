\ compile-reads.f - what a compile reads besides its source: the roots it looked
\ up and what they resolved to, the environment variables it read, the texts it
\ spells, the names it looked up and the files it read.
\ A recording is one mapping of reads. Each read is three cells (kind, first
\ length, second length), the first bytes, the second bytes, then zero bytes to
\ the next cell boundary: ROOT-READ holds an absolute spelling and its canonical
\ directory, ENV-READ a name and its value, ENV-ABSENT a name and no bytes,
\ TEXT-READ a text the compile spells and no bytes, FILE-READ a path and the
\ three cells of its file's STAMP, FILE-LREAD a path and the three cells of its
\ LSTAMP, NAME-READ a name and the wordlist cell it was looked up in, CALL-READ
\ the index of a record the require check judged and the count of requires made
\ by then, each a cell. A hash of each read indexes a recording, so a repeated
\ read is found without a scan.
package COMPILE-READS
public

1 constant ROOT-READ
2 constant ENV-READ
3 constant ENV-ABSENT
4 constant TEXT-READ
5 constant FILE-READ
6 constant FILE-LREAD
7 constant NAME-READ
8 constant CALL-READ

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
EXPORT BOOT:data-base
EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
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
   a u munmap 0 < if s" compile reads: cannot release a recording" EXIT-STATUS:IOERR die then ;

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
create STAT-BUF FS-ABI:STAT-BYTES SCRATCH-ALLOT

: STAT-CELL@ ( n -- n ) STAT-BUF + CELL-VIEW @ ;

: STAT-TYPE ( -- n )
   STAT-BUF FS-ABI:STAT-MODE-OFF + {: m:^ :}
   m c@  m 1 + c@ 256 * or  FS-ABI:S-IFMT and ;

create STAMP-BUF 3 cells SCRATCH-ALLOT   \ STAMP+'s second bytes
create WID-BUF CELL SCRATCH-ALLOT        \ NAME+'s second bytes
create CALL-BUF 2 cells SCRATCH-ALLOT    \ CALL+'s first, then second bytes

\ The bytes from the start of the read at off to the start of the next.
: SPAN ( n -- n ) {: off:n :}
   off CELL + CELL@ off 2 cells + CELL@ + PAD 3 cells + ;

: SAME? ( n n str str -- bool )
   {: off:n kind:n as:str bs:str :}
   as STR:LEN bs STR:LEN {: u:n v:n :}
   off CELL@ kind =  off CELL + CELL@ u = and  off 2 cells + CELL@ v = and if
      off 3 cells + AT u >str as STR:=
      off 3 cells + u + AT v >str bs STR:= and exit
   then
   0 0= 0= ;

: HASH ( n str str -- n )
   {: kind:n as:str bs:str :}
   FNV:BASIS kind FNV:STEP  as FNV:BYTES  bs FNV:BYTES ;

: HASH-AT ( n -- n )
   {: off:n :}
   off CELL + CELL@ {: u:n :}
   off CELL@  off 3 cells + AT u >str
   off 3 cells + u + AT  off 2 cells + CELL@ >str HASH ;

\ Slot i of the index holds a read's offset plus 1, or 0 when empty.
: SLOT ( n -- ^n ) cells IDX @ + CELL-VIEW ;
: HOME ( n -- n ) SLOTS @ 1- and ;
: NEXT ( n -- n ) 1+ HOME ;

: VACANT ( n -- n )
   HOME begin dup SLOT @ 0= 0= while NEXT repeat ;

: STOP? ( n n str str -- bool )
   {: i:n kind:n as:str bs:str :}
   i SLOT @ {: s:n :}
   s 0= if 0 0= exit then
   s 1- kind as bs SAME? ;

\ The slot holding this read, else the empty slot it would take.
: PROBE ( n str str -- n )
   {: kind:n as:str bs:str :}
   kind as bs HASH HOME
   begin dup kind as bs STOP? 0= while NEXT repeat ;

: INSERT ( n -- )
   {: off:n :}
   off 1+  off HASH-AT VACANT SLOT ! ;

\ A failed map marks the recording missed and throws E-MEM-MAP carrying its
\ record.
: FRESH ( n str -- ^u8 )
   {: bytes:n m:str :}
   bytes map-anon 0= 0= if
      ST-MISSED STATUS !
      drop bytes m REJECTION:MAP-PASS
   then ;

\ APPEND grows the index before a read would fill over half its slots, so
\ VACANT and PROBE always reach an empty slot.
: GROW ( -- )
   SLOTS @ 2 * SLOTS-MIN max {: want:n :}
   want cells s" an index of a compile's reads" FRESH
   {: fresh:^ :}
   IDX @ SLOTS @ cells UNMAP
   fresh IDX !  want SLOTS !
   0 begin dup USED @ < while dup INSERT dup SPAN + repeat drop ;

: ROOM ( n -- ) {: need:n :}
   need CAP @ <= if exit then
   CAP @ 2 * need max ROOM-MIN max {: cap:n :}
   cap s" a recording of a compile's reads" FRESH
   {: fresh:^ :}
   MAP @ fresh USED @ >span ARENA-COPY
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
: NEGATIVE ( str n -- ) {: w:str n:n :}
   s" E-PATH-RANGE" REJECTION:OPEN
   96 REJECTION:EMIT1  w REJECTION:EMIT-STR
   s" ` got a length of " REJECTION:EMIT-STR  n REJECTION:EMIT-N
   REJECTION:HELP  s" pass the span's own length, 0 or more" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ w is the word the program called.
: APPEND ( str n str str -- )
   {: w:str kind:n as:str bs:str :}
   as STR:LEN bs STR:LEN {: u:n v:n :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   u 0 < if w u NEGATIVE then
   v 0 < if w v NEGATIVE then
   FILLED @ 1+ 2 * SLOTS @ > if GROW then
   kind as bs PROBE {: place:n :}
   place SLOT @ 0= 0= if exit then
   USED @ {: off:n :}
   u v + PAD 3 cells + {: size:n :}
   off size + ROOM
   kind off AT CELL-VIEW !
   u off CELL + AT CELL-VIEW !
   v off 2 cells + AT CELL-VIEW !
   as STR:DATA off 3 cells + AT u >span ARENA-COPY
   bs STR:DATA off 3 cells + u + AT v >span ARENA-COPY
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
: READS$ ( -- str )
   STATUS @ ST-OFF = if NULL-PTR 0 >str exit then
   MAP @ USED @ >str ;

\ A root's absolute spelling and its canonical directory.
: ROOT+ ( str str -- ) {: as:str bs:str :}
   s" COMPILE-READS:ROOT+" ROOT-READ as bs APPEND ;

\ An environment variable's name and value; a NULL-PTR value is an absent one.
: ENV+ ( str str -- ) {: as:str bs:str :}
   bs STR:DATA NULL-PTR = if
      s" COMPILE-READS:ENV+" ENV-ABSENT as NULL-PTR 0 >str APPEND exit
   then
   s" COMPILE-READS:ENV+" ENV-READ as bs APPEND ;

\ A text whose names the part depends on as if its source spelled them. One of
\ length 0 or less records nothing.
: TEXT+ ( str -- ) {: s:str :}
   s STR:LEN 0 <= if exit then
   s" COMPILE-READS:TEXT+" TEXT-READ s s STR:DATA 0 >str APPEND ;

\ A name looked up in a wordlist, which the part depends on as if its source
\ spelled it; the wordlist cell is the second bytes, little-endian.
: NAME+ ( str n -- ) {: s:str wid:n :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   wid WID-BUF CELL-VIEW !
   s" COMPILE-READS:NAME+" NAME-READ s WID-BUF CELL >str APPEND ;

\ The record at dictionary index idx, which the require check judged after
\ asks requires were made; both cells little-endian.
: CALL+ ( n n -- ) {: idx:n asks:n :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   idx CALL-BUF CELL-VIEW !  asks CALL-BUF CELL + CELL-VIEW !
   s" COMPILE-READS:CALL+" CALL-READ
   CALL-BUF CELL >str  CALL-BUF CELL + CELL >str APPEND ;

private

EXPORT CHECKER-RESOLVE:bare-search-wl
EXPORT CHECKER-RESOLVE:bare-scope-find
EXPORT CHECKER-RESOLVE:bare-tok-imm?

global

\ The name's code cell in wordlist wid, or 0; a dependency of the part that asks.
: search-wl ( str n -- n ) {: s:str wid:n :}
   s wid NAME+  s wid bare-search-wl ;

\ The search order's binding of the name, a dependency as the part's text is.
: scope-find ( str -- ^n ^n ^n n ) {: s:str :}
   s TEXT+  s bare-scope-find ;

\ The name's immediate flag (2) or 0, a dependency as scope-find's name is.
: tok-imm? ( str -- n ) {: s:str :}
   s TEXT+  s bare-tok-imm? ;

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

: STAMP+ ( str n str n n n -- )
   {: w:str kind:n s:str sec:n ns:n size:n :}
   sec STAMP-BUF CELL-VIEW !
   ns STAMP-BUF CELL + CELL-VIEW !
   size STAMP-BUF 2 cells + CELL-VIEW !
   w kind s STAMP-BUF 3 cells >str APPEND ;

public

\ A path a word reads and z, its NUL-terminated copy, recorded with its STAMP
\ while a recording is active; on a thread other than main, MISS.
: FILE+ ( str ^u8 -- ) {: s:str z:^ :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   s" COMPILE-READS:FILE+" FILE-READ s z STAT-BUF stat64 STAT-STAMP STAMP+ ;

\ FILE+ of a path a word reads without following a symlink at its end, which
\ LSTAMP stats.
: LFILE+ ( str ^u8 -- ) {: s:str z:^ :}
   OFF-MAIN? if MISS exit then
   STATUS @ ST-OFF = if exit then
   s" COMPILE-READS:LFILE+" FILE-LREAD s z STAT-BUF lstat64 STAT-STAMP STAMP+ ;

;package
