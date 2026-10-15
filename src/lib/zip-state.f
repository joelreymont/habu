\ zip-state.f - checked archive identities and owned storage.
\ Copies returned by NAME$/READ, and copied replacement inputs, remain owned by
\ the archive until COMMIT, CLOSE, or image preparation. Failed COMMIT stays live.
\ Handles are inspectable identities, validated on every operation, not pointers.
\ STORAGE CLASS. Read scratch is task-local; archive buffers and entries belong
\ to their handle; the identity registry and serial are process-wide and locked.
require src/lib/ffi-abi.f
require src/lib/zip-types.f
require src/lib/memory.f
require src/lib/task.f
require src/lib/fork-child.f

package ZIP
using FFI
using MEM

$7FFFFFFFFFFFFFFF constant MAX-SIZE
34 constant NODE-CELLS
NODE-CELLS 1+ constant ARCHIVE-CELLS
3 constant ENTRY-CELLS
$4000 constant CHUNK-BYTES
3 cells constant CHUNK-HEADER-BYTES
$40 constant STAT-BYTES
$10 constant READONLY-FLAG
$4 constant STAT-SIZE-VALID
\ LP64 zip_stat_t begins with valid, name, index, size (eight bytes each).
3 constant STAT-SIZE-CELL
\ libzip writes these scratch bytes; each calling task owns its own row.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and STAT-BYTES CELL + TASK:+USER ZIP-SCRATCH drop
: STAT-DATA ( -- ptr n ) ZIP-SCRATCH ;
: EOF-BYTE ( -- ptr u8 ) ZIP-SCRATCH STAT-BYTES + BYTE-VIEW ;

\ Archive identities span tasks. Only the short registry operations take this
\ lock; decompression and archive-owned buffers remain independent.
here data-base - negate 7 and allot
variable STATE-LOCK
\ A contended PAUSE must not end a task between native disposal and unlinking.
: STATE-GET ( -- )
   TASK:DEFER-ENTER
   begin 0 1 STATE-LOCK atomic-cas 0= if exit then TASK:PAUSE again ;
: STATE-RELEASE ( -- )
   0 STATE-LOCK atomic!
   TASK:DEFER-LEAVE ;

\ fork copies the lock but not a different task that held it. The list stays
\ valid before and after each pointer publication, so the child can release it.
: CHILD-RESET ( -- ) 0 STATE-LOCK atomic! ;
: REGISTER-RESET ( -- )
   [: CHILD-RESET ;] FORK-CHILD:REGISTER ;
REGISTER-RESET

PTR-VARIABLE ARCHIVES
PTR-VARIABLE INPUTS
variable SERIAL

: NULL? ( ptr a -- bool ) >CELL 0= ;
: LINK ( ptr n -- ptr ptr n ) 0 ptr-field ;
: ID ( ptr n -- ptr n ) 1 cells + ;
: OBJECT ( ptr n -- ptr ptr u8 ) 2 ptr-field ;
: ENTRIES ( ptr n -- ptr ptr n ) 3 ptr-field ;
: BUFFERS ( ptr n -- ptr ptr n ) 4 ptr-field ;
: MODE ( ptr n -- ptr n ) 5 cells + ;
: RAW-PTR ( ptr n -- ptr ptr u8 ) 8 ptr-field ;
: RAW-LEN ( ptr n -- ptr n ) 9 cells + ;
: SOURCE-PTR ( ptr n -- ptr ptr u8 ) 26 ptr-field ;
: SOURCE-LEN ( ptr n -- ptr n ) 27 cells + ;
: SOURCE-FD ( ptr n -- ptr n ) 28 cells + ;
: SNAP-FD ( ptr n -- ptr n ) 29 cells + ;
: SOURCE-LIVE ( ptr n -- ptr n ) 30 cells + ;
: COPY-BUF ( ptr n -- ptr ptr u8 ) 31 ptr-field ;
: DIRECTORY-PTR ( ptr n -- ptr ptr u8 ) 32 ptr-field ;
: RAW-SCRATCH ( ptr n -- ptr ptr u8 ) 33 ptr-field ;
: CHUNKS ( ptr n -- ptr ptr n ) 34 ptr-field ;
: INDEX ( ptr n -- ptr n ) 2 cells + ;
: DATA-PTR ( ptr n -- ptr ptr u8 ) 1 ptr-field ;
: CAPACITY ( ptr n -- ptr n ) 2 cells + ;
: CHUNK-LINK ( ptr n -- ptr ptr n ) 0 ptr-field ;
: CHUNK-TOTAL ( ptr n -- ptr n ) 1 cells + ;
: CHUNK-USED ( ptr n -- ptr n ) 2 cells + ;

: NODE-ALLOC ( -- ptr n ) NODE-CELLS >COUNT MEM-ALLOC-CELLS ;
: ARCHIVE-ALLOC ( -- ptr n ) ARCHIVE-CELLS >COUNT MEM-ALLOC-CELLS ;
: BYTES-FREE ( ptr u8 n -- ) BYTES-ALLOC-LEN RELEASE-BYTES ;
: NODE-FREE ( ptr n -- ) BYTE-VIEW NODE-CELLS cells BYTES-FREE ;
: ARCHIVE-FREE ( ptr n -- ) BYTE-VIEW ARCHIVE-CELLS cells BYTES-FREE ;

: NEXT-ID-LOCKED ( -- n )
   SERIAL @ MAX-SIZE = if E-SIZE throw then
   1 SERIAL +! SERIAL @ ;

: NEXT-ID ( -- n )
   STATE-GET [: NEXT-ID-LOCKED ;] [: STATE-RELEASE ;] finally ;

: SIZE-CHECK ( n -- )
   dup 0 < if E-SIZE throw then
   MAX-SIZE >= if E-SIZE throw then ;

: CHUNK-SIZE ( n -- n )
   dup SIZE-CHECK
   dup 0= if E-SIZE throw then
   dup MAX-SIZE CHUNK-HEADER-BYTES - CELL 1- - > if E-SIZE throw then
   CELL 1- + CELL negate and ;

: NEW-CHUNK ( ptr n n -- ptr n )
   {: archive:ptr size:n :}
   size CHUNK-HEADER-BYTES + CHUNK-BYTES max MEM-ALLOC-BYTES
   {: base:ptr total:n :}
   base CELL-VIEW {: chunk:ptr :}
   total chunk CHUNK-TOTAL !
   CHUNK-HEADER-BYTES chunk CHUNK-USED !
   archive CHUNKS @ chunk CHUNK-LINK !
   chunk archive CHUNKS ! chunk ;

: CHUNK-FITS? ( ptr n n -- bool )
   {: chunk:ptr size:n :}
   size chunk CHUNK-TOTAL @ chunk CHUNK-USED @ - <= ;

: CHUNK-FOR ( ptr n n -- ptr n )
   {: archive:ptr size:n :}
   archive CHUNKS @ dup NULL? if drop archive size NEW-CHUNK exit then
   dup size CHUNK-FITS? if exit then
   drop archive size NEW-CHUNK ;

: CHUNK-RESERVE ( ptr n n -- ptr u8 )
   {: archive:ptr len:n :}
   len CHUNK-SIZE {: size:n :}
   archive size CHUNK-FOR {: chunk:ptr :}
   chunk CHUNK-USED @ {: off:n :}
   off size + chunk CHUNK-USED !
   chunk BYTE-VIEW off + ;

: FIND-ID ( n ptr n -- ptr n ) {: wanted:n head:ptr :}
   head begin dup NULL? 0= while
      dup ID @ wanted = if exit then
      LINK @
   repeat ;

: ARCHIVE-NODE ( archive -- ptr n )
   ARCHIVE>N STATE-GET ARCHIVES @ FIND-ID STATE-RELEASE
   dup NULL? if E-HANDLE throw then ;

: ENTRY-NODE ( ptr n entry -- ptr n )
   ENTRY>N swap ENTRIES @ FIND-ID
   dup NULL? if E-ENTRY throw then ;

: ENTRY-INDEX ( ptr n entry -- n ) ENTRY-NODE INDEX @ ;

: ADD-NODE ( ptr n ptr ptr n -- ) {: node:ptr head:ptr :}
   head @ node LINK ! node head ! ;

: INPUT-OBJECT ( ptr n -- ptr ptr u8 ) 2 ptr-field ;
: INPUT-OWNER ( ptr n -- ptr ptr n ) 3 ptr-field ;
: INPUT-OFF ( ptr n -- ptr n ) 5 cells + ;
: INPUT-LEN ( ptr n -- ptr n ) 6 cells + ;
: INPUT-CRC ( ptr n -- ptr n ) 7 cells + ;
: INPUT-EXPECT ( ptr n -- ptr n ) 8 cells + ;
: INPUT-DATA ( ptr n -- ptr ptr u8 ) 9 ptr-field ;
: INPUT-DONE ( ptr n -- ptr n ) 10 cells + ;
: INPUT-LAST ( ptr n -- ptr n ) 11 cells + ;
: INPUT-KIND ( ptr n -- ptr n ) 12 cells + ;
: INPUT-FD ( ptr n -- ptr n ) 13 cells + ;

: INPUT-NODE ( input -- ptr n )
   INPUT>N STATE-GET INPUTS @ FIND-ID STATE-RELEASE
   dup NULL? if E-HANDLE throw then ;

: REGISTER-INPUT ( ptr n -- input ) {: node:ptr :}
   STATE-GET node INPUTS ADD-NODE STATE-RELEASE
   node ID @ >INPUT ;

: UNREGISTER-INPUT ( ptr n -- ) {: node:ptr :}
   STATE-GET
   INPUTS begin dup @ node <> while @ LINK repeat
   node LINK @ swap !
   STATE-RELEASE ;

: REGISTER ( -- archive )
   NEXT-ID ARCHIVE-ALLOC {: ident:n node:ptr :}
   ident node ID !
   -1 node SOURCE-FD ! -1 node SNAP-FD !
   STATE-GET node ARCHIVES ADD-NODE STATE-RELEASE
   ident >ARCHIVE ;

: UNREGISTER ( ptr n -- ) {: node:ptr :}
   STATE-GET
   ARCHIVES begin dup @ node <> while
      @ LINK
   repeat node LINK @ swap !
   STATE-RELEASE ;

: FREE-CHUNK ( ptr n -- ptr n )
   {: chunk:ptr :}
   chunk CHUNK-LINK @
   chunk BYTE-VIEW chunk CHUNK-TOTAL @ BYTES-FREE ;

: FREE-CHUNKS ( ptr n -- )
   begin dup NULL? 0= while FREE-CHUNK repeat drop ;

: FREE-BUFFER ( ptr n -- ) {: node:ptr :}
   node DATA-PTR @ NULL? 0= if
      node DATA-PTR @ node CAPACITY @ BYTES-FREE
   then node NODE-FREE ;

: FREE-BUFFERS ( ptr n -- )
   begin dup NULL? 0= while
      dup LINK @ swap FREE-BUFFER
   repeat drop ;

: FORGET-ARCHIVE ( ptr n -- )
   dup UNREGISTER
   dup CHUNKS @ FREE-CHUNKS
   dup BUFFERS @ FREE-BUFFERS ARCHIVE-FREE ;

: BUFFER-ALLOC ( ptr n n -- ptr u8 ) {: archive:ptr len:n :}
   len SIZE-CHECK NODE-ALLOC {: node:ptr :}
   node archive BUFFERS ADD-NODE
   len 1 max MEM-ALLOC-BYTES {: buf:ptr cap:n :}
   buf node DATA-PTR ! cap node CAPACITY ! buf ;

: BUFFER-RELEASE ( ptr n ptr u8 -- ) {: archive:ptr buf:ptr :}
   archive BUFFERS
   begin dup @ NULL? 0= while
      dup @ DATA-PTR @ buf = if
         dup @ {: node:ptr :}
         node LINK @ swap !
         node FREE-BUFFER exit
      then
      @ LINK
   repeat
   drop E-READ throw ;

: COPY-IN ( ptr n ptr u8 n -- ptr u8 n ) {: archive:ptr src:ptr len:n :}
   archive len BUFFER-ALLOC {: dst:ptr :}
   src dst len BYTE-COPY dst len ;

;using
;using
;package
