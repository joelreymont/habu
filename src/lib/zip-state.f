\ zip-state.f - checked archive identities and owned storage.
\ Copies returned by NAME$/READ, and copied replacement inputs, remain owned by
\ the archive until COMMIT or CLOSE. Failed COMMIT stays live.
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
31 constant NODE-CELLS
NODE-CELLS 1+ constant ARCHIVE-CELLS
3 constant ENTRY-CELLS
$4000 constant CHUNK-BYTES
3 cells constant CHUNK-HEADER-BYTES
$40 constant STAT-BYTES
$10 constant READONLY-FLAG
$4 constant STAT-SIZE-VALID
\ LP64 zip_stat_t begins with valid, name, index, size (eight bytes each).
3 constant STAT-SIZE-CELL
\ libzip writes these scratch bytes.
STAT-BYTES 1+ TASK:+USERS ZIP-SCRATCH u8
: STAT-DATA ( -- ^n ) 0 ZIP-SCRATCH CELL-VIEW ;
: EOF-BYTE ( -- ^u8 ) STAT-BYTES ZIP-SCRATCH ;

\ Only the short registry operations take this lock; decompression and
\ archive-owned buffers remain independent.
here data-base - negate 7 and allot
SCRATCH-VARIABLE STATE-LOCK
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
\ Carried into a built program, so no later archive or input gets a kept
\ handle's number.
variable SERIAL

: NULL? ( ^a -- bool ) >CELL 0= ;
: LINK ( ^n -- ^^n ) 0 ptr-field ;
: ID ( ^n -- ^n ) 1 cells + ;
: OBJECT ( ^n -- ^^u8 ) 2 ptr-field ;
: ENTRIES ( ^n -- ^^n ) 3 ptr-field ;
: BUFFERS ( ^n -- ^^n ) 4 ptr-field ;
: MODE ( ^n -- ^n ) 5 cells + ;
: RAW-PTR ( ^n -- ^^u8 ) 8 ptr-field ;
: RAW-LEN ( ^n -- ^n ) 9 cells + ;
: SOURCE-PTR ( ^n -- ^^u8 ) 23 ptr-field ;
: SOURCE-LEN ( ^n -- ^n ) 24 cells + ;
: SOURCE-FD ( ^n -- ^n ) 25 cells + ;
: SNAP-FD ( ^n -- ^n ) 26 cells + ;
: SOURCE-LIVE ( ^n -- ^n ) 27 cells + ;
: COPY-BUF ( ^n -- ^^u8 ) 28 ptr-field ;
: DIRECTORY-PTR ( ^n -- ^^u8 ) 29 ptr-field ;
: RAW-SCRATCH ( ^n -- ^^u8 ) 30 ptr-field ;
: CHUNKS ( ^n -- ^^n ) 31 ptr-field ;
: INDEX ( ^n -- ^n ) 2 cells + ;
: DATA-PTR ( ^n -- ^^u8 ) 1 ptr-field ;
: CAPACITY ( ^n -- ^n ) 2 cells + ;
: CHUNK-LINK ( ^n -- ^^n ) 0 ptr-field ;
: CHUNK-TOTAL ( ^n -- ^n ) 1 cells + ;
: CHUNK-USED ( ^n -- ^n ) 2 cells + ;

: NODE-ALLOC ( -- ^n ) NODE-CELLS >COUNT MEM-ALLOC-CELLS ;
: ARCHIVE-ALLOC ( -- ^n ) ARCHIVE-CELLS >COUNT MEM-ALLOC-CELLS ;
: BYTES-FREE ( ^u8 n -- ) BYTES-ALLOC-LEN RELEASE-BYTES ;
: NODE-FREE ( ^n -- ) BYTE-VIEW NODE-CELLS cells BYTES-FREE ;
: ARCHIVE-FREE ( ^n -- ) BYTE-VIEW ARCHIVE-CELLS cells BYTES-FREE ;

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

: NEW-CHUNK ( ^n n -- ^n )
   {: archive:^ size:n :}
   size CHUNK-HEADER-BYTES + CHUNK-BYTES max MEM-ALLOC-BYTES
   {: base:^ total:n :}
   base CELL-VIEW {: chunk:^ :}
   total chunk CHUNK-TOTAL !
   CHUNK-HEADER-BYTES chunk CHUNK-USED !
   archive CHUNKS @ chunk CHUNK-LINK !
   chunk archive CHUNKS ! chunk ;

: CHUNK-FITS? ( ^n n -- bool )
   {: chunk:^ size:n :}
   size chunk CHUNK-TOTAL @ chunk CHUNK-USED @ - <= ;

: CHUNK-FOR ( ^n n -- ^n )
   {: archive:^ size:n :}
   archive CHUNKS @ dup NULL? if drop archive size NEW-CHUNK exit then
   dup size CHUNK-FITS? if exit then
   drop archive size NEW-CHUNK ;

: CHUNK-RESERVE ( ^n n -- ^u8 )
   {: archive:^ len:n :}
   len CHUNK-SIZE {: size:n :}
   archive size CHUNK-FOR {: chunk:^ :}
   chunk CHUNK-USED @ {: off:n :}
   off size + chunk CHUNK-USED !
   chunk BYTE-VIEW off + ;

: FIND-ID ( n ^n -- ^n ) {: wanted:n head:^ :}
   head begin dup NULL? 0= while
      dup ID @ wanted = if exit then
      LINK @
   repeat ;

: ARCHIVE-NODE ( archive -- ^n )
   ARCHIVE>N STATE-GET ARCHIVES @ FIND-ID STATE-RELEASE
   dup NULL? if E-HANDLE throw then ;

: ENTRY-NODE ( ^n entry -- ^n )
   ENTRY>N swap ENTRIES @ FIND-ID
   dup NULL? if E-ENTRY throw then ;

: ENTRY-INDEX ( ^n entry -- n ) ENTRY-NODE INDEX @ ;

: ADD-NODE ( ^n ^^n -- ) {: node:^ head:^ :}
   head @ node LINK ! node head ! ;

: INPUT-OBJECT ( ^n -- ^^u8 ) 2 ptr-field ;
: INPUT-OWNER ( ^n -- ^^n ) 3 ptr-field ;
: INPUT-OFF ( ^n -- ^n ) 5 cells + ;
: INPUT-LEN ( ^n -- ^n ) 6 cells + ;
: INPUT-CRC ( ^n -- ^n ) 7 cells + ;
: INPUT-EXPECT ( ^n -- ^n ) 8 cells + ;
: INPUT-DATA ( ^n -- ^^u8 ) 9 ptr-field ;
: INPUT-DONE ( ^n -- ^n ) 10 cells + ;
: INPUT-LAST ( ^n -- ^n ) 11 cells + ;
: INPUT-KIND ( ^n -- ^n ) 12 cells + ;
: INPUT-FD ( ^n -- ^n ) 13 cells + ;
: INPUT-PAYLOAD ( ^n -- ^n ) 14 cells + ;
: INPUT-COMPRESSED ( ^n -- ^n ) 15 cells + ;

: INPUT-NODE ( input -- ^n )
   INPUT>N STATE-GET INPUTS @ FIND-ID STATE-RELEASE
   dup NULL? if E-HANDLE throw then ;

: REGISTER-INPUT ( ^n -- input ) {: node:^ :}
   STATE-GET node INPUTS ADD-NODE STATE-RELEASE
   node ID @ >INPUT ;

: UNREGISTER-INPUT ( ^n -- ) {: node:^ :}
   STATE-GET
   INPUTS begin dup @ node <> while @ LINK repeat
   node LINK @ swap !
   STATE-RELEASE ;

: REGISTER ( -- archive )
   NEXT-ID ARCHIVE-ALLOC {: ident:n node:^ :}
   ident node ID !
   -1 node SOURCE-FD ! -1 node SNAP-FD !
   STATE-GET node ARCHIVES ADD-NODE STATE-RELEASE
   ident >ARCHIVE ;

: UNREGISTER ( ^n -- ) {: node:^ :}
   STATE-GET
   ARCHIVES begin dup @ node <> while
      @ LINK
   repeat node LINK @ swap !
   STATE-RELEASE ;

: FREE-CHUNK ( ^n -- ^n )
   {: chunk:^ :}
   chunk CHUNK-LINK @
   chunk BYTE-VIEW chunk CHUNK-TOTAL @ BYTES-FREE ;

: FREE-CHUNKS ( ^n -- )
   begin dup NULL? 0= while FREE-CHUNK repeat drop ;

: FREE-BUFFER ( ^n -- ) {: node:^ :}
   node DATA-PTR @ NULL? 0= if
      node DATA-PTR @ node CAPACITY @ BYTES-FREE
   then node NODE-FREE ;

: FREE-BUFFERS ( ^n -- )
   begin dup NULL? 0= while
      dup LINK @ swap FREE-BUFFER
   repeat drop ;

: FORGET-ARCHIVE ( ^n -- )
   dup UNREGISTER
   dup CHUNKS @ FREE-CHUNKS
   dup BUFFERS @ FREE-BUFFERS ARCHIVE-FREE ;

: BUFFER-ALLOC ( ^n n -- ^u8 ) {: archive:^ len:n :}
   len SIZE-CHECK NODE-ALLOC {: node:^ :}
   node archive BUFFERS ADD-NODE
   len 1 max MEM-ALLOC-BYTES {: buf:^ cap:n :}
   buf node DATA-PTR ! cap node CAPACITY ! buf ;

: BUFFER-RELEASE ( ^n ^u8 -- ) {: archive:^ buf:^ :}
   archive BUFFERS
   begin dup @ NULL? 0= while
      dup @ DATA-PTR @ buf = if
         dup @ {: node:^ :}
         node LINK @ swap !
         node FREE-BUFFER exit
      then
      @ LINK
   repeat
   drop E-READ throw ;

: COPY-IN ( ^n ^u8 n -- ^u8 n ) {: archive:^ src:^ len:n :}
   archive len BUFFER-ALLOC {: dst:^ :}
   src dst len BYTE-COPY dst len ;

;using
;using
;package
