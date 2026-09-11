\ zip-state.f - checked archive identities and owned storage.
\ Copies returned by NAME$/READ, and copied replacement inputs, remain owned by
\ the archive until COMMIT, CLOSE, or image preparation. Failed COMMIT stays live.
\ Handles are inspectable identities, validated on every operation, not pointers.
require lib/ffi-abi.f
require lib/zip-types.f
require lib/memory.f

package ZIP
using FFI
using MEM

$7FFFFFFFFFFFFFFF constant MAX-SIZE
32 constant NODE-CELLS
$40 constant STAT-BYTES
$10 constant READONLY-FLAG
$4 constant STAT-SIZE-VALID
\ LP64 zip_stat_t begins with valid, name, index, size (eight bytes each).
3 constant STAT-SIZE-CELL
create STAT-DATA STAT-BYTES allot
create EOF-BYTE 1 allot
PTR-VARIABLE ARCHIVES
variable SERIAL

: NULL? ( ptr a -- bool ) >CELL 0= ;
: LINK ( ptr n -- ptr ptr n ) 0 ptr-field ;
: ID ( ptr n -- ptr n ) 1 cells + ;
: OBJECT ( ptr n -- ptr ptr u8 ) 2 ptr-field ;
: ENTRIES ( ptr n -- ptr ptr n ) 3 ptr-field ;
: BUFFERS ( ptr n -- ptr ptr n ) 4 ptr-field ;
: MODE ( ptr n -- ptr n ) 5 cells + ;
: INDEX ( ptr n -- ptr n ) 2 cells + ;
: DATA-PTR ( ptr n -- ptr ptr u8 ) 1 ptr-field ;
: CAPACITY ( ptr n -- ptr n ) 2 cells + ;

: NODE-ALLOC ( -- ptr n ) NODE-CELLS >COUNT MEM-ALLOC-CELLS ;
: BYTES-FREE ( ptr u8 n -- ) BYTES-ALLOC-LEN RELEASE-BYTES ;
: NODE-FREE ( ptr n -- ) BYTE-VIEW NODE-CELLS cells BYTES-FREE ;

: NEXT-ID ( -- n )
   SERIAL @ MAX-SIZE = if E-SIZE throw then
   1 SERIAL +! SERIAL @ ;

: SIZE-CHECK ( n -- )
   dup 0 < if E-SIZE throw then
   MAX-SIZE >= if E-SIZE throw then ;

: FIND-ID ( n ptr n -- ptr n ) {: wanted:n head:ptr :}
   head begin dup NULL? 0= while
      dup ID @ wanted = if exit then
      LINK @
   repeat ;

: ARCHIVE-NODE ( archive -- ptr n )
   ARCHIVE>N ARCHIVES @ FIND-ID
   dup NULL? if E-HANDLE throw then ;

: ENTRY-NODE ( ptr n entry -- ptr n )
   ENTRY>N swap ENTRIES @ FIND-ID
   dup NULL? if E-ENTRY throw then ;

: ENTRY-INDEX ( ptr n entry -- n ) ENTRY-NODE INDEX @ ;

: ADD-NODE ( ptr n ptr ptr n -- ) {: node:ptr head:ptr :}
   head @ node LINK ! node head ! ;

: REGISTER ( -- archive )
   NEXT-ID NODE-ALLOC {: ident:n node:ptr :}
   ident node ID ! node ARCHIVES ADD-NODE
   ident >ARCHIVE ;

: UNREGISTER ( ptr n -- ) {: node:ptr :}
   ARCHIVES begin dup @ node <> while
      @ LINK
   repeat node LINK @ swap ! ;

: FREE-ENTRIES ( ptr n -- )
   begin dup NULL? 0= while
      dup LINK @ swap NODE-FREE
   repeat drop ;

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
   dup ENTRIES @ FREE-ENTRIES
   dup BUFFERS @ FREE-BUFFERS NODE-FREE ;

: BUFFER-ALLOC ( ptr n n -- ptr u8 ) {: archive:ptr len:n :}
   len SIZE-CHECK NODE-ALLOC {: node:ptr :}
   node archive BUFFERS ADD-NODE
   len 1 max MEM-ALLOC-BYTES {: buf:ptr cap:n :}
   buf node DATA-PTR ! cap node CAPACITY ! buf ;

: COPY-IN ( ptr n ptr u8 n -- ptr u8 n ) {: archive:ptr src:ptr len:n :}
   archive len BUFFER-ALLOC {: dst:ptr :}
   src dst len BYTE-COPY dst len ;

;using
;using
;package
