\ zip.f - indexed ZIP reading and replacement through libzip.
\ Copies returned by NAME$/READ, and copied replacement inputs, remain owned by
\ the archive until successful COMMIT or CLOSE. COMMIT failure leaves it live.
\ Handles are inspectable identities, validated on every operation, not pointers.
require lib/zip-ffi.f
require lib/memory.f

package ZIP
using FFI
using MEM

$7FFFFFFFFFFFFFFF constant MAX-SIZE
8 constant NODE-CELLS
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

: UNLINK ( ptr n -- ) {: node:ptr :}
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
   dup UNLINK
   dup ENTRIES @ FREE-ENTRIES
   dup BUFFERS @ FREE-BUFFERS NODE-FREE ;

: DISCARD-NODE ( ptr n -- )
   dup OBJECT @ dup NULL? if drop else C-DISCARD then
   FORGET-ARCHIVE ;

: BUFFER-ALLOC ( ptr n n -- ptr u8 ) {: archive:ptr len:n :}
   len SIZE-CHECK NODE-ALLOC {: node:ptr :}
   node archive BUFFERS ADD-NODE
   len 1 max MEM-ALLOC-BYTES {: buf:ptr cap:n :}
   buf node DATA-PTR ! cap node CAPACITY ! buf ;

: COPY-IN ( ptr n ptr u8 n -- ptr u8 n ) {: archive:ptr src:ptr len:n :}
   archive len BUFFER-ALLOC {: dst:ptr :}
   src dst len BYTE-COPY dst len ;

: PATH-CHECK ( ptr u8 n -- ) {: path:ptr len:n :}
   len SIZE-CHECK len 0= if E-PATH throw then
   len 0 ?do path i + c@ 0= if E-PATH throw then loop ;

: OPEN-PATH ( archive ptr u8 n n -- archive ptr u8 n n ) {: archive:archive path:ptr len:n flags:n :}
   archive ARCHIVE-NODE {: node:ptr :}
   node len 1+ BUFFER-ALLOC {: cpath:ptr :}
   path len cpath CSTR cpath flags C-OPEN {: czip:ptr :}
   czip NULL? if E-OPEN throw then
   czip node OBJECT ! flags node MODE !
   archive path len flags ;

: OPEN-ATTEMPT ( archive ptr u8 n n -- n )
   [: OPEN-PATH ;] catch >r 2drop 2drop r> ;

: OPEN-CHECKED ( ptr u8 n n -- archive ) {: path:ptr len:n flags:n :}
   path len PATH-CHECK REGISTER {: archive:archive :}
   archive path len flags OPEN-ATTEMPT {: code:n :}
   code 0 <> if archive ARCHIVE-NODE DISCARD-NODE code throw then
   archive ;

: COUNT-NODE ( ptr n -- n )
   OBJECT @ C-COUNT dup 0 < if E-READ throw then ;

: INDEX-CHECK ( ptr n n -- ) {: node:ptr idx:n :}
   idx 0 < if E-ENTRY throw then
   idx node COUNT-NODE >= if E-ENTRY throw then ;

: NEW-ENTRY ( ptr n n -- entry ) {: archive:ptr idx:n :}
   archive idx INDEX-CHECK NEXT-ID NODE-ALLOC {: ident:n node:ptr :}
   ident node ID ! idx node INDEX ! node archive ENTRIES ADD-NODE
   ident >ENTRY ;

: CSTR-LENGTH ( ptr u8 -- n )
   0 begin 2dup + c@ 0 <> while 1+ repeat nip ;

: NAME-COPY ( ptr n n -- ptr u8 n ) {: node:ptr idx:n :}
   node OBJECT @ idx C-NAME {: name:ptr :}
   name NULL? if E-READ throw then
   node name name CSTR-LENGTH COPY-IN ;

: MEMBER-SIZE ( ptr n n -- n )
   swap OBJECT @ swap STAT-DATA C-STAT 0 <> if E-READ throw then
   STAT-DATA @ STAT-SIZE-VALID and 0= if E-READ throw then
   STAT-DATA STAT-SIZE-CELL cells + @ dup SIZE-CHECK ;

: READ-CHUNK ( ptr u8 ptr u8 n -- n )
   C-FREAD dup 0 <= if E-READ throw then ;

: READ-STEP ( n ptr u8 ptr u8 n -- n ) {: offset:n file:ptr buf:ptr len:n :}
   file buf offset + len offset - READ-CHUNK offset + ;

\ libzip verifies CRC when a read reaches EOF, including empty members.
: READ-EOF ( ptr u8 -- )
   EOF-BYTE 1 C-FREAD 0 <> if E-READ throw then ;

: READ-ALL ( ptr u8 ptr u8 n -- ptr u8 ptr u8 n ) {: file:ptr buf:ptr len:n :}
   0 begin dup len < while
      file buf len READ-STEP
   repeat drop file READ-EOF file buf len ;

: READ-ATTEMPT ( ptr u8 ptr u8 n -- n )
   [: READ-ALL ;] catch >r 2drop drop r> ;

: READ-COPY ( ptr n n -- ptr u8 n ) {: node:ptr idx:n :}
   node idx MEMBER-SIZE {: len:n :}
   node len BUFFER-ALLOC {: buf:ptr :}
   node OBJECT @ idx C-FOPEN {: file:ptr :}
   file NULL? if E-READ throw then
   file buf len READ-ATTEMPT {: code:n :}
   file C-FCLOSE 0 <> code 0 <> or if E-READ throw then
   buf len ;

: WRITABLE ( ptr n -- )
   MODE @ READONLY-FLAG and 0 <> if E-READONLY throw then ;

: INSTALL-SOURCE ( ptr n n ptr u8 -- ) {: node:ptr idx:n source:ptr :}
   source NULL? if E-WRITE throw then
   node OBJECT @ idx source C-REPLACE 0 <> if
      source C-SOURCE-FREE E-WRITE throw
   then ;

: REPLACE-COPY ( ptr n n ptr u8 n -- ) {: node:ptr idx:n data:ptr len:n :}
   node WRITABLE node data len COPY-IN {: buf:ptr size:n :}
   node OBJECT @ buf size C-SOURCE {: source:ptr :}
   node idx source INSTALL-SOURCE ;

public

: OPEN ( ptr u8 n -- archive ) READONLY-FLAG OPEN-CHECKED ;
: EDIT ( ptr u8 n -- archive ) 0 OPEN-CHECKED ;
: COUNT ( archive -- n ) ARCHIVE-NODE COUNT-NODE ;
: ENTRY ( archive n -- entry ) swap ARCHIVE-NODE swap NEW-ENTRY ;

: NAME$ ( archive entry -- ptr u8 n )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX NAME-COPY ;

: READ ( archive entry -- ptr u8 n )
   swap ARCHIVE-NODE dup rot ENTRY-INDEX READ-COPY ;

: REPLACE ( archive entry ptr u8 n -- ) {: archive:archive entry:entry data:ptr len:n :}
   archive ARCHIVE-NODE {: node:ptr :}
   node node entry ENTRY-INDEX data len REPLACE-COPY ;

: COMMIT ( archive -- )
   ARCHIVE-NODE dup OBJECT @ C-COMMIT 0 <> if E-COMMIT throw then
   FORGET-ARCHIVE ;

: CLOSE ( archive -- ) ARCHIVE-NODE DISCARD-NODE ;

;using
;using
;package
