\ One native-build invocation owns the canonicalization answers and source
\ bytes used by discovery, keying and both compiler loaders. The file mappings
\ do not move while discovery scans; only path metadata uses a growable pool.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/image-lifecycle.f
require lib/engine-id.f
require tools/event-closure-lib.f

package SOURCE-VIEW

5 constant QUERY-CELLS
5 constant FILE-CELLS
3 constant LOAD-CELLS
0 constant PATH-OFF
1 constant PATH-LEN
2 constant OTHER-OFF
3 constant OTHER-LEN
4 constant LAST-CELL
0 constant LOAD-FILE
1 constant LOAD-BASE
2 constant LOAD-ACTIVE

DYNAMIC-BUFFER QUERY n
DYNAMIC-BUFFER QUERY-USE n
DYNAMIC-BUFFER FILE-ROW n
DYNAMIC-BUFFER FILE-A ptr u8
DYNAMIC-BUFFER LOAD-ROW n
DYNAMIC-BUFFER POOL u8
DYNAMIC-BUFFER SCRATCH u8

variable QUERY-N
variable QUERY-USE-N
variable FILE-N
variable LOAD-N
variable POOL-N
variable READY
variable IMAGE-HOOK-ARMED

create HASH-CTX SHA256-CTX-BYTES allot
create HASH-RAW 32 allot
create HASH-HEX 64 allot
create HASH-CELL 1 cells allot

: QUERY@ ( n n -- n ) {: id:n field:n :}
   id QUERY-CELLS * field + QUERY @ ;

: QUERY! ( n n n -- ) {: v:n id:n field:n :}
   v id QUERY-CELLS * field + QUERY ! ;

: FILE@ ( n n -- n ) {: id:n field:n :}
   id FILE-CELLS * field + FILE-ROW @ ;

: FILE! ( n n n -- ) {: v:n id:n field:n :}
   v id FILE-CELLS * field + FILE-ROW ! ;

: LOAD@ ( n n -- n ) {: id:n field:n :}
   id LOAD-CELLS * field + LOAD-ROW @ ;

: LOAD! ( n n n -- ) {: v:n id:n field:n :}
   v id LOAD-CELLS * field + LOAD-ROW ! ;

: POOL$ ( n n -- ptr u8 n ) {: off:n size:n :}
   off POOL size ;

: POOL-ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   POOL-N @ {: off:n :}
   off u + 1 max POOL-RESERVE
   a off POOL u BYTE-COPY
   off u + POOL-N !
   off ;

: QUERY-PATH$ ( n -- ptr u8 n ) {: id:n :}
   id PATH-OFF QUERY@ id PATH-LEN QUERY@ POOL$ ;

: QUERY-ANSWER$ ( n -- ptr u8 n ) {: id:n :}
   id OTHER-OFF QUERY@ id OTHER-LEN QUERY@ POOL$ ;

: FILE-PATH$ ( n -- ptr u8 n ) {: id:n :}
   id PATH-OFF FILE@ id PATH-LEN FILE@ POOL$ ;

: FILE-ROOT$ ( n -- ptr u8 n ) {: id:n :}
   id OTHER-OFF FILE@ id OTHER-LEN FILE@ POOL$ ;

: FILE-BYTES$ ( n -- ptr u8 n ) {: id:n :}
   id FILE-A @ id LAST-CELL FILE@ ;

: QUERY-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup QUERY-N @ < while
      dup QUERY-PATH$ a u STR= if exit then
      1+
   repeat drop -1 ;

: QUERY-USED ( n -- ) {: id:n :}
   QUERY-USE-N @ 1+ QUERY-USE-RESERVE
   id QUERY-USE-N @ QUERY-USE !
   1 QUERY-USE-N +! ;

: FILE-FIND ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n root:ptr rootu:n :}
   0 begin dup FILE-N @ < while
      dup FILE-PATH$ a u STR= if
         dup FILE-ROOT$ root rootu STR= if exit then
      then
      1+
   repeat drop -1 ;

: QUERY-ADD ( ptr u8 n ptr u8 n bool -- )
   {: a:ptr u:n answer:ptr answeru:n exists:bool :}
   QUERY-N @ {: id:n :}
   id 1+ QUERY-CELLS * QUERY-RESERVE
   a u POOL-ADD id PATH-OFF QUERY!
   u id PATH-LEN QUERY!
   answer answeru POOL-ADD id OTHER-OFF QUERY!
   answeru id OTHER-LEN QUERY!
   exists if 1 else 0 then id LAST-CELL QUERY!
   id 1+ QUERY-N ! ;

: FILE-ADD ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 )
   {: a:ptr u:n root:ptr rootu:n bytes:ptr size:n :}
   FILE-N @ {: id:n :}
   id 1+ FILE-CELLS * FILE-ROW-RESERVE
   id 1+ FILE-A-RESERVE
   a u POOL-ADD id PATH-OFF FILE!
   u id PATH-LEN FILE!
   root rootu POOL-ADD id OTHER-OFF FILE!
   rootu id OTHER-LEN FILE!
   size id LAST-CELL FILE!
   size 1 max MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: owned:ptr :}
   bytes owned size BYTE-COPY
   owned id FILE-A !
   id 1+ FILE-N !
   owned ;

: CANON-COLLECT ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u:n :}
   a u QUERY-FIND {: id:n :}
   id 0 >= if
      id QUERY-ANSWER$ id LAST-CELL QUERY@ 0<> exit
   then
   a u SOURCE-ROOT:CANON-OS {: answer:ptr answeru:n exists:bool :}
   a u answer answeru exists QUERY-ADD
   QUERY-N @ 1- QUERY-ANSWER$ exists ;

: CANON-LOOKUP ( ptr u8 n -- ptr u8 n bool )
   QUERY-FIND {: id:n :}
   id 0 < if E-BUILD-SOURCE throw then
   id QUERY-USED
   id QUERY-ANSWER$ id LAST-CELL QUERY@ 0<> ;

: READ-COLLECT ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: a:ptr u:n root:ptr rootu:n :}
   a u root rootu FILE-FIND {: id:n :}
   id 0 >= if id FILE-BYTES$ exit then
   INCLUDE-BUF-CAP SCRATCH-RESERVE
   a u 0 SCRATCH INCLUDE-BUF-CAP READ-ALL {: got:n :}
   a u root rootu 0 SCRATCH got FILE-ADD got ;

: READ-LOOKUP ( ptr u8 n ptr u8 n -- ptr u8 n )
   FILE-FIND {: id:n :}
   id 0 < if E-BUILD-SOURCE throw then
   id FILE-BYTES$ ;

: HASH-N ( n -- )
   HASH-CELL !
   HASH-CTX HASH-CELL CELL SHA256-FEED ;

: HASH-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u HASH-N
   HASH-CTX a u SHA256-FEED ;

: HASH-QUERIES ( -- )
   QUERY-N @ HASH-N
   QUERY-N @ 0 ?do
      i QUERY-PATH$ HASH-BYTES
      i QUERY-ANSWER$ HASH-BYTES
      i LAST-CELL QUERY@ HASH-N
   loop ;

: HASH-USED-QUERIES ( -- )
   QUERY-USE-N @ HASH-N
   QUERY-USE-N @ 0 ?do
      i QUERY-USE @ {: id:n :}
      id QUERY-PATH$ HASH-BYTES
      id QUERY-ANSWER$ HASH-BYTES
      id LAST-CELL QUERY@ HASH-N
   loop ;

: HASH-FILES ( -- )
   FILE-N @ HASH-N
   FILE-N @ 0 ?do
      i FILE-PATH$ HASH-BYTES
      i FILE-ROOT$ HASH-BYTES
      i FILE-BYTES$ HASH-BYTES
   loop ;

TRUSTED: ADDRESS ( ptr u8 -- n ) ;
TRUSTED: FRAME ( n -- ptr u8 ) ;

variable INPUT-HITS
variable INPUT-CURSOR

: INPUT-MATCH ( n n n -- ) {: base:n wanted:n cursor:n :}
   base wanted = if
      1 INPUT-HITS +!
      cursor INPUT-CURSOR !
   then ;

\ A nested evaluate saves the suspended file cursor in its evaluator frame.
\ Match each active load's own SOURCE base; include depth and evaluator depth
\ differ when a word evaluates text while a file is loading.
: INPUT-CONSUMED ( n n -- n ) {: base:n size:n :}
   0 INPUT-HITS !
   data-base SRCLOC:INB-CELL + @ base
   data-base INP-CELL + @ INPUT-MATCH
   data-base EVAL-TOP-CELL + @
   begin dup 0<> while
      dup FRAME {: fr:ptr :}
      fr EVAL-INB + CELL-VIEW @ base
      fr CELL-VIEW @ INPUT-MATCH
      fr EVAL-PREV + CELL-VIEW @ swap drop
   repeat drop
   INPUT-HITS @ 1 <> if E-BUILD-SOURCE throw then
   INPUT-CURSOR @ base - {: used:n :}
   used 0 < used size > or if E-BUILD-SOURCE throw then
   used ;

\ A load row is added before its continuation, so the final row at the unit
\ cut is the selected package input. Earlier active rows contribute only bytes
\ the interpreter has actually consumed. All bytes come from this owned view.
: HASH-LOADS ( -- )
   LOAD-N @ HASH-N
   LOAD-N @ 0 ?do
      i LOAD-FILE LOAD@ {: file:n :}
      file FILE-PATH$ HASH-BYTES
      file FILE-ROOT$ HASH-BYTES
      file FILE-BYTES$ {: bytes:ptr size:n :}
      i LOAD-N @ 1- <> i LOAD-ACTIVE LOAD@ 0<> and if
         i LOAD-BASE LOAD@ size INPUT-CONSUMED
      else size then
      bytes swap HASH-BYTES
   loop ;

: LOAD-START ( ptr u8 n ptr u8 n ptr u8 -- n )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr :}
   path pathu root rootu FILE-FIND {: file:n :}
   file 0 < if E-BUILD-SOURCE throw then
   LOAD-N @ {: id:n :}
   id 1+ LOAD-CELLS * LOAD-ROW-RESERVE
   file id LOAD-FILE LOAD!
   source ADDRESS id LOAD-BASE LOAD!
   1 id LOAD-ACTIVE LOAD!
   id 1+ LOAD-N !
   id ;

: LOAD-FINISH ( n -- ) {: id:n :}
   id 0 < id LOAD-N @ >= or if E-BUILD-SOURCE throw then
   id LOAD-ACTIVE LOAD@ 0= if E-BUILD-SOURCE throw then
   0 id LOAD-ACTIVE LOAD! ;

: LOAD-OWNED ( ptr u8 n ptr u8 n ptr u8 [ -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr q :}
   path pathu root rootu source LOAD-START {: id:n :}
   q catch {: rc:n :}
   id LOAD-FINISH
   rc 0<> if rc throw then ;

public

: CLOSE ( -- )
   SOURCE-INPUT:RESET
   SOURCE-UNIT:RESET
   0 READY !
   FILE-N @ 0 ?do
      i FILE-BYTES$ {: bytes:ptr size:n :}
      bytes size 1 max MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   loop
   QUERY-RELEASE QUERY-USE-RELEASE FILE-ROW-RELEASE FILE-A-RELEASE LOAD-ROW-RELEASE
   POOL-RELEASE SCRATCH-RELEASE
   0 QUERY-N ! 0 QUERY-USE-N ! 0 FILE-N ! 0 LOAD-N ! 0 POOL-N ! ;

: OPEN ( -- )
   CLOSE
   IMAGE-HOOK-ARMED @ 0= if
      [: CLOSE 0 IMAGE-HOOK-ARMED ! ;] IMAGE-LIFECYCLE:REGISTER
      1 IMAGE-HOOK-ARMED !
   then
   [: CANON-COLLECT ;] [: READ-COLLECT ;] SOURCE-INPUT:USE
   \ The retained loader may have cached CWD before OPEN; the fresh target has
   \ not. Both must answer its first CWD-INIT from this invocation's view.
   s" ." SOURCE-INPUT:CANON 2drop drop ;

: COLLECT ( ptr u8 n -- )
   [: READ-COLLECT ;] EC:BUILD-WITH ;

: USE ( -- )
   [: CANON-LOOKUP ;] [: READ-LOOKUP ;] SOURCE-INPUT:USE
   [: LOAD-OWNED ;] SOURCE-UNIT:USE
   1 READY ! ;

: READY? ( -- bool ) READY @ 0<> ;

: CALLBACKS ( -- [ ptr u8 n -- ptr u8 n bool ] [ ptr u8 n ptr u8 n -- ptr u8 n ] )
   [: CANON-LOOKUP ;] [: READ-LOOKUP ;] ;

: LOAD-CALLBACK ( -- [ ptr u8 n ptr u8 n ptr u8 [ -- ] -- ] )
   [: LOAD-OWNED ;] ;

: START-LOAD ( ptr u8 n ptr u8 n ptr u8 -- n ) LOAD-START ;
: FINISH-LOAD ( n -- ) LOAD-FINISH ;

: UNIT-KEY ( -- ptr u8 n )
   LOAD-N @ 0= if E-BUILD-SOURCE throw then
   LOAD-N @ 1- LOAD-ACTIVE LOAD@ 0= if E-BUILD-SOURCE throw then
   HASH-CTX SHA256-BEGIN
   3 HASH-N ENGINE-ID:KEY$ HASH-BYTES HASH-USED-QUERIES HASH-LOADS
   HASH-CTX HASH-RAW SHA256-END
   HASH-RAW HASH-HEX SHA256>HEX
   HASH-HEX 64 ;

: KEY ( -- ptr u8 n )
   HASH-CTX SHA256-BEGIN
   1 HASH-N HASH-QUERIES
   2 HASH-N HASH-FILES
   HASH-CTX HASH-RAW SHA256-END
   HASH-RAW HASH-HEX SHA256>HEX
   HASH-HEX 64 ;

;package
