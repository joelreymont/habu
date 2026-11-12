\ load-graph.f - the load graph of the registry's rows: every file a row loads
\ and every file those load, each read once and named by its canonical path,
\ with the imports and launches it makes.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/test/suite.f
require tools/lint/text.f
require src/lib/source-lex.f
require test/load-refs.f

package LOAD-GRAPH

$2000 constant HASH-CAP                  \ slots, a power of two; half may fill

DYNAMIC-BUFFER PATH-BYTES u8
DYNAMIC-BUFFER PATH-OFF n
DYNAMIC-BUFFER PATH-LEN n
DYNAMIC-BUFFER EDGE-OFF n                \ its first edge
DYNAMIC-BUFFER EDGE-CNT n
DYNAMIC-BUFFER EDGES n                   \ each file's loads, back to back
DYNAMIC-BUFFER EDGE-LINE n               \ the line naming each load
DYNAMIC-BUFFER EDGE-IMPORT bool          \ TRUE for an import, FALSE for a launch
create HASH HASH-CAP cells allot         \ file id + 1 per slot; 0 is empty

variable PATH-U
variable FILE-N
variable EDGE-N
variable PROBE

: FILE$ ( n -- ptr u8 n ) {: id:n :}
   0 PATH-BYTES id PATH-OFF @ +
   id PATH-LEN @ ;

: FILE-ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   FILE-N @ {: id:n :}
   PATH-U @ u + PATH-BYTES-RESERVE
   id 1+ PATH-OFF-RESERVE
   id 1+ PATH-LEN-RESERVE
   id 1+ EDGE-OFF-RESERVE
   id 1+ EDGE-CNT-RESERVE
   a 0 PATH-BYTES PATH-U @ + u BYTE-COPY
   PATH-U @ id PATH-OFF !
   u id PATH-LEN !
   0 id EDGE-OFF !
   0 id EDGE-CNT !
   PATH-U @ u + PATH-U !
   id 1+ FILE-N !
   id ;

: HASH-SLOT ( n -- ptr n )
   cells HASH + ;

: FNV ( ptr u8 n -- n ) {: a:ptr u:n :}
   $CBF29CE484222325 u 0 ?do a i + c@ xor $100000001B3 * loop ;

: SLOT-ID ( -- n )
   PROBE @ HASH-SLOT @ ;

\ Probe from a canonical path's hash to the slot holding it, or to the empty
\ slot it would take.
: PROBE! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FNV HASH-CAP 1- and PROBE !
   begin SLOT-ID 0<> while
      SLOT-ID 1- FILE$ a u STR= if exit then
      PROBE @ 1+ HASH-CAP 1- and PROBE !
   repeat ;

: INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u PROBE!
   SLOT-ID 0<> if SLOT-ID 1- exit then
   FILE-N @ HASH-CAP 2 / >= if
      s" load graph: more files than HASH-CAP holds" type cr
      E-TBL-BOUNDS throw
   then
   a u FILE-ADD {: id:n :}
   id 1+ PROBE @ HASH-SLOT !
   id ;

: GRAPH-RESET ( -- )
   0 PATH-U ! 0 FILE-N ! 0 EDGE-N !
   HASH-CAP 0 ?do 0 i HASH-SLOT ! loop ;

: EDGE+ ( n n bool -- ) {: id:n line:n import:bool :}
   EDGE-N @ 1+ EDGES-RESERVE
   EDGE-N @ 1+ EDGE-LINE-RESERVE
   EDGE-N @ 1+ EDGE-IMPORT-RESERVE
   id EDGE-N @ EDGES !
   line EDGE-N @ EDGE-LINE !
   import EDGE-N @ EDGE-IMPORT !
   EDGE-N @ 1+ EDGE-N ! ;

\ A reference names its file from the tree root; a path that names no file is
\ not an edge, and one holding a 0 byte names none. A launch is followed only
\ into a Forth source: a launched script or data file is not lexed.
: REF ( ptr u8 n n n -- )
   {: a:ptr u:n line:n kind:n :}
   kind LOAD-REFS:LAUNCHES <> {: import:bool :}
   a u 0 INDEX-OF MATCH option
      some OF drop exit ENDOF
      none OF ENDOF
   ;MATCH
   a u FS:FILE? 0= if exit then
   a u SOURCE-ROOT:CANONICAL drop {: path:ptr pathu:n :}
   import 0= path pathu s" .f" ENDS-WITH? 0= and if exit then
   path pathu INTERN line import EDGE+ ;

: SCAN ( n -- ) {: id:n :}
   EDGE-N @ id EDGE-OFF !
   id FILE$ LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   LINT-LEX:ERROR? if
      s" load graph: cannot lex " type id FILE$ type cr
      E-SUITE-ROW throw
   then
   [: REF ;] LOAD-REFS:EACH
   EDGE-N @ id EDGE-OFF @ - id EDGE-CNT ! ;

\ A file a row loads; a path that names no file is not read.
: ROW-FILE ( n bool ptr u8 n -- )
   {: row:n entry:bool a:ptr u:n :}
   a u FS:FILE? if a u SOURCE-ROOT:CANONICAL drop INTERN drop then ;

public

\ Files are numbered from 0 in the order they are found, so a scan's new files
\ come after it and each is scanned once. Call it once the registry and its
\ TEST:ONLY choice are complete, before the readers below.
: READ ( -- )
   GRAPH-RESET
   [: ROW-FILE ;] TEST:VISIT-LOAD-FILES
   0 begin dup FILE-N @ < while dup SCAN 1+ repeat drop ;

\ A file's loads are EDGE-COUNT consecutive edges from EDGE-FIRST, in source
\ order; each names the file loaded, the line naming it and whether it is an
\ import or a launch.

: FILE-COUNT ( -- n )
   FILE-N @ ;

\ The id of a file READ read, by any path naming it. Any other path is refused:
\ a file the walk never read has no loads to trust.
: FILE-ID ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u SOURCE-ROOT:CANONICAL drop PROBE!
   SLOT-ID 0<> if SLOT-ID 1- exit then
   s" load graph: not a file of the load graph: " type a u type cr
   E-SUITE-ROW throw ;

EXPORT FILE$

: EDGE-FIRST ( n -- n )
   EDGE-OFF @ ;

: EDGE-COUNT ( n -- n )
   EDGE-CNT @ ;

: EDGE-TO ( n -- n )
   EDGES @ ;

: EDGE-LINE@ ( n -- n )
   EDGE-LINE @ ;

: EDGE-IMPORT? ( n -- bool )
   EDGE-IMPORT @ ;

;package
