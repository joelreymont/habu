\ lsp-docs.f - the documents a language client has open: each one's URI, the
\ file path the URI names, its text and its version, as the client last sent
\ them.
\
\ A document is a slot of a growable table: byte buffers (lib/byte-buffer.f)
\ for the URI, the path and the text, then the version, then 1 while the slot
\ holds a document. Closing a document frees its buffers and its slot, and an
\ open takes the first free slot. A document is found by its URI's bytes.
\
\ STORAGE CLASS. PROCESS-GLOBAL: one table, for the server's one task.

require lib/errors.f
require lib/string.f
require lib/byte-buffer.f
require lib/adt/option.f

package LSP-DOCS
using BUF

HDR-BYTES 1 cells / constant HDR-CELLS
0 constant URI-AT                       \ a slot's cells: the URI's buffer,
URI-AT HDR-CELLS + constant PATH-AT     \ the path's,
PATH-AT HDR-CELLS + constant TEXT-AT    \ the text's,
TEXT-AT HDR-CELLS + constant VERSION-AT \ the version,
VERSION-AT 1+ constant LIVE-AT          \ and 1 while the slot holds a document
LIVE-AT 1+ constant SLOT-CELLS

DYNAMIC-BUFFER SLOTS n
\ The slots ever taken. Nothing writes past them, so a slot taken new reads
\ zero, which is a buffer never initialised and a slot not live.
variable SLOT-COUNT

: SLOT-CELL ( n n -- ptr n )  swap SLOT-CELLS * + SLOTS ;
: LIVE? ( n -- bool )  LIVE-AT SLOT-CELL @ 0<> ;
: URI$ ( n -- ptr u8 n )  URI-AT SLOT-CELL SPAN$ BLEN>N ;

\ A new buffer at this cell of the slot, holding these bytes.
: KEEP ( ptr u8 n n n -- )
   {: a:ptr u:n slot:n at:n :}
   slot at SLOT-CELL u 1 max N>BLEN INIT
   a u N>BLEN slot at SLOT-CELL REPLACE ;

: FREE-SLOT ( -- n )
   SLOT-COUNT @ 0 ?do i LIVE? 0= if i unloop exit then loop
   SLOT-COUNT @ 1+ SLOT-CELLS * SLOTS-RESERVE
   SLOT-COUNT @
   1 SLOT-COUNT +! ;

public

\ The slot of the open document with this URI.
: DOC-FIND ( ptr u8 n -- option<n> )
   {: a:ptr u:n :}
   SLOT-COUNT @ 0 ?do
      i LIVE? if
         i URI$ a u STR= if i OPTION:SOME unloop exit then
      then
   loop
   OPTION:NONE ;

\ Closes the document in this slot.
: DOC-CLOSE ( n -- )
   {: slot:n :}
   slot URI-AT SLOT-CELL DISPOSE
   slot PATH-AT SLOT-CELL DISPOSE
   slot TEXT-AT SLOT-CELL DISPOSE
   0 slot LIVE-AT SLOT-CELL ! ;

\ Opens a document: its URI, version, path and text. A URI already open is
\ closed first, so each URI is held once.
: DOC-OPEN ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: u:ptr uu:n v:n p:ptr pu:n t:ptr tu:n :}
   u uu DOC-FIND MATCH option
      some OF DOC-CLOSE ENDOF
      none OF ENDOF
   ;MATCH
   FREE-SLOT {: slot:n :}
   u uu slot URI-AT KEEP
   p pu slot PATH-AT KEEP
   t tu slot TEXT-AT KEEP
   v slot VERSION-AT SLOT-CELL !
   1 slot LIVE-AT SLOT-CELL ! ;

\ The document in this slot takes a new version and text.
: DOC-CHANGE ( n n ptr u8 n -- )
   {: slot:n v:n t:ptr tu:n :}
   t tu N>BLEN slot TEXT-AT SLOT-CELL REPLACE
   v slot VERSION-AT SLOT-CELL ! ;

;using
;package
