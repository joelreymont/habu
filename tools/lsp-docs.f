\ lsp-docs.f - the documents a language client has open: URI, path, text and
\ version as the client last sent them, and what the server's checks keep.
\ A closed document frees its buffers and its slot; an open takes the first free
\ slot. DOC-NEXT-DIRTY hands out dirty documents slot after slot, wrapping
\ round, so each one's turn comes however often another changes.
\ STORAGE CLASS. PROCESS-GLOBAL: one table, for the server's one task.

require src/lib/errors.f
require src/lib/string.f
require src/lib/byte-buffer.f
require src/lib/adt/option.f

package LSP-DOCS
using BUF

HDR-BYTES 1 cells / constant HDR-CELLS
0 constant URI-AT                       \ a slot's cells: the URI's buffer,
URI-AT HDR-CELLS + constant PATH-AT     \ the path's,
PATH-AT HDR-CELLS + constant TEXT-AT    \ the text's,
TEXT-AT HDR-CELLS + constant CANON-AT   \ the canonical path's,
CANON-AT HDR-CELLS + constant DEPS-AT   \ the published files',
DEPS-AT HDR-CELLS + constant LOADS-AT   \ the loads',
LOADS-AT HDR-CELLS + constant VERSION-AT \ the version,
VERSION-AT 1+ constant ID-AT            \ identity of this opening,
ID-AT 1+ constant DIRTY-AT              \ 1 while it waits for a check,
DIRTY-AT 1+ constant LIVE-AT            \ and 1 while the slot holds a document
LIVE-AT 1+ constant SLOT-CELLS

DYNAMIC-BUFFER SLOTS n
\ The slots ever taken. Nothing writes past them, so a slot taken new reads
\ zero, which is a buffer never initialised and a slot neither dirty nor live.
variable SLOT-COUNT
variable NEXT-ID

: SLOT-CELL ( n n -- ptr n )  swap SLOT-CELLS * + SLOTS ;
: LIVE? ( n -- bool )  LIVE-AT SLOT-CELL @ 0<> ;
: BYTES$ ( n n -- ptr u8 n )  SLOT-CELL SPAN$ BLEN>N ;

: KEEP ( ptr u8 n n n -- )
   {: a:ptr u:n slot:n at:n :}
   slot at SLOT-CELL u 1 max N>BLEN INIT
   a u N>BLEN slot at SLOT-CELL REPLACE ;

: STORE ( ptr u8 n n n -- )
   {: a:ptr u:n slot:n at:n :}
   a u N>BLEN slot at SLOT-CELL REPLACE ;

: FREE-SLOT ( -- n )
   SLOT-COUNT @ 0 ?do i LIVE? 0= if i unloop exit then loop
   SLOT-COUNT @ 1+ SLOT-CELLS * SLOTS-RESERVE
   SLOT-COUNT @
   1 SLOT-COUNT +! ;

: HOLDER ( ptr u8 n n -- option<n> )
   {: a:ptr u:n at:n :}
   SLOT-COUNT @ 0 ?do
      i LIVE? if
         i at BYTES$ a u STR= if i OPTION:SOME unloop exit then
      then
   loop
   OPTION:NONE ;

public

: DOC-SLOTS ( -- n )  SLOT-COUNT @ ;

: DOC-LIVE? ( n -- bool )  LIVE? ;

: DOC-URI$ ( n -- ptr u8 n )  URI-AT BYTES$ ;
: DOC-PATH$ ( n -- ptr u8 n )  PATH-AT BYTES$ ;
: DOC-TEXT$ ( n -- ptr u8 n )  TEXT-AT BYTES$ ;
: DOC-VERSION@ ( n -- n )  VERSION-AT SLOT-CELL @ ;
: DOC-ID@ ( n -- n )  ID-AT SLOT-CELL @ ;

\ The checker names the document by its path's canonical spelling, taken at open.
: DOC-CANON$ ( n -- ptr u8 n )  CANON-AT BYTES$ ;

\ Each path ended by LF.
: DOC-DEPS$ ( n -- ptr u8 n )  DEPS-AT BYTES$ ;
: DOC-DEPS! ( ptr u8 n n -- )  DEPS-AT STORE ;

\ The loads the last completed check reported, one JSON object per line as
\ CHECK:VERIFY-LOADS$ gives them.
: DOC-LOADS$ ( n -- ptr u8 n )  LOADS-AT BYTES$ ;
: DOC-LOADS! ( ptr u8 n n -- )  LOADS-AT STORE ;

: DOC-DIRTY ( n -- )  1 swap DIRTY-AT SLOT-CELL ! ;
: DOC-CLEAN ( n -- )  0 swap DIRTY-AT SLOT-CELL ! ;

: DOC-DIRTY? ( n -- bool )  DIRTY-AT SLOT-CELL @ 0<> ;

: DOC-DIRTY-ALL ( -- )
   SLOT-COUNT @ 0 ?do i LIVE? if i DOC-DIRTY then loop ;

\ Wraps round; -1 starts at the first slot.
: DOC-NEXT-DIRTY ( n -- option<n> )
   {: after:n :}
   SLOT-COUNT @ {: count:n :}
   count 0 ?do
      after 1+ i + count mod
      dup DOC-DIRTY? if OPTION:SOME unloop exit then
      drop
   loop
   OPTION:NONE ;

\ In slot order, skipping slot N; -1 skips none.
: DOC-EACH-BUT ( n [ ptr u8 n ptr u8 n -- ] -- )
   {: but:n each :}
   SLOT-COUNT @ 0 ?do
      i LIVE? i but <> and if i DOC-CANON$ i DOC-TEXT$ each execute then
   loop ;

: DOC-FIND ( ptr u8 n -- option<n> )  URI-AT HOLDER ;

: DOC-HOLDING ( ptr u8 n -- option<n> )  CANON-AT HOLDER ;

: DOC-OPENED-AS ( ptr u8 n -- option<n> )  PATH-AT HOLDER ;

: DOC-CLOSE ( n -- )
   {: slot:n :}
   slot URI-AT SLOT-CELL DISPOSE
   slot PATH-AT SLOT-CELL DISPOSE
   slot TEXT-AT SLOT-CELL DISPOSE
   slot CANON-AT SLOT-CELL DISPOSE
   slot DEPS-AT SLOT-CELL DISPOSE
   slot LOADS-AT SLOT-CELL DISPOSE
   slot DOC-CLEAN
   0 slot LIVE-AT SLOT-CELL ! ;

: DOC-CHANGE ( n n ptr u8 n -- )
   {: slot:n v:n t:ptr tu:n :}
   t tu slot TEXT-AT STORE
   v slot VERSION-AT SLOT-CELL !
   slot DOC-DIRTY ;

private

\ A document in a free slot. Its path's canonical spelling comes first, so a
\ path without one throws before anything changes.
: ADD ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: u:ptr uu:n v:n p:ptr pu:n t:ptr tu:n :}
   p pu SOURCE-ROOT:CANONICAL drop {: c:ptr cu:n :}
   FREE-SLOT {: slot:n :}
   u uu slot URI-AT KEEP
   p pu slot PATH-AT KEEP
   t tu slot TEXT-AT KEEP
   c cu slot CANON-AT KEEP
   NULL$ slot DEPS-AT KEEP
   NULL$ slot LOADS-AT KEEP
   v slot VERSION-AT SLOT-CELL !
   NEXT-ID @ 1+ dup NEXT-ID ! slot ID-AT SLOT-CELL !
   slot DOC-DIRTY
   1 slot LIVE-AT SLOT-CELL ! ;

public

\ Opens a document: its URI, version, path and text. A URI already open takes
\ the version and text as a change would, so each URI is held once and keeps
\ what its checks published. A path holding a NUL or longer than PATH-CAP has
\ no canonical spelling: E-PATH-NUL or E-PATH-RANGE, and nothing opens.
: DOC-OPEN ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: u:ptr uu:n v:n p:ptr pu:n t:ptr tu:n :}
   u uu DOC-FIND MATCH option
      some OF v t tu DOC-CHANGE ENDOF
      none OF u uu v p pu t tu ADD ENDOF
   ;MATCH ;

;using
;package
