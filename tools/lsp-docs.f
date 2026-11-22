split-str
\ lsp-docs.f - the documents a language client has open: URI, path, text and
\ version as the client last sent them, and what the server's checks keep.
\ A closed document frees its vectors and its slot; an open takes the first free
\ slot. DOC-NEXT-DIRTY hands out dirty documents slot after slot, wrapping
\ round, so each one's turn comes however often another changes.
\ STORAGE CLASS. PROCESS-GLOBAL: one table, for the server's one task.

require src/lib/error.f
require src/lib/string.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/span.f
require src/lib/adt/option.f

package LSP-DOCS

\ A slot's id names this opening; dirty holds while the document waits for a
\ check, and live while the slot holds a document.
STRUCTURE entry DERIVE addr
   FIELD uri ^VEC:vector<u8>
   FIELD path ^VEC:vector<u8>
   FIELD text ^VEC:vector<u8>
   FIELD canon ^VEC:vector<u8>
   FIELD deps ^VEC:vector<u8>
   FIELD loads ^VEC:vector<u8>
   FIELD version n
   FIELD id n
   FIELD dirty bool
   FIELD live bool
;STRUCTURE

DYNAMIC-BUFFER SLOTS entry
\ The slots ever taken. Nothing writes past them, so a slot taken new reads
\ zero, which is null vectors and a slot neither dirty nor live.
variable SLOT-COUNT
variable NEXT-ID

: LIVE? ( n -- bool )  SLOTS ENTRY-LIVE @ ;
: BYTES$ ( ^^VEC:vector<u8> -- str )  @ VEC:SPAN SPAN:$ ;

: KEEP ( str ^^VEC:vector<u8> -- )
   {: a:^ u:n at:^ :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   v at !
   a u v VEC:APPEND ;

: STORE ( str ^^VEC:vector<u8> -- )
   {: a:^ u:n at:^ :}
   at @ 0 VEC:RESIZE
   a u at @ VEC:APPEND ;

: FREE-SLOT ( -- n )
   SLOT-COUNT @ 0 ?do i LIVE? 0= if i unloop exit then loop
   SLOT-COUNT @ 1+ SLOTS-RESERVE
   SLOT-COUNT @
   1 SLOT-COUNT +! ;

: HOLDER ( str [ ^entry -- ^^VEC:vector<u8> ] -- option<n> )
   {: a:^ u:n field :}
   SLOT-COUNT @ 0 ?do
      i LIVE? if
         i SLOTS field execute BYTES$ a u STR= if i OPTION:SOME unloop exit then
      then
   loop
   OPTION:NONE ;

public

: DOC-SLOTS ( -- n )  SLOT-COUNT @ ;

: DOC-LIVE? ( n -- bool )  LIVE? ;

: DOC-URI$ ( n -- str )  SLOTS ENTRY-URI BYTES$ ;
: DOC-PATH$ ( n -- str )  SLOTS ENTRY-PATH BYTES$ ;
: DOC-TEXT$ ( n -- ^u8 n )  SLOTS ENTRY-TEXT BYTES$ ;
: DOC-VERSION@ ( n -- n )  SLOTS ENTRY-VERSION @ ;
: DOC-ID@ ( n -- n )  SLOTS ENTRY-ID @ ;

\ The checker names the document by its path's canonical spelling, taken at open.
: DOC-CANON$ ( n -- ^u8 n )  SLOTS ENTRY-CANON BYTES$ ;

\ Each path ended by LF.
: DOC-DEPS$ ( n -- str )  SLOTS ENTRY-DEPS BYTES$ ;
: DOC-DEPS! ( str n -- )  SLOTS ENTRY-DEPS STORE ;

\ The loads the last completed check reported, one JSON object per line as
\ LSP-CHECK:VERIFY-LOADS$ gives them.
: DOC-LOADS$ ( n -- str )  SLOTS ENTRY-LOADS BYTES$ ;
: DOC-LOADS! ( str n -- )  SLOTS ENTRY-LOADS STORE ;

: DOC-DIRTY ( n -- )  true swap SLOTS ENTRY-DIRTY ! ;
: DOC-CLEAN ( n -- )  false swap SLOTS ENTRY-DIRTY ! ;

: DOC-DIRTY? ( n -- bool )  SLOTS ENTRY-DIRTY @ ;

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
: DOC-EACH-BUT ( n [ str str -- ] -- )
   {: but:n each :}
   SLOT-COUNT @ 0 ?do
      i LIVE? i but <> and if i DOC-CANON$ i DOC-TEXT$ each execute then
   loop ;

: DOC-FIND ( ^u8 n -- option<n> )  [: ENTRY-URI ;] HOLDER ;

: DOC-HOLDING ( str -- option<n> )  [: ENTRY-CANON ;] HOLDER ;

: DOC-OPENED-AS ( str -- option<n> )  [: ENTRY-PATH ;] HOLDER ;

: DOC-CLOSE ( n -- )
   {: slot:n :}
   slot SLOTS {: e:^ :}
   e ENTRY-URI @ VEC:DELETE
   e ENTRY-PATH @ VEC:DELETE
   e ENTRY-TEXT @ VEC:DELETE
   e ENTRY-CANON @ VEC:DELETE
   e ENTRY-DEPS @ VEC:DELETE
   e ENTRY-LOADS @ VEC:DELETE
   slot DOC-CLEAN
   false e ENTRY-LIVE ! ;

: DOC-CHANGE ( n n ^u8 n -- )
   {: slot:n v:n t:^ tu:n :}
   t tu slot SLOTS ENTRY-TEXT STORE
   v slot SLOTS ENTRY-VERSION !
   slot DOC-DIRTY ;

private

\ A document in a free slot. Its path's canonical spelling comes first, so a
\ path without one throws before anything changes.
: ADD ( str n str str -- )
   {: u:^ uu:n v:n p:^ pu:n t:^ tu:n :}
   p pu SOURCE-ROOT:CANONICAL drop {: c:^ cu:n :}
   FREE-SLOT {: slot:n :}
   u uu slot SLOTS ENTRY-URI KEEP
   p pu slot SLOTS ENTRY-PATH KEEP
   t tu slot SLOTS ENTRY-TEXT KEEP
   c cu slot SLOTS ENTRY-CANON KEEP
   NULL$ slot SLOTS ENTRY-DEPS KEEP
   NULL$ slot SLOTS ENTRY-LOADS KEEP
   v slot SLOTS ENTRY-VERSION !
   NEXT-ID @ 1+ dup NEXT-ID ! slot SLOTS ENTRY-ID !
   slot DOC-DIRTY
   true slot SLOTS ENTRY-LIVE ! ;

public

\ Opens a document: its URI, version, path and text. A URI already open takes
\ the version and text as a change would, so each URI is held once and keeps
\ what its checks published. A path holding a NUL, or whose canonical spelling
\ passes PATH-CAP, throws E-PATH-NUL or E-PATH-RANGE, and nothing opens.
: DOC-OPEN ( ^u8 n n ^u8 n ^u8 n -- )
   {: u:^ uu:n v:n p:^ pu:n t:^ tu:n :}
   u uu DOC-FIND MATCH option
      some OF v t tu DOC-CHANGE ENDOF
      none OF u uu v p pu t tu ADD ENDOF
   ;MATCH ;

;package
