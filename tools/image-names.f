\ Read native-build's optional <image>.names map. Columns are addressed by
\ name; unknown versions or malformed rows supply no name. Matching a full
\ span also checks its length, so a stale sidecar cannot label a resized body.
require lib/string.f
require lib/memory.f
require lib/fs.f
require src/habu/code-span.f

package IMAGE-NAMES

DYNAMIC-BUFFER TEXT n
create PATH FS-PATH-CAP allot
variable USED variable POS variable ROWS
variable COL-START variable COL-LEN variable COL-WID variable COL-NAME

\ The version-1 sidecar uses one space between fields; the header names columns
\ so a reordered or extended map is read by meaning, not by column position.
: FIELD$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n col:n :}
   0
   col 0 ?do
      {: start:n :}
      a u 32 start SPLIT-NEXT 0= if drop 2drop s" " unloop exit then
      {: field:ptr size:n next:n :} next
   loop
   {: start:n :} a u 32 start SPLIT-NEXT 2drop ;

: COLUMN ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n name:ptr size:n :}
   1 begin
      {: col:n :}
      a u col FIELD$ dup 0= if 2drop -1 exit then
      name size STR= if col 1- exit then
      col 1+
   again ;

: LINE$ ( -- ptr u8 n )
   0 TEXT BYTE-VIEW USED @ 10 POS @ SPLIT-NEXT
   drop POS ! ;

: NUMBER ( ptr u8 n -- n )
   STR>NUMBER? MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH ;

: ROW$ ( ptr u8 n n n -- ptr u8 n ) {: a:ptr u:n off:n want:n :}
   a u COL-START @ FIELD$ NUMBER {: start:n :}
   a u COL-LEN @ FIELD$ NUMBER {: size:n :}
   start 0 < size CODE-SPAN:VALID? 0= or if s" " exit then
   size CODE-SPAN:BYTES {: span:n :}
   want 0 < if
      off start >= off start - span < and
   else
      off start = want span = and
   then if a u COL-NAME @ FIELD$ exit then s" " ;

public

: LOAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   0 USED !
   pathu 6 + FS-PATH-CAP > if E-FS-CAPACITY throw then
   path PATH pathu BYTE-COPY
   s" .names" drop PATH pathu + 6 BYTE-COPY
   PATH pathu 6 + FILE? 0= if exit then
   PATH pathu 6 + FILE-SIZE {: size:n :}
   size CELL + CELL / TEXT-RESERVE
   PATH pathu 6 + 0 TEXT BYTE-VIEW size READ-ALL size <> if
      s" image-names: short read" 74 die then
   size USED ! 0 POS !
   LINE$ s" habu-names 1" STR= 0= if 0 USED ! exit then
   LINE$ {: header:ptr headeru:n :}
   header headeru 0 FIELD$ s" columns" STR= 0= if 0 USED ! exit then
   header headeru s" start" COLUMN COL-START !
   header headeru s" len" COLUMN COL-LEN !
   header headeru s" wid" COLUMN COL-WID !
   header headeru s" name" COLUMN COL-NAME !
   COL-START @ COL-LEN @ min COL-WID @ min COL-NAME @ min 0 < if
      0 USED ! exit then
   POS @ ROWS ! ;

: SPAN-NAME$ ( n n -- ptr u8 n ) {: off:n size:n :}
   ROWS @ POS !
   begin POS @ USED @ < while
      LINE$ off size ROW$ dup 0 > if exit then 2drop
   repeat s" " ;

: NAME$ ( n -- ptr u8 n ) -1 SPAN-NAME$ ;

;package
