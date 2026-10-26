\ word-docs.f - package WORD-DOCS: the words a Habu source text defines, each
\ with its package, section, stack effect and the comment above it, read
\ through the shared lexer (src/lib/source-lex.f, package LINT-LEX).
\
\ FILE ( ptr u8 n -- ) reads one source text and appends one record per
\ definition in source order: `: NAME`, and `EXPORT NAME` inside an open
\ package (at top level EXPORT is a build directive and defines nothing). Other
\ definers make no record. `package P` opens P, a reopened P included, in its
\ private section; `public` and `private` switch sections; `;package` closes it.
\ Record n, below COUNT:
\   PACKAGE$ ( n -- ptr u8 n )  the open package; empty at top level
\   NAME$    ( n -- ptr u8 n )  the name; for EXPORT its operand as written
\                               (`MAKI-SKETCH:XY`), whose tail the package
\                               publishes
\   EFFECT$  ( n -- ptr u8 n )  the trimmed body of a `( ... )` comment that
\                               is the next token after the name, on its line
\                               or a later one, with no `\` comment between,
\                               as the checker reads a declared effect; else
\                               empty, and empty for EXPORT
\   COMMENT$ ( n -- ptr u8 n )  the whole `\` comment lines that end on the
\                               line above the definition, each without its `\`
\                               and one space after it and trimmed at its end,
\                               joined by newlines; a blank line or code ends
\                               them; empty without one
\   PUBLIC?  ( n -- bool )      made in a package's public section
\   EXPORT?  ( n -- bool )      made by EXPORT
\ RESET ( -- ) drops every record and releases their storage. A text the lexer
\ refuses (LINT-LEX:ERROR?), or a `:`, `package` or EXPORT that the text ends
\ before naming, throws E-SOURCE; the records read before it stay until RESET.
\ A record past COUNT is E-VEC-BOUNDS, and any record after RESET E-VEC-STATE.
\
\ STORAGE CLASS. PROCESS-WIDE: one record set for the image. A record is one
\ bump allocation, its header cells and then its copied text, taken from
\ chunks of CHUNK-BYTES, or larger for a larger record, that RESET releases
\ together. The answered spans stay valid until RESET; FILE keeps none of the
\ text it was given.

require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/source-lex.f

\ LINT-LEX stays qualified: its COUNT, SOURCE and WORD collide with globals.
package WORD-DOCS
public

-9570 constant E-FIRST
-9579 constant E-LAST
-9570 constant E-SOURCE     \ the lexer refused the text, or a definer has no name

private

$10000 constant CHUNK-BYTES
1024 constant INDEX-CAP     \ first capacity of each index vector, in cells
5 constant HEAD-CELLS       \ a record's header: four text lengths, then flags
4 constant FLAGS-CELL
1 constant PUBLIC-BIT
2 constant EXPORT-BIT
40 constant LPAREN
92 constant BACKSLASH

VEC:HEADER-CELLS TYPED-BUFFER RECS ptr u8     \ each record, in source order
VEC:HEADER-CELLS TYPED-BUFFER CHUNKS ptr u8   \ each chunk; its first cell holds its length
TYPED-VARIABLE CHUNK ptr u8                   \ the chunk records are taken from
variable USED                                 \ bytes of CHUNK taken
variable ROOM                                 \ CHUNK's length; 0 when there is none

TYPED-VARIABLE SRC ptr u8                     \ the text FILE reads
variable SRC-U
TYPED-VARIABLE PKG ptr u8                     \ the open package's name, in the text
variable PKG-U                                \ 0 at top level
variable PUB                                  \ the open section is public

\ ---- storage ------------------------------------------------------------------

\ Record and chunk counts drive raw loops and COUNT, so a vector length or
\ capacity leaves its role here.
CAST: ITEM>N ( NUM:item-count -- n )

: CELL! ( n ptr u8 -- )
   CELL-VIEW ! ;

: CELL@ ( ptr u8 -- n )
   CELL-VIEW @ ;

\ The index vectors' capacity cell is their liveness token (vector.f).
: LIVE? ( -- bool )
   0 RECS VEC:CAP@ ITEM>N 0 > ;

: INIT ( -- )
   0 RECS INDEX-CAP VEC:N>ITEM VEC:INIT
   0 CHUNKS INDEX-CAP VEC:N>ITEM VEC:INIT ;

: RELEASE-CHUNKS ( -- )
   0 CHUNKS VEC:LEN@ ITEM>N 0 ?do
      0 CHUNKS i VEC:N>INDEX VEC:@ {: base:ptr :}
      base base CELL@ MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   loop ;

: ALIGNED ( n -- n )
   CELL 1- + CELL negate and ;

\ Map a chunk with room for n bytes past its length cell and take from it.
: NEW-CHUNK ( n -- ) {: need:n :}
   need CELL + CHUNK-BYTES max {: len:n :}
   len MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: base:ptr :}
   base 0 CHUNKS VEC:PUSH drop
   len base CELL!
   base CHUNK !
   CELL USED !
   len ROOM ! ;

\ n bytes at a cell boundary, which stay in place until RESET.
: TAKE ( n -- ptr u8 ) {: need:n :}
   ROOM @ USED @ ALIGNED - need < if need NEW-CHUNK then
   USED @ ALIGNED {: at:n :}
   at need + USED !
   CHUNK @ at + ;

\ ---- the text -----------------------------------------------------------------

: SRC@ ( -- ptr u8 )
   SRC @ ;

: BYTE ( n -- n )
   SRC@ + c@ ;

\ The first byte of the line holding byte i.
: LINE-START ( n -- n )
   begin dup 0 > if dup 1- BYTE STR-LF <> else false then while 1- repeat ;

\ The newline that ends the line holding byte i, or the text's end.
: LINE-END ( n -- n )
   begin dup SRC-U @ < if dup BYTE STR-LF <> else false then while 1+ repeat ;

\ The first byte in [i, e) that ends no token, or e.
: INK ( n n -- n ) {: e:n :}
   begin dup e < if dup BYTE LINT-LEX:ENGINE-DELIM? else false then while 1+ repeat ;

\ The byte past token k; a string literal's span ends before its payload.
: PAST ( n -- n ) {: k:n :}
   k LINT-LEX:LITERAL? if k LINT-LEX:CONTENT + 1+ else k LINT-LEX:TOKEN + then
   SRC@ - ;

\ ---- the comment above a definition --------------------------------------------
\ Between two tokens the text holds only delimiters and `\` comments, so a
\ whole line in that gap whose first byte past its delimiters is `\` is a
\ comment line.

\ The line above the line that starts at s lies past byte floor and is a
\ comment line.
: NOTE-ABOVE? ( n n -- bool ) {: s:n floor:n :}
   s 0= if false exit then
   s 1- LINE-START {: p:n :}
   p floor < if false exit then
   p s 1- INK {: i:n :}
   i s 1- < if i BYTE BACKSLASH = exit then
   false ;

\ The first comment line of the run that ends above the line that starts at s,
\ or s when there is none.
: NOTES-START ( n n -- n ) {: floor:n :}
   begin dup floor NOTE-ABOVE? while 1- LINE-START repeat ;

: SKIP-SPACE ( n n -- n ) {: e:n :}
   dup e < if dup BYTE STR-SPACE = if 1+ then then ;

\ The text of the comment line that starts at s, and the next line's start.
: NOTE ( n -- ptr u8 n n ) {: s:n :}
   s LINE-END {: e:n :}
   s e INK 1+ e SKIP-SPACE {: t:n :}
   SRC@ t + e t - RTRIM
   e 1+ ;

\ The joined length of the comment lines in [top, stop).
: NOTES-LEN ( n n -- n ) {: top:n stop:n :}
   top stop >= if 0 exit then
   -1 top
   begin dup stop < while
      NOTE {: u:n next:n :} drop
      u + 1+ next
   repeat drop ;

\ Copy them to dst, a newline before each line but the run's first, as
\ NOTES-LEN counts them.
: NOTES-COPY ( ptr u8 n n -- ) {: dst:ptr top:n stop:n :}
   dst top
   begin dup stop < while
      dup top <> {: later:bool :}
      NOTE {: a:ptr u:n next:n :}
      later if STR-LF over c! 1+ then
      a over u BYTE-COPY u +
      next
   repeat 2drop ;

\ ---- records ------------------------------------------------------------------

: REC ( n -- ptr u8 )
   0 RECS swap VEC:N>INDEX VEC:@ ;

: FIELD@ ( n n -- n ) {: k:n f:n :}
   k REC f cells + CELL@ ;

\ Text field f of record k: package 0, name 1, effect 2, comment 3.
: TEXT$ ( n n -- ptr u8 n ) {: k:n f:n :}
   k REC HEAD-CELLS cells +
   f 0 ?do k i FIELD@ + loop
   k f FIELD@ ;

: FLAG? ( n n -- bool ) {: k:n bit:n :}
   k FLAGS-CELL FIELD@ bit and 0<> ;

: SECTION ( -- n )
   PUB @ if PUBLIC-BIT else 0 then ;

\ Append the record of the definition token k starts, named a u, with effect
\ e eu and flags.
: ADD ( n ptr u8 n ptr u8 n n -- )
   {: k:n a:ptr u:n e:ptr eu:n flags:n :}
   k LINT-LEX:BYTE@ LINE-START {: stop:n :}
   k 0= if 0 else k 1- PAST then {: floor:n :}
   stop floor NOTES-START {: top:n :}
   top stop NOTES-LEN {: cu:n :}
   PKG-U @ u + eu + cu + {: tu:n :}
   HEAD-CELLS cells tu + TAKE {: r:ptr :}
   PKG-U @ r CELL!
   u r CELL + CELL!
   eu r 2 cells + CELL!
   cu r 3 cells + CELL!
   flags r FLAGS-CELL cells + CELL!
   r HEAD-CELLS cells + {: t:ptr :}
   PKG @ t PKG-U @ BYTE-COPY
   a t PKG-U @ + u BYTE-COPY
   e t PKG-U @ + u + eu BYTE-COPY
   t PKG-U @ + u + eu + top stop NOTES-COPY
   r 0 RECS VEC:PUSH drop ;

\ ---- the scan -----------------------------------------------------------------

: LEXED ( -- )
   LINT-LEX:ERROR? if E-SOURCE throw then ;

\ Token k parses the token after it as a name; that token's index.
: NAMED ( n -- n ) {: k:n :}
   k LINT-LEX:OPERAND LEXED
   k 1+ LINT-LEX:COUNT >= if E-SOURCE throw then
   k 1+ ;

: NO-TEXT ( -- ptr u8 n )
   SRC@ 0 ;

\ The effect after name token j: a `( ... )` comment that is the next token,
\ on its line or a later one, with no `\` comment between. The lexer drops `\`
\ comments, and only delimiters and them lie between tokens, so a byte in that
\ gap that ends no token starts one.
: EFFECT ( n -- ptr u8 n ) {: j:n :}
   j 1+ {: c:n :}
   c LINT-LEX:COUNT >= if NO-TEXT exit then
   c LINT-LEX:KIND@ LINT-LEX:COMMENT <> if NO-TEXT exit then
   c LINT-LEX:TOKEN drop c@ LPAREN <> if NO-TEXT exit then
   c LINT-LEX:BYTE@ {: e:n :}
   j PAST e INK e <> if NO-TEXT exit then
   c LINT-LEX:CONTENT TRIM ;

: COLON ( n -- ) {: k:n :}
   k NAMED {: j:n :}
   k j LINT-LEX:TOKEN j EFFECT SECTION ADD ;

: EXPORT-AT ( n -- ) {: k:n :}
   k NAMED {: j:n :}
   k j LINT-LEX:TOKEN NO-TEXT SECTION EXPORT-BIT or ADD ;

: OPEN-PKG ( n -- )
   NAMED LINT-LEX:TOKEN PKG-U ! PKG !
   false PUB ! ;

: CLOSE-PKG ( -- )
   0 PKG-U !
   false PUB ! ;

: STEP ( n -- ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   k LINT-LEX:OPERAND? if exit then
   k LINT-LEX:TOKEN {: a:ptr u:n :}
   a u s" :" STR= if k COLON exit then
   a u s" package" STR=CI if k OPEN-PKG exit then
   PKG-U @ 0= if exit then
   a u s" ;package" STR=CI if CLOSE-PKG exit then
   a u s" public" STR=CI if true PUB ! exit then
   a u s" private" STR=CI if false PUB ! exit then
   a u s" EXPORT" STR=CI if k EXPORT-AT then ;

public

: FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   LIVE? 0= if INIT then
   a SRC !  u SRC-U !
   a PKG !  CLOSE-PKG
   a u LINT-LEX:SOURCE LEXED
   0 begin dup LINT-LEX:COUNT < while dup STEP 1+ repeat drop ;

: RESET ( -- )
   LIVE? 0= if exit then
   RELEASE-CHUNKS
   0 CHUNKS VEC:DISPOSE
   0 RECS VEC:DISPOSE
   0 ROOM !
   0 USED ! ;

: PACKAGE$ ( n -- ptr u8 n )
   0 TEXT$ ;

: NAME$ ( n -- ptr u8 n )
   1 TEXT$ ;

: EFFECT$ ( n -- ptr u8 n )
   2 TEXT$ ;

: COMMENT$ ( n -- ptr u8 n )
   3 TEXT$ ;

: PUBLIC? ( n -- bool )
   PUBLIC-BIT FLAG? ;

: EXPORT? ( n -- bool )
   EXPORT-BIT FLAG? ;

\ Defined last: inside this package a bare COUNT is this word from here on.
: COUNT ( -- n )
   0 RECS VEC:LEN@ ITEM>N ;

;package
