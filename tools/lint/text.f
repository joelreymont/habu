\ text.f — checked text/file helpers for native lint tools.

require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/adt/option.f                      \ option<n> finders
require src/core/exit-status.f

\ ---- whole-file read -------------------------------------------------------

: LINT-TRUE ( -- bool )
   0 0= ;

: LINT-FALSE ( -- bool )
   0 1 = ;

\ Runtime-sized whole-file storage for an input that grows: a fixed buffer dies
\ mid-run the day the file crosses its constant. A slab is three cells the
\ caller owns (backing pointer, capacity, length last read), so a tool holding
\ two files live declares two slabs.
package LINT-SLAB

$1000000 constant MAX-BYTES

: BUF-FIELD ( ^n -- ^^u8 )
   0 ptr-field ;

: BUF@ ( ^n -- ^u8 )
   BUF-FIELD @ ;

: BUF! ( ^u8 ^n -- )
   BUF-FIELD ! ;

: CAP@ ( ^n -- n ) {: s:^ :}
   s 1 cells + @ ;

: CAP! ( n ^n -- ) {: v:n s:^ :}
   v s 1 cells + ! ;

: LEN@ ( ^n -- n ) {: s:^ :}
   s 2 cells + @ ;

: LEN! ( n ^n -- ) {: v:n s:^ :}
   v s 2 cells + ! ;

: ALLOC-NEED ( n -- n )
   dup 0 <= IF drop 1 THEN ;

: ALLOC ( n ^n -- ) {: size:n s:^ :}
   size ALLOC-NEED MEM-ALLOC-64K-SPAN {: span:str :}
   s BUF@ s CAP@ {: old:^ oldcap:n :}
   span STR:LEN s CAP!
   span STR:DATA s BUF!
   old 0= 0= IF old oldcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES THEN ;

: TOO-LARGE ( str n -- ) {: path:str size:n :}
   s" lint-slab: file exceeds maximum: " type path type cr
   s" lint-slab: needed " type size .
   s" lint-slab: have " type MAX-BYTES .
   E-FS-CAPACITY throw ;

: ENSURE-CAPACITY ( str n ^n -- ) {: path:str size:n s:^ :}
   size MAX-BYTES > IF path size TOO-LARGE THEN
   s BUF@ 0= size s CAP@ > or IF size s ALLOC THEN ;

public

\ Cells a caller must reserve for one slab: `create MY-SLAB LINT-SLAB:CELLS cells allot`.
3 constant CELLS

: LOAD ( str ^n -- ) {: path:str s:^ :}
   path FS:FILE-SIZE {: size:n :}
   path size s ENSURE-CAPACITY
   path s BUF@ s CAP@ FS:READ-ALL s LEN! ;

: TEXT ( ^n -- str ) {: s:^ :}
   s BUF@ s LEN@ >str ;

;package

\ The one shared slab, for tools that hold a single source live: each file is
\ consumed before the next one loads, so a larger later file may replace the
\ backing region while smaller files reuse it.
package LINT-SOURCE

create SLAB LINT-SLAB:CELLS cells allot

public

: LOAD ( str -- )
   SLAB LINT-SLAB:LOAD ;

: TEXT ( -- str )
   SLAB LINT-SLAB:TEXT ;

;package

\ ---- string ops ------------------------------------------------------------
: LINT-STR= ( str str -- bool ) {: s:str t:str :}
   s STR:LEN t STR:LEN <> IF LINT-FALSE exit THEN
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ over t STR:DATA + c@ <> IF
         drop LINT-FALSE exit
      THEN
      1+
   repeat drop LINT-TRUE ;

: LINT-FIND-SUB ( str str -- option<n> ) {: s:str ss:str :}
   ss STR:LEN 0= IF 0 OPTION:SOME exit THEN
   0 begin dup s STR:LEN ss STR:LEN - <= while
      dup s STR:DATA +  ss STR:LEN >str  ss LINT-STR=
      IF OPTION:SOME exit THEN  1+
   repeat  drop  OPTION:NONE ;

\ ---- more string ops for the linters ----
: LINT-BMOVE ( ^u8 str -- )
   \ A negative length copies nothing.
   dup STR:DATA swap STR:LEN 0 max BYTE-COPY ;

: LINT-FOLD ( n -- n )
   dup 64 > over 91 < and IF 32 + THEN ;

: FOLD-TO ( str ^u8 -- ) {: s:str dst:^ :}
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ LINT-FOLD  over dst + c!  1+
   repeat  drop ;

13 constant LINT-CR

\ Shared split results: (offset, length) rows into the caller's source string.
\ Consumers call the public words bare through `using LINT-SPLIT`.
package LINT-SPLIT
private

$400 constant SMAX
\ Each split field's start is an address, so the starts are declared pointer
\ storage; the lengths beside them stay plain cells.
SMAX TYPED-BUFFER SOFF ^u8
create SLEN SMAX cells allot
variable CUR    \ scan cursor
variable MARK   \ start of the field being scanned

public

variable SN#
: SPLIT-CLEAR  ( -- )  0 SN# ! ;
: SPLIT+ ( str -- ) {: s:str :}
   SN# @ SMAX >= IF s" lint: split result overflow" EXIT-STATUS:FULL die THEN
   s STR:DATA SN# @ SOFF !  s STR:LEN SLEN SN# @ cells + !  SN# @ 1+ SN# ! ;

private

: ADD-LINE ( str -- ) {: s:str :}              \ one line, trailing CR dropped
   s STR:LEN 0 > IF
      s STR:DATA s STR:LEN 1- + c@ LINT-CR = IF
         s STR:DATA s STR:LEN 1- >str SPLIT+ exit
      THEN
   THEN
   s SPLIT+ ;

public

: SPLIT-LINES ( str -- ) {: s:str :}
   SPLIT-CLEAR  0 MARK !  0 CUR !
   begin CUR @ s STR:LEN < while
      s STR:DATA CUR @ + c@ 10 = IF
         s STR:DATA MARK @ +  CUR @ MARK @ - >str  ADD-LINE
         CUR @ 1+ MARK !
      THEN
      CUR @ 1+ CUR !
   repeat
   MARK @ s STR:LEN < IF
      s STR:DATA MARK @ +  s STR:LEN MARK @ - >str  ADD-LINE
   THEN ;

;package
