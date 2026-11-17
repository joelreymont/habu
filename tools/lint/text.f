\ text.f — checked text/file helpers for native lint tools.

require src/lib/string.f
require src/lib/string-roles.f               \ package STR: the typed string surface
require src/lib/memory.f
require src/lib/fs.f
require src/lib/adt/option.f                      \ option<n> finders
require src/core/exit-status.f

\ ---- whole-file read -------------------------------------------------------
create READ-PROBE 1 allot
variable RFD  variable RGOT  variable RLEN
TYPED-VARIABLE LINT-OUT-A ^u8
variable LINT-OUT-CAP
variable LINT-OUT-U
variable LINT-OUT-ON

: LINT-OUT-A@ ( -- ^u8 )
   LINT-OUT-A @ ;

: LINT-OUT-A! ( ^u8 -- )
   LINT-OUT-A ! ;

: LINT-TRUE ( -- bool )
   0 0= ;

: LINT-FALSE ( -- bool )
   0 1 = ;

: LINT-NOT ( bool -- bool )
   IF LINT-FALSE ELSE LINT-TRUE THEN ;

: LINT-OUT-BUFFER! ( ^u8 n -- ) {: a:^ cap:n :}
   a LINT-OUT-A!
   cap LINT-OUT-CAP !
   0 LINT-OUT-U !
   LINT-TRUE LINT-OUT-ON ! ;

: LINT-OUT-BUFFER-OFF ( -- )
   LINT-FALSE LINT-OUT-ON ! ;

: LINT-OUT$ ( -- ^u8 n )
   LINT-OUT-A@ LINT-OUT-U @ ;

: LINT-FD-WRITE ( n ^u8 n -- ) {: fd:n a:^ u:n :}
   u 0= IF exit THEN
   fd a u write u <> IF s" lint: write failed" EXIT-STATUS:IOERR die THEN ;

: LINT-OUT-WRITE ( n ^u8 n -- ) {: fd:n a:^ u:n :}
   LINT-OUT-ON @ IF
      a u STR:LENGTH LINT-OUT-A@ LINT-OUT-CAP @ STR:LENGTH LINT-OUT-U STR:BUF-APPEND
      exit
   THEN
   fd a u LINT-FD-WRITE ;

\ capacity/IO exits must name the offending file — a bare "file exceeds buffer"
\ hides which source outgrew which lint buffer.
: LINT-READ-DIE ( ^u8 n ^u8 n n -- )
   {: msg:^ msgu:n path:^ pathu:n rc:n :}
   2 msg msgu LINT-FD-WRITE
   path pathu rc die ;

\ open takes a NUL-terminated path, so FS:>PATHZ copies the Habu path first.
: READ-FILE ( ^u8 n ^u8 n -- ^u8 n )
   {: a:^ u buf:^ cap :}
   a u FS:>PATHZ 0 0 open  RFD !
   RFD @ 0 < IF s" lint: cannot open file: " a u EXIT-STATUS:IOERR LINT-READ-DIE THEN
   0 RLEN !
   begin  RFD @  buf RLEN @ +  cap RLEN @ -  read  dup RGOT !  0 >
   while  RLEN @ RGOT @ + RLEN !  repeat
   RGOT @ 0 < IF RFD @ close s" lint: cannot read file: " a u EXIT-STATUS:IOERR LINT-READ-DIE THEN
   RLEN @ cap = IF
      RFD @ READ-PROBE 1 read RGOT !
      RGOT @ 0 < IF RFD @ close s" lint: cannot read file: " a u EXIT-STATUS:IOERR LINT-READ-DIE THEN
      RGOT @ 0 > IF RFD @ close s" lint: file exceeds buffer: " a u EXIT-STATUS:FULL LINT-READ-DIE THEN
   THEN
   RFD @ close
   buf RLEN @ ;

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
   size ALLOC-NEED MEM-ALLOC-64K-SPAN {: buf:^ cap:n :}
   s BUF@ s CAP@ {: old:^ oldcap:n :}
   cap s CAP!
   buf s BUF!
   old 0= 0= IF old oldcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES THEN ;

: TOO-LARGE ( ^u8 n n -- ) {: path:^ pathu:n size:n :}
   s" lint-slab: file exceeds maximum: " type path pathu type cr
   s" lint-slab: needed " type size .
   s" lint-slab: have " type MAX-BYTES .
   E-FS-CAPACITY throw ;

: ENSURE-CAPACITY ( ^u8 n n ^n -- ) {: path:^ pathu:n size:n s:^ :}
   size MAX-BYTES > IF path pathu size TOO-LARGE THEN
   s BUF@ 0= size s CAP@ > or IF size s ALLOC THEN ;

public

\ Cells a caller must reserve for one slab: `create MY-SLAB LINT-SLAB:CELLS cells allot`.
3 constant CELLS

: LOAD ( ^u8 n ^n -- ) {: path:^ pathu:n s:^ :}
   path pathu FS:FILE-SIZE {: size:n :}
   path pathu size s ENSURE-CAPACITY
   path pathu s BUF@ s CAP@ FS:READ-ALL s LEN! ;

: TEXT ( ^n -- ^u8 n ) {: s:^ :}
   s BUF@ s LEN@ ;

;package

\ The one shared slab, for the many linters that hold a single source live:
\ TOKENIZE callers consume each file before loading the next one, so a larger
\ later file may replace the backing region while smaller files reuse it.
package LINT-SOURCE

create SLAB LINT-SLAB:CELLS cells allot

public

: LOAD ( ^u8 n -- )
   SLAB LINT-SLAB:LOAD ;

: TEXT ( -- ^u8 n )
   SLAB LINT-SLAB:TEXT ;

;package

\ ---- string ops ------------------------------------------------------------
: LINT-STR= ( ^u8 n ^u8 n -- bool ) {: a:^ u b:^ v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;

: LINT-PREFIX? ( ^u8 n ^u8 n -- bool ) {: a:^ u b:^ v :}
   u v < IF LINT-FALSE exit THEN
   a v b v LINT-STR= ;

: LINT-FIND-SUB ( ^u8 n ^u8 n -- option<n> ) {: a:^ u:n sa:^ su:n :}
   su 0= IF 0 OPTION:SOME exit THEN
   0 begin dup u su - <= while
      dup a +  su  sa su LINT-STR= IF OPTION:SOME exit THEN  1+
   repeat  drop  OPTION:NONE ;

\ ---- more string ops for the linters ----
: LINT-BMOVE ( ^u8 ^u8 n -- )
   \ A negative length copies nothing.
   0 max BYTE-COPY ;

: LINT-FOLD ( n -- n )
   dup 64 > over 91 < and IF 32 + THEN ;

: LINT-STR=CI ( ^u8 n ^u8 n -- bool ) {: a:^ u b:^ v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ LINT-FOLD over b + c@ LINT-FOLD <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;

: FOLD-TO ( ^u8 n ^u8 -- ) {: a:^ u dst:^ :}
   0 begin dup u < while  dup a + c@ LINT-FOLD  over dst + c!  1+  repeat  drop ;

\ ---- bounded string helpers for source tools ------------------------------
13 constant LINT-CR
34 constant DQUOTE
46 constant DOT
47 constant SLASH
: LINT-WS? ( n -- bool )  dup 32 = over 9 = or over 10 = or swap LINT-CR = or ;
\ Whether byte i of the span a u exists and is not whitespace (LINT-INK-AT?)
\ or is whitespace (LINT-GAP-AT?). Each reads the byte only when i is inside
\ the span: `and` evaluates both operands, and the byte past a span that fills
\ its mapping is unmapped.
: LINT-INK-AT? ( ^u8 n n -- bool )
   {: a:^ u:n i:n :}
   i u >= IF LINT-FALSE exit THEN
   a i + c@ LINT-WS? 0= ;
: LINT-GAP-AT? ( ^u8 n n -- bool )
   {: a:^ u:n i:n :}
   i u >= IF LINT-FALSE exit THEN
   a i + c@ LINT-WS? ;
: LINT-LTRIM ( ^u8 n -- ^u8 n ) {: a:^ u :}
   0 begin dup u < while
      dup a + c@ LINT-WS? 0= IF dup a +  u rot -  exit THEN  1+
   repeat  drop  a 0 ;
: LINT-RTRIM ( ^u8 n -- ^u8 n ) {: a:^ u :}
   u RLEN !
   begin RLEN @ 0 > while
      a RLEN @ 1- + c@ LINT-WS? IF RLEN @ 1- RLEN ! ELSE a RLEN @ exit THEN
   repeat  a 0 ;
: LINT-TRIM ( ^u8 n -- ^u8 n )  LINT-LTRIM LINT-RTRIM ;
: LINT-STARTS-WITH? ( ^u8 n ^u8 n -- bool )  LINT-PREFIX? ;
: LINT-SUFFIX? ( ^u8 n ^u8 n -- bool ) {: a:^ u b:^ v :}
   u v < IF LINT-FALSE exit THEN
   a u v - + v b v LINT-STR= ;
: LINT-ENDS-WITH? ( ^u8 n ^u8 n -- bool )  LINT-SUFFIX? ;
: LINT-INDEX-OF ( ^u8 n n -- option<n> ) {: a:^ u:n c:n :}   \ SOME first byte offset, NONE if absent
   0 begin dup u < while  dup a + c@ c = IF OPTION:SOME exit THEN  1+  repeat  drop OPTION:NONE ;
: LINT-COUNT-CHAR ( ^u8 n n -- n ) {: a:^ u c :}
   0  0 begin dup u < while
      dup a + c@ c = IF swap 1+ swap THEN  1+
   repeat  drop ;

\ Shared split results: (offset, length) rows into the caller's source string.
\ The scan cursors are the package's own, so a split during a read never moves
\ READ-FILE's.
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
: SPLIT+ ( ^u8 n -- ) {: a:^ u:n :}
   SN# @ SMAX >= IF s" lint: split result overflow" EXIT-STATUS:FULL die THEN
   a SN# @ SOFF !  u SLEN SN# @ cells + !  SN# @ 1+ SN# ! ;
: S@ ( n -- ^u8 n )  dup SOFF @  swap cells SLEN + @ ;

private

: ADD-LINE ( ^u8 n -- ) {: a:^ u:n :}              \ one line, trailing CR dropped
   u 0 > IF a u 1- + c@ LINT-CR = IF a u 1- SPLIT+ exit THEN THEN
   a u SPLIT+ ;

public

: SPLIT-LINES ( ^u8 n -- ) {: a:^ u:n :}
   SPLIT-CLEAR  0 MARK !  0 CUR !
   begin CUR @ u < while
      a CUR @ + c@ 10 = IF
         a MARK @ +  CUR @ MARK @ -  ADD-LINE
         CUR @ 1+ MARK !
      THEN
      CUR @ 1+ CUR !
   repeat
   MARK @ u < IF a MARK @ +  u MARK @ -  ADD-LINE THEN ;
: SPLIT-WHITESPACE ( ^u8 n -- ) {: a:^ u:n :}
   SPLIT-CLEAR  0 CUR !
   begin CUR @ u < while
      begin a u CUR @ LINT-GAP-AT? while CUR @ 1+ CUR ! repeat
      CUR @ u < IF
         CUR @ MARK !
         begin a u CUR @ LINT-INK-AT? while CUR @ 1+ CUR ! repeat
         a MARK @ +  CUR @ MARK @ -  SPLIT+
      THEN
   repeat ;
: JOIN-SPLIT ( ^u8 n ^u8 n ^len -- ) {: sep:^ su:n dst:^ cap:n lp:^ :}
   lp STR:BUF-RESET
   0 begin dup SN# @ < while
      dup 0 > IF sep su STR:LENGTH dst cap STR:LENGTH lp STR:BUF-APPEND THEN
      dup S@ STR:LENGTH dst cap STR:LENGTH lp STR:BUF-APPEND  1+
   repeat  drop ;

;package
: HAS-EXT? ( ^u8 n ^u8 n -- bool )  LINT-SUFFIX? ;
\ ---- throw-code name registry for CLI failure attribution ------------------
\ The table names the codes LINT-MAIN (tools/lint/lib.f) can name in its
\ attribution line: the intern and token tables' EXIT-STATUS:FULL.
8 constant LINT-CODE-MAX
32 constant LINT-CODE-NAME-CAP
create LINT-CODE-VALS LINT-CODE-MAX cells allot
create LINT-CODE-NAME-BUF LINT-CODE-MAX LINT-CODE-NAME-CAP * allot
create LINT-CODE-NAME-US LINT-CODE-MAX cells allot
variable LINT-CODE#

: LINT-CODE-NAME$ ( n -- ^u8 n ) {: i:n :}
   LINT-CODE-NAME-BUF i LINT-CODE-NAME-CAP * +
   LINT-CODE-NAME-US i cells + @ ;

: LINT-CODE-VAL@ ( n -- n )
   cells LINT-CODE-VALS + @ ;

: LINT-CODE-NAME+ ( ^u8 n n -- ) {: a:^ u:n code:n :}
   LINT-CODE# @ LINT-CODE-MAX >= IF s" lint: code-name table full" EXIT-STATUS:INTERNAL die THEN
   u LINT-CODE-NAME-CAP > IF s" lint: code name too long" EXIT-STATUS:INTERNAL die THEN
   code LINT-CODE-VALS LINT-CODE# @ cells + !
   a LINT-CODE-NAME-BUF LINT-CODE# @ LINT-CODE-NAME-CAP * + u LINT-BMOVE
   u LINT-CODE-NAME-US LINT-CODE# @ cells + !
   LINT-CODE# @ 1+ LINT-CODE# ! ;

s" EXIT-STATUS:FULL" EXIT-STATUS:FULL LINT-CODE-NAME+

: LINT-CODE-FIND ( n -- n ) {: code:n :}   \ registry index or -1
   0 begin dup LINT-CODE# @ < while
      dup LINT-CODE-VAL@ code = IF exit THEN
      1+
   repeat drop -1 ;
