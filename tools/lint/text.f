\ text.f — checked text/file helpers for native lint tools.

require src/lib/string.f
require src/lib/string-roles.f               \ package STR: the typed string surface
require src/lib/memory.f
require src/lib/fs.f
require src/lib/adt/option.f                      \ option<n> finders

\ ---- whole-file read -------------------------------------------------------
\ Lint tools load this first, so shared helpers fail closed under
\ LINT-CHECK-HOOK. CHECK! is a primitive axiom so the hook definition itself
\ compiles checked.
s" CHECK!" s" ptr u8 n -- n" TRUST

: LINT-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> IF 70 throw THEN ;
LOWER-CERT-HOOK:INSTALL
' LINT-CHECK-HOOK set-check

create READ-PROBE 1 allot
variable RFD  variable RGOT  variable RLEN
TYPED-VARIABLE LINT-OUT-A ptr u8
variable LINT-OUT-CAP
variable LINT-OUT-U
variable LINT-OUT-ON

: LINT-OUT-A@ ( -- ptr u8 )
   LINT-OUT-A @ ;

: LINT-OUT-A! ( ptr u8 -- )
   LINT-OUT-A ! ;

: LINT-TRUE ( -- bool )
   0 0= ;

: LINT-FALSE ( -- bool )
   0 1 = ;

: LINT-NOT ( bool -- bool )
   IF LINT-FALSE ELSE LINT-TRUE THEN ;

: LINT-OUT-BUFFER! ( ptr u8 n -- ) {: a:ptr cap:n :}
   a LINT-OUT-A!
   cap LINT-OUT-CAP !
   0 LINT-OUT-U !
   LINT-TRUE LINT-OUT-ON ! ;

: LINT-OUT-BUFFER-OFF ( -- )
   LINT-FALSE LINT-OUT-ON ! ;

: LINT-OUT$ ( -- ptr u8 n )
   LINT-OUT-A@ LINT-OUT-U @ ;

: LINT-FD-WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0= IF exit THEN
   fd a u write u <> IF s" lint: write failed" 74 die THEN ;

: LINT-OUT-WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   LINT-OUT-ON @ IF
      a u STR:LENGTH LINT-OUT-A@ LINT-OUT-CAP @ STR:LENGTH LINT-OUT-U STR:BUF-APPEND
      exit
   THEN
   fd a u LINT-FD-WRITE ;

\ capacity/IO exits must name the offending file — a bare "file exceeds buffer"
\ hides which source outgrew which lint buffer.
: LINT-READ-DIE ( ptr u8 n ptr u8 n -- )
   {: msg:ptr msgu:n path:ptr pathu:n :}
   2 msg msgu LINT-FD-WRITE
   path pathu 1 die ;

\ open takes a NUL-terminated path, so FS:>PATHZ copies the Habu path first.
: READ-FILE ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: a:ptr u buf:ptr cap :}
   a u FS:>PATHZ 0 0 open  RFD !
   RFD @ 0 < IF s" lint: cannot open file: " a u LINT-READ-DIE THEN
   0 RLEN !
   begin  RFD @  buf RLEN @ +  cap RLEN @ -  read  dup RGOT !  0 >
   while  RLEN @ RGOT @ + RLEN !  repeat
   RGOT @ 0 < IF RFD @ close s" lint: cannot read file: " a u LINT-READ-DIE THEN
   RLEN @ cap = IF
      RFD @ READ-PROBE 1 read RGOT !
      RGOT @ 0 < IF RFD @ close s" lint: cannot read file: " a u LINT-READ-DIE THEN
      RGOT @ 0 > IF RFD @ close s" lint: file exceeds buffer: " a u LINT-READ-DIE THEN
   THEN
   RFD @ close
   buf RLEN @ ;

\ Runtime-sized whole-file storage for an input that grows: a fixed buffer dies
\ mid-run the day the file crosses its constant. A slab is three cells the
\ caller owns (backing pointer, capacity, length last read), so a tool holding
\ two files live declares two slabs.
package LINT-SLAB

$1000000 constant MAX-BYTES

: BUF-FIELD ( ptr n -- ptr ptr u8 )
   0 ptr-field ;

: BUF@ ( ptr n -- ptr u8 )
   BUF-FIELD @ ;

: BUF! ( ptr u8 ptr n -- )
   BUF-FIELD ! ;

: CAP@ ( ptr n -- n ) {: s:ptr :}
   s 1 cells + @ ;

: CAP! ( n ptr n -- ) {: v:n s:ptr :}
   v s 1 cells + ! ;

: LEN@ ( ptr n -- n ) {: s:ptr :}
   s 2 cells + @ ;

: LEN! ( n ptr n -- ) {: v:n s:ptr :}
   v s 2 cells + ! ;

: ALLOC-NEED ( n -- n )
   dup 0 <= IF drop 1 THEN ;

: ALLOC ( n ptr n -- ) {: size:n s:ptr :}
   size ALLOC-NEED MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   s BUF@ s CAP@ {: old:ptr oldcap:n :}
   cap s CAP!
   buf s BUF!
   old 0= 0= IF old oldcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES THEN ;

: TOO-LARGE ( ptr u8 n n -- ) {: path:ptr pathu:n size:n :}
   s" lint-slab: file exceeds maximum: " type path pathu type cr
   s" lint-slab: needed " type size .
   s" lint-slab: have " type MAX-BYTES .
   E-FS-CAPACITY throw ;

: ENSURE-CAPACITY ( ptr u8 n n ptr n -- ) {: path:ptr pathu:n size:n s:ptr :}
   size MAX-BYTES > IF path pathu size TOO-LARGE THEN
   s BUF@ 0= size s CAP@ > or IF size s ALLOC THEN ;

public

\ Cells a caller must reserve for one slab: `create MY-SLAB LINT-SLAB:CELLS cells allot`.
3 constant CELLS

: LOAD ( ptr u8 n ptr n -- ) {: path:ptr pathu:n s:ptr :}
   path pathu FS:FILE-SIZE {: size:n :}
   path pathu size s ENSURE-CAPACITY
   path pathu s BUF@ s CAP@ FS:READ-ALL s LEN! ;

: TEXT ( ptr n -- ptr u8 n ) {: s:ptr :}
   s BUF@ s LEN@ ;

;package

\ The one shared slab, for the many linters that hold a single source live:
\ TOKENIZE callers consume each file before loading the next one, so a larger
\ later file may replace the backing region while smaller files reuse it.
package LINT-SOURCE

create SLAB LINT-SLAB:CELLS cells allot

public

: LOAD ( ptr u8 n -- )
   SLAB LINT-SLAB:LOAD ;

: TEXT ( -- ptr u8 n )
   SLAB LINT-SLAB:TEXT ;

;package

\ ---- string ops ------------------------------------------------------------
: LINT-STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;

: LINT-PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < IF LINT-FALSE exit THEN
   a v b v LINT-STR= ;

: LINT-FIND-SUB ( ptr u8 n ptr u8 n -- option<n> ) {: a:ptr u:n sa:ptr su:n :}
   su 0= IF 0 OPTION:SOME exit THEN
   0 begin dup u su - <= while
      dup a +  su  sa su LINT-STR= IF OPTION:SOME exit THEN  1+
   repeat  drop  OPTION:NONE ;

: LINT-CONTAINS? ( ptr u8 n ptr u8 n -- bool )
   LINT-FIND-SUB MATCH option
     none OF LINT-FALSE ENDOF
     some OF drop LINT-TRUE ENDOF
   ;MATCH ;

\ ---- more string ops for the linters ----
: LINT-BMOVE ( ptr u8 ptr u8 n -- )
   \ A negative length copies nothing.
   0 max BYTE-COPY ;

: LINT-FOLD ( n -- n )
   dup 64 > over 91 < and IF 32 + THEN ;

: LINT-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ LINT-FOLD over b + c@ LINT-FOLD <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;

: FOLD-TO ( ptr u8 n ptr u8 -- ) {: a:ptr u dst:ptr :}
   0 begin dup u < while  dup a + c@ LINT-FOLD  over dst + c!  1+  repeat  drop ;

\ ---- bounded string helpers for source tools ------------------------------
13 constant LINT-CR
34 constant DQUOTE
46 constant DOT
47 constant SLASH
96 constant BEFORE-A
123 constant AFTER-Z

: ASCII-UP ( n -- n )  dup BEFORE-A > over AFTER-Z < and IF 32 - THEN ;
: COPY-LOWER ( ptr u8 n ptr u8 -- ) {: a:ptr u dst:ptr :}  a u dst FOLD-TO ;
: COPY-UPPER ( ptr u8 n ptr u8 -- ) {: a:ptr u dst:ptr :}
   0 begin dup u < while  dup a + c@ ASCII-UP  over dst + c!  1+  repeat  drop ;
: LINT-WS? ( n -- bool )  dup 32 = over 9 = or over 10 = or swap LINT-CR = or ;
\ Whether byte i of the span a u exists and is not whitespace (LINT-INK-AT?)
\ or is whitespace (LINT-GAP-AT?). Each reads the byte only when i is inside
\ the span: `and` evaluates both operands, and the byte past a span that fills
\ its mapping is unmapped (a source ending on a 64 KiB boundary faulted
\ public-signatures).
: LINT-INK-AT? ( ptr u8 n n -- bool )
   {: a:ptr u:n i:n :}
   i u >= IF LINT-FALSE exit THEN
   a i + c@ LINT-WS? 0= ;
: LINT-GAP-AT? ( ptr u8 n n -- bool )
   {: a:ptr u:n i:n :}
   i u >= IF LINT-FALSE exit THEN
   a i + c@ LINT-WS? ;
: LINT-LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ LINT-WS? 0= IF dup a +  u rot -  exit THEN  1+
   repeat  drop  a 0 ;
: LINT-RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u RLEN !
   begin RLEN @ 0 > while
      a RLEN @ 1- + c@ LINT-WS? IF RLEN @ 1- RLEN ! ELSE a RLEN @ exit THEN
   repeat  a 0 ;
: LINT-TRIM ( ptr u8 n -- ptr u8 n )  LINT-LTRIM LINT-RTRIM ;
: LINT-STARTS-WITH? ( ptr u8 n ptr u8 n -- bool )  LINT-PREFIX? ;
: LINT-SUFFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < IF LINT-FALSE exit THEN
   a u v - + v b v LINT-STR= ;
: LINT-ENDS-WITH? ( ptr u8 n ptr u8 n -- bool )  LINT-SUFFIX? ;
: LINT-INDEX-OF ( ptr u8 n n -- option<n> ) {: a:ptr u:n c:n :}   \ SOME first byte offset, NONE if absent
   0 begin dup u < while  dup a + c@ c = IF OPTION:SOME exit THEN  1+  repeat  drop OPTION:NONE ;
: LINT-COUNT-CHAR ( ptr u8 n n -- n ) {: a:ptr u c :}
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
SMAX TYPED-BUFFER SOFF ptr u8
create SLEN SMAX cells allot
variable CUR    \ scan cursor
variable MARK   \ start of the field being scanned

public

variable SN#
: SPLIT-CLEAR  ( -- )  0 SN# ! ;
: SPLIT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SN# @ SMAX >= IF s" lint: split result overflow" 1 die THEN
   a SN# @ SOFF !  u SLEN SN# @ cells + !  SN# @ 1+ SN# ! ;
: S@ ( n -- ptr u8 n )  dup SOFF @  swap cells SLEN + @ ;

private

: ADD-LINE ( ptr u8 n -- ) {: a:ptr u:n :}              \ one line, trailing CR dropped
   u 0 > IF a u 1- + c@ LINT-CR = IF a u 1- SPLIT+ exit THEN THEN
   a u SPLIT+ ;

public

: SPLIT-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   SPLIT-CLEAR  0 MARK !  0 CUR !
   begin CUR @ u < while
      a CUR @ + c@ 10 = IF
         a MARK @ +  CUR @ MARK @ -  ADD-LINE
         CUR @ 1+ MARK !
      THEN
      CUR @ 1+ CUR !
   repeat
   MARK @ u < IF a MARK @ +  u MARK @ -  ADD-LINE THEN ;
: SPLIT-WHITESPACE ( ptr u8 n -- ) {: a:ptr u:n :}
   SPLIT-CLEAR  0 CUR !
   begin CUR @ u < while
      begin a u CUR @ LINT-GAP-AT? while CUR @ 1+ CUR ! repeat
      CUR @ u < IF
         CUR @ MARK !
         begin a u CUR @ LINT-INK-AT? while CUR @ 1+ CUR ! repeat
         a MARK @ +  CUR @ MARK @ -  SPLIT+
      THEN
   repeat ;
: JOIN-SPLIT ( ptr u8 n ptr u8 n ptr len -- ) {: sep:ptr su:n dst:ptr cap:n lp:ptr :}
   lp STR:BUF-RESET
   0 begin dup SN# @ < while
      dup 0 > IF sep su STR:LENGTH dst cap STR:LENGTH lp STR:BUF-APPEND THEN
      dup S@ STR:LENGTH dst cap STR:LENGTH lp STR:BUF-APPEND  1+
   repeat  drop ;

;package
: HAS-EXT? ( ptr u8 n ptr u8 n -- bool )  LINT-SUFFIX? ;
\ ---- throw-code name registry for CLI failure attribution ------------------
\ Lint modules register their named throw codes at load (intern/token caps) so
\ LINT-MAIN (tools/lint/lib.f) can name an uncaught code in its attribution
\ line regardless of which modules a CLI loads.
8 constant LINT-CODE-MAX
32 constant LINT-CODE-NAME-CAP
create LINT-CODE-VALS LINT-CODE-MAX cells allot
create LINT-CODE-NAME-BUF LINT-CODE-MAX LINT-CODE-NAME-CAP * allot
create LINT-CODE-NAME-US LINT-CODE-MAX cells allot
variable LINT-CODE#

: LINT-CODE-NAME$ ( n -- ptr u8 n ) {: i:n :}
   LINT-CODE-NAME-BUF i LINT-CODE-NAME-CAP * +
   LINT-CODE-NAME-US i cells + @ ;

: LINT-CODE-VAL@ ( n -- n )
   cells LINT-CODE-VALS + @ ;

: LINT-CODE-NAME+ ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   LINT-CODE# @ LINT-CODE-MAX >= IF s" lint: code-name table full" 1 die THEN
   u LINT-CODE-NAME-CAP > IF s" lint: code name too long" 1 die THEN
   code LINT-CODE-VALS LINT-CODE# @ cells + !
   a LINT-CODE-NAME-BUF LINT-CODE# @ LINT-CODE-NAME-CAP * + u LINT-BMOVE
   u LINT-CODE-NAME-US LINT-CODE# @ cells + !
   LINT-CODE# @ 1+ LINT-CODE# ! ;

: LINT-CODE-FIND ( n -- n ) {: code:n :}   \ registry index or -1
   0 begin dup LINT-CODE# @ < while
      dup LINT-CODE-VAL@ code = IF exit THEN
      1+
   repeat drop -1 ;
