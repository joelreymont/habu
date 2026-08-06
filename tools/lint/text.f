\ text.f — checked text/file helpers for native lint tools.

require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/adt/option.f                      \ option<n> finders (switchover: lint finder trio)

\ ---- whole-file read -------------------------------------------------------
\ open/read/close are engine prims: open ( path flags mode -- fd ), read ( fd
\ buf n -- n ), close ( fd -- ). O_RDONLY = 0. Path must be NUL-terminated.
\ Lint tools load this first so shared helpers fail closed under
\ LINT-CHECK-HOOK. CHECK! (engine checker entrypoint) is modeled as a
\ primitive axiom so the hook definition itself compiles checked
\ (axiom owner: habu-primitive-effect-axiom-1119f176).
s" CHECK!" s" ptr u8 n -- n" TRUST

: LINT-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> IF 70 throw THEN ;
LOWER-CERT-HOOK:INSTALL
' LINT-CHECK-HOOK set-check

create PATHBUF 1024 allot
create READ-PROBE 1 allot
variable RFD  variable RGOT  variable RLEN
variable LINT-OUT-A
variable LINT-OUT-CAP
variable LINT-OUT-U
variable LINT-OUT-ON

: LINT-OUT-A-FIELD ( -- ptr ptr u8 )
   LINT-OUT-A 0 ptr-field ;

: LINT-OUT-A@ ( -- ptr u8 )
   LINT-OUT-A-FIELD @ ;

: LINT-OUT-A! ( ptr u8 -- )
   LINT-OUT-A-FIELD ! ;

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

variable RPATH-U

: LINT-PATHZ ( ptr u8 n -- ) {: a:ptr u :}
   u 1+ 1024 > IF s" lint: path too long" 1 die THEN
   0 begin dup u < while
      dup a + c@ over PATHBUF + c!
      1+
   repeat drop
   0 u PATHBUF + c!
   u RPATH-U ! ;

\ capacity/IO exits must name the offending file (LESSONS: label every
\ capacity exit) — a bare "file exceeds buffer" hides which source outgrew
\ which lint buffer.
: LINT-READ-DIE ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   2 msg msgu LINT-FD-WRITE
   PATHBUF RPATH-U @ 1 die ;

: READ-FILE ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u buf:ptr cap :}
   a u LINT-PATHZ
   PATHBUF 0 0 open  RFD !
   RFD @ 0 < IF s" lint: cannot open file: " LINT-READ-DIE THEN
   0 RLEN !
   begin  RFD @  buf RLEN @ +  cap RLEN @ -  read  dup RGOT !  0 >
   while  RLEN @ RGOT @ + RLEN !  repeat
   RGOT @ 0 < IF RFD @ close s" lint: cannot read file: " LINT-READ-DIE THEN
   RLEN @ cap = IF
      RFD @ READ-PROBE 1 read RGOT !
      RGOT @ 0 < IF RFD @ close s" lint: cannot read file: " LINT-READ-DIE THEN
      RGOT @ 0 > IF RFD @ close s" lint: file exceeds buffer: " LINT-READ-DIE THEN
   THEN
   RFD @ close
   buf RLEN @ ;

\ Runtime-sized whole-file storage for a tool whose authoritative input grows
\ over time. A fixed `create ... allot` buffer is the wrong shape for such an
\ input: the day the file crosses the constant the tool dies mid-run and every
\ answer it was going to give looks like the file's fault. A slab is three cells
\ the CALLER owns - the backing pointer, the capacity behind it, and the length
\ last read - so a tool that has to hold two files live at once declares two
\ slabs instead of contending for one shared buffer.
package LINT-SLAB

$1000000 constant MAX-BYTES

: BUF-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: BUF@ ( ptr a -- ptr u8 )
   BUF-FIELD @ ;

: BUF! ( ptr u8 ptr a -- )
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
   cap s CAP!
   buf s BUF! ;

: TOO-LARGE ( ptr u8 n n -- ) {: path:ptr pathu:n size:n :}
   s" lint-slab: file exceeds maximum: " type path pathu type cr
   s" lint-slab: bytes=" type size .
   E-FS-CAPACITY throw ;

: ENSURE-CAPACITY ( ptr u8 n n ptr n -- ) {: path:ptr pathu:n size:n s:ptr :}
   size MAX-BYTES > IF path pathu size TOO-LARGE THEN
   s BUF@ 0= size s CAP@ > or IF size s ALLOC THEN ;

public

\ Cells a caller must reserve for one slab: `create MY-SLAB LINT-SLAB:CELLS cells allot`.
3 constant CELLS

: LOAD ( ptr u8 n ptr n -- ) {: path:ptr pathu:n s:ptr :}
   path pathu FILE-SIZE {: size:n :}
   path pathu size s ENSURE-CAPACITY
   path pathu s BUF@ s CAP@ READ-ALL s LEN! ;

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

\ LINT-FIND-SUB: SOME offset of the first occurrence of sa/su in a/u, NONE if absent.
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
: LINT-BMOVE ( ptr u8 ptr u8 n -- ) {: a:ptr dst:ptr u :}
   0 begin dup u < while  dup a + c@  over dst + c!  1+  repeat  drop ;

: LINT-FOLD ( n -- n )
   dup 64 > over 91 < and IF 32 + THEN ;

: LINT-SC-STRING-LEAD? ( n -- bool )
   LINT-FOLD dup 115 = swap 99 = or ;

: LINT-STRING-LEAD? ( n -- bool )
   dup LINT-SC-STRING-LEAD? swap $2E = or ;

: LINT-NORMAL-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> IF LINT-FALSE exit THEN
   a 1 BYTE@ $22 <> IF LINT-FALSE exit THEN
   a 0 BYTE@ LINT-STRING-LEAD? ;

: LINT-ESC-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 3 <> IF LINT-FALSE exit THEN
   a 1 BYTE@ $5C <> IF LINT-FALSE exit THEN
   a 2 BYTE@ $22 <> IF LINT-FALSE exit THEN
   a 0 BYTE@ LINT-STRING-LEAD? ;

: LINT-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ LINT-FOLD over b + c@ LINT-FOLD <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;

\ A total order on case-folded names, so a tool holding a fixed name set can keep
\ it sorted and reach it by binary search instead of scanning it. Kept apart from
\ the equality helpers above because ordering is the whole of this module's job:
\ CMP-CI answers 0 on exactly the pairs LINT-STR=CI calls equal, which is what
\ makes a search over the sorted set agree with a scan that used LINT-STR=CI.
package LINT-ORDER

public

: CMP-CI ( ptr u8 n ptr u8 n -- n )   \ <0 a before b, 0 equal folded, >0 a after
   {: a:ptr u:n b:ptr v:n :}
   u v < IF u ELSE v THEN {: m:n :}
   0 begin dup m < while
      dup a + c@ LINT-FOLD
      over b + c@ LINT-FOLD
      2dup < IF 2drop drop -1 exit THEN
      > IF drop 1 exit THEN
      1+
   repeat drop
   u v < IF -1 exit THEN
   u v > IF 1 exit THEN
   0 ;

;package

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

\ Shared split result arrays. Entries point into the original source string.
$400 constant SMAX
create SOFF SMAX cells allot   create SLEN SMAX cells allot   variable SN#
: SPLIT-CLEAR  ( -- )  0 SN# ! ;
: SPLIT+ ( ptr u8 n -- ) {: a:ptr u :}
   SN# @ SMAX >= IF s" lint: split result overflow" 1 die THEN
   a SOFF SN# @ cells + !  u SLEN SN# @ cells + !  SN# @ 1+ SN# ! ;
: S@ ( n -- ptr u8 n )  dup cells SOFF + @  swap cells SLEN + @ ;
: ADD-LINE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 >  a u 1- + c@ LINT-CR = and IF a u 1- SPLIT+ ELSE a u SPLIT+ THEN ;
: SPLIT-LINES ( ptr u8 n -- ) {: a:ptr u :}
   SPLIT-CLEAR  0 RLEN !  0 RGOT !
   begin RGOT @ u < while
      a RGOT @ + c@ 10 = IF
         a RLEN @ +  RGOT @ RLEN @ -  ADD-LINE
         RGOT @ 1+ RLEN !
      THEN
      RGOT @ 1+ RGOT !
   repeat
   RLEN @ u < IF a RLEN @ +  u RLEN @ -  ADD-LINE THEN ;
: SPLIT-WHITESPACE ( ptr u8 n -- ) {: a:ptr u :}
   SPLIT-CLEAR  0 RGOT !
   begin RGOT @ u < while
      begin RGOT @ u <  a RGOT @ + c@ LINT-WS? and while RGOT @ 1+ RGOT ! repeat
      RGOT @ u < IF
         RGOT @ RLEN !
         begin RGOT @ u <  a RGOT @ + c@ LINT-WS? 0= and while RGOT @ 1+ RGOT ! repeat
         a RLEN @ +  RGOT @ RLEN @ -  SPLIT+
      THEN
   repeat ;
: JOIN-SPLIT ( ptr u8 n ptr u8 n ptr len -- ) {: sep:ptr su dst:ptr cap lp:ptr :}
   lp STR:BUF-RESET
   0 begin dup SN# @ < while
      dup 0 > IF sep su STR:LENGTH dst cap STR:LENGTH lp STR:BUF-APPEND THEN
      dup S@ STR:LENGTH dst cap STR:LENGTH lp STR:BUF-APPEND  1+
   repeat  drop ;
: HAS-EXT? ( ptr u8 n ptr u8 n -- bool )  LINT-SUFFIX? ;
: PATHISH? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u SLASH LINT-INDEX-OF MATCH option
     none OF LINT-FALSE ENDOF
     some OF drop LINT-TRUE ENDOF
   ;MATCH IF LINT-TRUE exit THEN
   a u s" .md" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .sh" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .f" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .fs" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .tsv" HAS-EXT? ;

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
