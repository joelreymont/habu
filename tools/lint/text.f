\ text.f — checked text/file helpers for native lint tools.

\ ---- whole-file read -------------------------------------------------------
\ open/read/close are engine prims: open ( path flags mode -- fd ), read ( fd
\ buf n -- n ), close ( fd -- ). O_RDONLY = 0. Path must be NUL-terminated.
\ Audited hook-install boundary: lint tools load this first so shared helpers
\ fail closed under LINT-CHECK-HOOK.
0 set-check

: LINT-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> IF 70 throw THEN ;
' LINT-CHECK-HOOK set-check

create PATHBUF 1024 allot
variable RFD  variable RGOT  variable RLEN

: LINT-TRUE ( -- bool )
   0 0= ;

: LINT-FALSE ( -- bool )
   0 1 = ;

: LINT-NOT ( bool -- bool )
   IF LINT-FALSE ELSE LINT-TRUE THEN ;

: PATHZ ( ptr u8 n -- ) {: a:ptr u :}
   u 1+ 1024 > IF s" lint: path too long" 1 die THEN
   0 begin dup u < while
      dup a + c@ over PATHBUF + c!
      1+
   repeat drop
   0 u PATHBUF + c! ;

: READ-FILE ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u buf:ptr cap :}
   a u PATHZ
   PATHBUF 0 0 open  RFD !
   RFD @ 0 < IF s" lint: cannot open file" 1 die THEN
   0 RLEN !
   begin  RFD @  buf RLEN @ +  cap RLEN @ -  read  dup RGOT !  0 >
   while  RLEN @ RGOT @ + RLEN !  repeat
   RFD @ close
   buf RLEN @ ;

\ ---- string ops ------------------------------------------------------------
: STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;

: PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < IF LINT-FALSE exit THEN
   a v b v STR= ;

\ FIND-SUB ( a u sa su -- idx | -1 ) : index of first occurrence of sa/su in a/u
: FIND-SUB ( ptr u8 n ptr u8 n -- n ) {: a:ptr u sa:ptr su :}
   su 0= IF 0 exit THEN
   0 begin dup u su - <= while
      dup a +  su  sa su STR= IF exit THEN  1+
   repeat  drop  -1 ;

: CONTAINS? ( ptr u8 n ptr u8 n -- bool )
   FIND-SUB 0< LINT-NOT ;

\ ---- more string ops for the linters ----
: BMOVE ( ptr u8 ptr u8 n -- ) {: a:ptr dst:ptr u :}
   0 begin dup u < while  dup a + c@  over dst + c!  1+  repeat  drop ;

: FOLD ( n -- n )
   dup 64 > over 91 < and IF 32 + THEN ;

: STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> IF LINT-FALSE exit THEN
   0 begin dup u < while
      dup a + c@ FOLD over b + c@ FOLD <> IF drop LINT-FALSE exit THEN
      1+
   repeat drop LINT-TRUE ;

: FOLD-TO ( ptr u8 n ptr u8 -- ) {: a:ptr u dst:ptr :}
   0 begin dup u < while  dup a + c@ FOLD  over dst + c!  1+  repeat  drop ;

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
: WS? ( n -- bool )  dup 32 = over 9 = or over 10 = or swap LINT-CR = or ;
: LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ WS? 0= IF dup a +  u rot -  exit THEN  1+
   repeat  drop  a 0 ;
: RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u RLEN !
   begin RLEN @ 0 > while
      a RLEN @ 1- + c@ WS? IF RLEN @ 1- RLEN ! ELSE a RLEN @ exit THEN
   repeat  a 0 ;
: TRIM ( ptr u8 n -- ptr u8 n )  LTRIM RTRIM ;
: STARTS-WITH? ( ptr u8 n ptr u8 n -- bool )  PREFIX? ;
: SUFFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < IF LINT-FALSE exit THEN
   a u v - + v b v STR= ;
: ENDS-WITH? ( ptr u8 n ptr u8 n -- bool )  SUFFIX? ;
: INDEX-OF ( ptr u8 n n -- n ) {: a:ptr u c :}
   0 begin dup u < while  dup a + c@ c = IF exit THEN  1+  repeat  drop -1 ;
: COUNT-CHAR ( ptr u8 n n -- n ) {: a:ptr u c :}
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
      begin RGOT @ u <  a RGOT @ + c@ WS? and while RGOT @ 1+ RGOT ! repeat
      RGOT @ u < IF
         RGOT @ RLEN !
         begin RGOT @ u <  a RGOT @ + c@ WS? 0= and while RGOT @ 1+ RGOT ! repeat
         a RLEN @ +  RGOT @ RLEN @ -  SPLIT+
      THEN
   repeat ;
: BUF-APPEND ( ptr u8 n ptr u8 n ptr n -- ) {: a:ptr u dst:ptr cap lp:ptr :}
   lp @ u + cap > IF s" lint: string buffer overflow" 1 die THEN
   a dst lp @ + u BMOVE  lp @ u + lp ! ;
: BUF-APPEND-C ( n ptr u8 n ptr n -- ) {: c dst:ptr cap lp:ptr :}
   lp @ 1+ cap > IF s" lint: string buffer overflow" 1 die THEN
   c dst lp @ + c!  lp @ 1+ lp ! ;
: JOIN-SPLIT ( ptr u8 n ptr u8 n ptr n -- ) {: sep:ptr su dst:ptr cap lp:ptr :}
   0 lp !
   0 begin dup SN# @ < while
      dup 0 > IF sep su dst cap lp BUF-APPEND THEN
      dup S@ dst cap lp BUF-APPEND  1+
   repeat  drop ;
: HAS-EXT? ( ptr u8 n ptr u8 n -- bool )  SUFFIX? ;
: PATHISH? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u SLASH INDEX-OF 0 >= IF LINT-TRUE exit THEN
   a u s" .md" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .sh" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .f" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .fs" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .tsv" HAS-EXT? ;
