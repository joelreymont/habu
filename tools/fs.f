\ fs.f — small native filesystem layer for Habu tools.

1024 constant FS-PATH-CAP
4096 constant FS-DIR-CAP
32 constant FS-MAX-DEPTH
256 constant FS-STAT-CAP

$F000 constant S-IFMT
$4000 constant S-IFDIR
$8000 constant S-IFREG

46 constant FS-DOT
47 constant FS-SLASH

-2100 constant E-FS-PATH
-2101 constant E-FS-STAT
-2102 constant E-FS-OPEN
-2103 constant E-FS-DIR
-2104 constant E-FS-DEPTH

create FS-PATHZ-BUF FS-PATH-CAP allot
create FS-STAT-BUF FS-STAT-CAP allot
create FS-WALK-BUF FS-MAX-DEPTH FS-PATH-CAP * allot
create FS-DIR-BUF FS-MAX-DEPTH FS-DIR-CAP * allot
create FS-BASES FS-MAX-DEPTH cells allot
create FS-FDS FS-MAX-DEPTH cells allot
create FS-NS FS-MAX-DEPTH cells allot
create FS-OFFS FS-MAX-DEPTH cells allot
create FS-RECS FS-MAX-DEPTH cells allot
variable FS-DEPTH
variable FS-I
variable FS-WALK-XT
variable FS-CHILD-U
variable FS-ENT
variable FS-NAME-A
variable FS-NAME-U

: COPY-BYTES ( ptr u8 ptr u8 n -- ) {: a:ptr dst:ptr u :}
   0 begin dup u < while
      dup a + c@  over dst + c!
      1+
   repeat drop ;

: U16@ ( ptr u8 -- n ) {: a:ptr :}
   a c@  a 1 + c@ 8 lshift or ;

: FS-SLOT ( n -- ptr u8 )
   FS-WALK-BUF swap FS-PATH-CAP * + ;

: FS-CUR ( -- ptr u8 )
   FS-DEPTH @ FS-SLOT ;

: FS-NEXT ( -- ptr u8 )
   FS-DEPTH @ 1 + FS-SLOT ;

: FS-DIR@ ( -- ptr u8 )
   FS-DIR-BUF FS-DEPTH @ FS-DIR-CAP * + ;

: FS-BASE@ ( -- ptr n )
   FS-BASES FS-DEPTH @ cells + ;

: FS-FD@ ( -- n )
   FS-FDS FS-DEPTH @ cells + @ ;

: FS-FD! ( n -- )
   FS-FDS FS-DEPTH @ cells + ! ;

: FS-N@ ( -- n )
   FS-NS FS-DEPTH @ cells + @ ;

: FS-N! ( n -- )
   FS-NS FS-DEPTH @ cells + ! ;

: FS-OFF@ ( -- n )
   FS-OFFS FS-DEPTH @ cells + @ ;

: FS-OFF! ( n -- )
   FS-OFFS FS-DEPTH @ cells + ! ;

: FS-REC@ ( -- n )
   FS-RECS FS-DEPTH @ cells + @ ;

: FS-REC! ( n -- )
   FS-RECS FS-DEPTH @ cells + ! ;

: FS-PATHZ ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   u 1 + FS-PATH-CAP > IF E-FS-PATH throw THEN
   a FS-PATHZ-BUF u COPY-BYTES
   0 FS-PATHZ-BUF u + c!
   FS-PATHZ-BUF ;

: EXISTS? ( ptr u8 n -- bool )
   FS-PATHZ 0 access 0= ;

: STAT-MODE ( ptr u8 n -- n ) {: a:ptr u :}
   a u FS-PATHZ FS-STAT-BUF stat64 0 < IF -1 exit THEN
   FS-STAT-BUF 4 + U16@ ;

: FILE? ( ptr u8 n -- bool )
   STAT-MODE dup 0 < IF drop LINT-FALSE exit THEN
   S-IFMT and S-IFREG = ;

: DIR? ( ptr u8 n -- bool )
   STAT-MODE dup 0 < IF drop LINT-FALSE exit THEN
   S-IFMT and S-IFDIR = ;

: STR-SUFFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < IF LINT-FALSE exit THEN
   a u v - + v b v STR= ;

: HAS-EXT? ( ptr u8 n ptr u8 n -- bool )
   STR-SUFFIX? ;

: BASENAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u FS-I !
   begin FS-I @ 0 > while
      a FS-I @ 1 - + c@ FS-SLASH = IF
         a FS-I @ +  u FS-I @ -  exit
      THEN
      FS-I @ 1 - FS-I !
   repeat
   a u ;

: PATH= ( ptr u8 n ptr u8 n -- bool )
   STR= ;

: DOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 = IF a c@ FS-DOT = ELSE LINT-FALSE THEN ;

: DOTDOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 2 = IF a c@ FS-DOT =  a 1 + c@ FS-DOT =  and ELSE LINT-FALSE THEN ;

: SKIP-DIR? ( ptr u8 n -- bool )
   BASENAME
   2dup s" .jj" PATH= IF 2drop LINT-TRUE exit THEN
   2dup s" .git" PATH= IF 2drop LINT-TRUE exit THEN
   s" .dots" PATH= ;

: SKIP-ENTRY? ( ptr u8 n -- bool )
   2dup DOT-ENTRY? IF 2drop LINT-TRUE exit THEN
   2dup DOTDOT-ENTRY? IF 2drop LINT-TRUE exit THEN
   SKIP-DIR? ;

: JOIN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n ) {: pa:ptr pu na:ptr nu dst:ptr :}
   pu 0 > IF pa pu 1 - + c@ FS-SLASH = ELSE LINT-FALSE THEN IF
      pu nu + FS-PATH-CAP > IF E-FS-PATH throw THEN
      pa dst pu COPY-BYTES
      na dst pu + nu COPY-BYTES
      pu nu +
   ELSE
      pu 1 + nu + FS-PATH-CAP > IF E-FS-PATH throw THEN
      pa dst pu COPY-BYTES
      FS-SLASH dst pu + c!
      na dst pu 1 + + nu COPY-BYTES
      pu 1 + nu +
   THEN ;

: OPEN-DIR ( ptr u8 n -- n )
   FS-PATHZ open-rd dup 0 < IF E-FS-OPEN throw THEN ;

: D-RECLEN ( ptr u8 -- n )
   16 + U16@ ;

: D-NAMELEN ( ptr u8 -- n )
   18 + U16@ ;

: D-NAME ( ptr u8 -- ptr u8 n )
   dup 21 +  swap D-NAMELEN ;

: READ-DIR ( -- bool )
   FS-FD@ FS-DIR@ FS-DIR-CAP FS-BASE@ getdirentries64
   dup 0 < IF FS-FD@ close E-FS-DIR throw THEN
   dup FS-N! 0 > ;

: WALK-PATH ( ptr u8 n -- ) {: a:ptr u :}
   a u SKIP-DIR? IF exit THEN
   a u FILE? IF a u FS-WALK-XT @ execute exit THEN
   a u DIR? 0= IF E-FS-STAT throw THEN
   FS-DEPTH @ 1 + FS-MAX-DEPTH >= IF E-FS-DEPTH throw THEN
   a u OPEN-DIR FS-FD!
   0 FS-BASE@ !
   begin READ-DIR while
      0 FS-OFF!
      begin FS-OFF@ FS-N@ < while
         FS-DIR@ FS-OFF@ + FS-ENT !
         FS-ENT @ D-RECLEN dup 0 <= IF FS-FD@ close E-FS-DIR throw THEN FS-REC!
         FS-ENT @ D-NAME 2dup SKIP-ENTRY? IF
            2drop
         ELSE
            FS-NAME-U !  FS-NAME-A !
            a u FS-NAME-A @ FS-NAME-U @ FS-NEXT JOIN-PATH FS-CHILD-U !
            FS-DEPTH @ 1 + FS-DEPTH !
            FS-CUR FS-CHILD-U @ recurse
            FS-DEPTH @ 1 - FS-DEPTH !
         THEN
         FS-OFF@ FS-REC@ + FS-OFF!
      repeat
   repeat
   FS-FD@ close ;

: WALK-FILES ( ptr u8 n n -- ) {: a:ptr u xt :}
   xt FS-WALK-XT !
   0 FS-DEPTH !
   a FS-CUR u COPY-BYTES
   FS-CUR u WALK-PATH ;
