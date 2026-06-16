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

: COPY-BYTES {: a dst u :} ( a dst u -- )
   0 begin dup u < while
      dup a + c@  over dst + c!
      1+
   repeat drop ;

: U16@ {: a :} ( a -- u )
   a c@  a 1 + c@ 8 lshift or ;

: FS-SLOT ( n -- a )
   FS-PATH-CAP * FS-WALK-BUF + ;

: FS-CUR ( -- a )
   FS-DEPTH @ FS-SLOT ;

: FS-NEXT ( -- a )
   FS-DEPTH @ 1 + FS-SLOT ;

: FS-DIR@ ( -- a )
   FS-DEPTH @ FS-DIR-CAP * FS-DIR-BUF + ;

: FS-BASE@ ( -- a )
   FS-DEPTH @ cells FS-BASES + ;

: FS-FD@ ( -- fd )
   FS-DEPTH @ cells FS-FDS + @ ;

: FS-FD! ( fd -- )
   FS-DEPTH @ cells FS-FDS + ! ;

: FS-N@ ( -- n )
   FS-DEPTH @ cells FS-NS + @ ;

: FS-N! ( n -- )
   FS-DEPTH @ cells FS-NS + ! ;

: FS-OFF@ ( -- n )
   FS-DEPTH @ cells FS-OFFS + @ ;

: FS-OFF! ( n -- )
   FS-DEPTH @ cells FS-OFFS + ! ;

: FS-REC@ ( -- n )
   FS-DEPTH @ cells FS-RECS + @ ;

: FS-REC! ( n -- )
   FS-DEPTH @ cells FS-RECS + ! ;

: FS-PATHZ {: a u :} ( a u -- z )
   u 1 + FS-PATH-CAP > IF E-FS-PATH throw THEN
   a FS-PATHZ-BUF u COPY-BYTES
   0 FS-PATHZ-BUF u + c!
   FS-PATHZ-BUF ;

: EXISTS? ( a u -- f )
   FS-PATHZ 0 access 0= ;

: STAT-MODE {: a u :} ( a u -- mode|-1 )
   a u FS-PATHZ FS-STAT-BUF stat64 0 < IF -1 exit THEN
   FS-STAT-BUF 4 + U16@ ;

: FILE? ( a u -- f )
   STAT-MODE dup 0 < IF drop 0 exit THEN
   S-IFMT and S-IFREG = ;

: DIR? ( a u -- f )
   STAT-MODE dup 0 < IF drop 0 exit THEN
   S-IFMT and S-IFDIR = ;

: STR-SUFFIX? {: a u b v :} ( a u b v -- f )
   u v < IF 0 exit THEN
   a u v - + v b v STR= ;

: HAS-EXT? ( a u ea eu -- f )
   STR-SUFFIX? ;

: BASENAME {: a u :} ( a u -- ba bu )
   u FS-I !
   begin FS-I @ 0 > while
      a FS-I @ 1 - + c@ FS-SLASH = IF
         a FS-I @ +  u FS-I @ -  exit
      THEN
      FS-I @ 1 - FS-I !
   repeat
   a u ;

: PATH= ( a u b v -- f )
   STR= ;

: DOT-ENTRY? {: a u :} ( a u -- f )
   u 1 = IF a c@ FS-DOT = ELSE 0 THEN ;

: DOTDOT-ENTRY? {: a u :} ( a u -- f )
   u 2 = IF a c@ FS-DOT =  a 1 + c@ FS-DOT =  and ELSE 0 THEN ;

: SKIP-DIR? ( a u -- f )
   BASENAME
   2dup s" .jj" PATH= IF 2drop -1 exit THEN
   2dup s" .git" PATH= IF 2drop -1 exit THEN
   s" .dots" PATH= ;

: SKIP-ENTRY? ( a u -- f )
   2dup DOT-ENTRY? IF 2drop -1 exit THEN
   2dup DOTDOT-ENTRY? IF 2drop -1 exit THEN
   SKIP-DIR? ;

: JOIN-PATH {: pa pu na nu dst :} ( pa pu na nu dst -- u )
   pu 0 > IF pa pu 1 - + c@ FS-SLASH = ELSE 0 THEN IF
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

: OPEN-DIR ( a u -- fd )
   FS-PATHZ open-rd dup 0 < IF E-FS-OPEN throw THEN ;

: D-RECLEN ( ent -- u )
   16 + U16@ ;

: D-NAMELEN ( ent -- u )
   18 + U16@ ;

: D-NAME ( ent -- a u )
   dup 21 +  swap D-NAMELEN ;

: READ-DIR ( -- more? )
   FS-FD@ FS-DIR@ FS-DIR-CAP FS-BASE@ getdirentries64
   dup 0 < IF FS-FD@ close E-FS-DIR throw THEN
   dup FS-N! 0 > ;

: WALK-PATH {: a u :} ( a u -- )
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

: WALK-FILES {: a u xt :} ( a u xt -- )
   xt FS-WALK-XT !
   0 FS-DEPTH !
   a FS-CUR u COPY-BYTES
   FS-CUR u WALK-PATH ;
