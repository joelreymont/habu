\ fs.f - checked filesystem helpers.
\
\ Load after lib/errors.f and lib/string.f.

1024 constant FS-PATH-CAP
FS-PATH-CAP 1 + constant FS-PATHZ-CAP
4096 constant FS-DIR-CAP
32 constant FS-MAX-DEPTH
256 constant FS-STAT-CAP
8 constant FS-BYTE-BITS
4 constant FS-STAT-MODE-OFF
16 constant FS-DIRENT-RECLEN-OFF
18 constant FS-DIRENT-NAMELEN-OFF
21 constant FS-DIRENT-NAME-OFF
1 constant FS-O-WRONLY
$8 constant FS-O-APPEND
$200 constant FS-O-CREAT
$400 constant FS-O-TRUNC
420 constant FS-MODE-0644
1 constant FS-READ-PROBE-CAP

$F000 constant S-IFMT
$4000 constant S-IFDIR
$8000 constant S-IFREG

$2E constant FS-DOT
$2F constant FS-SLASH

create FS-PATHZ-BUF FS-PATHZ-CAP allot
create FS-READ-PROBE FS-READ-PROBE-CAP allot
create FS-STAT-BUF FS-STAT-CAP allot
create FS-WALK-BUF FS-MAX-DEPTH FS-PATH-CAP * allot
create FS-DIR-BUF FS-MAX-DEPTH FS-DIR-CAP * allot
create FS-BASES FS-MAX-DEPTH cells allot
create FS-FDS FS-MAX-DEPTH cells allot
create FS-NS FS-MAX-DEPTH cells allot
create FS-OFFS FS-MAX-DEPTH cells allot
create FS-RECS FS-MAX-DEPTH cells allot

variable FS-DEPTH
variable FS-CHILD-U
variable FS-ENT
variable FS-NAME-A
variable FS-NAME-U
variable FS-IO-FD
variable FS-IO-LEN
variable FS-IO-RD
variable FS-IO-OFF
variable FS-IO-WR

: FS-FALSE ( -- bool )
   0 0= 0= ;

: FS-TRUE ( -- bool )
   0 0= ;

: FS-U16@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ a 1 + c@ FS-BYTE-BITS lshift or ;

: FS-CHECK-DEPTH ( n -- ) {: d :}
   d 0 < if E-FS-DEPTH throw then
   d FS-MAX-DEPTH >= if E-FS-DEPTH throw then ;

: FS-PATH-SLOT ( n -- ptr u8 ) {: d :}
   d FS-CHECK-DEPTH
   d FS-PATH-CAP * FS-WALK-BUF + ;

: FS-DIR-SLOT ( n -- ptr u8 ) {: d :}
   d FS-CHECK-DEPTH
   d FS-DIR-CAP * FS-DIR-BUF + ;

: FS-CUR-PATH ( -- ptr u8 )
   FS-DEPTH @ FS-PATH-SLOT ;

: FS-NEXT-PATH ( -- ptr u8 )
   FS-DEPTH @ 1 + FS-PATH-SLOT ;

: FS-CUR-DIR ( -- ptr u8 )
   FS-DEPTH @ FS-DIR-SLOT ;

: FS-BASE@ ( -- ptr n )
   FS-DEPTH @ cells FS-BASES + ;

: FS-FD-PTR ( n -- ptr n ) {: d :}
   d FS-CHECK-DEPTH
   d cells FS-FDS + ;

: FS-FD@ ( -- n )
   FS-DEPTH @ FS-FD-PTR @ ;

: FS-FD! ( n -- ) {: fd :}
   fd FS-DEPTH @ FS-FD-PTR ! ;

: FS-FDS-RESET ( -- )
   0 begin dup FS-MAX-DEPTH < while
      -1 over FS-FD-PTR !
      1+
   repeat drop ;

: FS-CLOSE-FD-AT ( n -- ) {: d :}
   d FS-FD-PTR @ dup 0 >= if
      close -1 d FS-FD-PTR !
   else
      drop
   then ;

: FS-CLOSE-WALK ( -- )
   FS-DEPTH @ begin dup 0 >= while
      dup FS-CLOSE-FD-AT
      1-
   repeat drop ;

: FS-THROW-WALK ( n -- ) {: code :}
   FS-CLOSE-WALK
   code throw ;

: FS-N@ ( -- n )
   FS-DEPTH @ cells FS-NS + @ ;

: FS-N! ( n -- ) {: n :}
   n FS-DEPTH @ cells FS-NS + ! ;

: FS-OFF@ ( -- n )
   FS-DEPTH @ cells FS-OFFS + @ ;

: FS-OFF! ( n -- ) {: off :}
   off FS-DEPTH @ cells FS-OFFS + ! ;

: FS-REC@ ( -- n )
   FS-DEPTH @ cells FS-RECS + @ ;

: FS-REC! ( n -- ) {: rec :}
   rec FS-DEPTH @ cells FS-RECS + ! ;

: FS-CHECK-JOIN-CAP ( n -- )
   dup FS-PATH-CAP > if E-FS-CAPACITY throw then drop ;

: FS-PATHZ-INTO ( ptr u8 n ptr u8 -- ptr u8 ) {: a:ptr u dst:ptr :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   0 dst u + c!
   dst ;

: FS-PATHZ ( ptr u8 n -- ptr u8 )
   FS-PATHZ-BUF FS-PATHZ-INTO ;

: EXISTS? ( ptr u8 n -- bool )
   FS-PATHZ 0 access 0= ;

: FS-TRY-STAT-MODE ( ptr u8 n -- n ) {: a:ptr u :}
   a u FS-PATHZ FS-STAT-BUF stat64 0 < if -1 exit then
   FS-STAT-BUF FS-STAT-MODE-OFF + FS-U16@ ;

: STAT-MODE ( ptr u8 n -- n )
   FS-TRY-STAT-MODE dup 0 < if E-FS-STAT throw then ;

: FILE? ( ptr u8 n -- bool )
   FS-TRY-STAT-MODE dup 0 < if
      drop FS-FALSE
   else
      S-IFMT and S-IFREG =
   then ;

: DIR? ( ptr u8 n -- bool )
   FS-TRY-STAT-MODE dup 0 < if
      drop FS-FALSE
   else
      S-IFMT and S-IFDIR =
   then ;

: BASENAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u begin dup 0 > while
      dup 1 - a + c@ FS-SLASH = if
         a over + u rot - exit
      then
      1-
   repeat
   drop a u ;

: JOIN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n ) {: pa:ptr pu na:ptr nu dst:ptr :}
   pu 0 < if E-FS-PATH throw then
   nu 0 < if E-FS-PATH throw then
   pu 0 > if pa pu 1 - + c@ FS-SLASH = else FS-FALSE then if
      pu nu + FS-CHECK-JOIN-CAP
      pa dst pu BYTE-COPY
      na dst pu + nu BYTE-COPY
      pu nu +
   else
      pu 1 + nu + FS-CHECK-JOIN-CAP
      pa dst pu BYTE-COPY
      FS-SLASH dst pu + c!
      na dst pu 1 + + nu BYTE-COPY
      pu 1 + nu +
   then ;

: FS-PATH= ( ptr u8 n ptr u8 n -- bool )
   STR= ;

: READ-ALL ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu dst:ptr cap :}
   cap 0 < if E-FS-CAPACITY throw then
   pa pu FS-PATHZ open-rd FS-IO-FD !
   FS-IO-FD @ 0 < if E-FS-OPEN throw then
   0 FS-IO-LEN !
   begin FS-IO-LEN @ cap < while
      FS-IO-FD @ dst FS-IO-LEN @ + cap FS-IO-LEN @ - read FS-IO-RD !
      FS-IO-RD @ 0 < if FS-IO-FD @ close E-FS-IO throw then
      FS-IO-RD @ cap FS-IO-LEN @ - > if FS-IO-FD @ close E-FS-IO throw then
      FS-IO-RD @ 0= if FS-IO-FD @ close FS-IO-LEN @ exit then
      FS-IO-LEN @ FS-IO-RD @ + FS-IO-LEN !
   repeat
   FS-IO-FD @ FS-READ-PROBE FS-READ-PROBE-CAP read FS-IO-RD !
   FS-IO-RD @ 0 < if FS-IO-FD @ close E-FS-IO throw then
   FS-IO-RD @ 0 > if FS-IO-FD @ close E-FS-CAPACITY throw then
   FS-IO-FD @ close
   FS-IO-LEN @ ;

: FS-WRITE-BY-FLAGS ( ptr u8 n ptr u8 n n -- ) {: pa:ptr pu src:ptr u flags :}
   u 0 < if E-FS-CAPACITY throw then
   pa pu EXISTS? if pa pu FILE? 0= if E-FS-OPEN throw then then
   pa pu FS-PATHZ flags FS-MODE-0644 open FS-IO-FD !
   pa pu FILE? 0= if E-FS-OPEN throw then
   0 FS-IO-OFF !
   begin FS-IO-OFF @ u < while
      FS-IO-FD @ src FS-IO-OFF @ + u FS-IO-OFF @ - write FS-IO-WR !
      FS-IO-WR @ 0 <= if FS-IO-FD @ close E-FS-IO throw then
      FS-IO-WR @ u FS-IO-OFF @ - > if FS-IO-FD @ close E-FS-IO throw then
      FS-IO-OFF @ FS-IO-WR @ + FS-IO-OFF !
   repeat
   FS-IO-FD @ close ;

: WRITE-ALL ( ptr u8 n ptr u8 n -- )
   FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-WRITE-BY-FLAGS ;

: APPEND-FILE ( ptr u8 n ptr u8 n -- )
   FS-O-WRONLY FS-O-CREAT or FS-O-APPEND or FS-WRITE-BY-FLAGS ;

: FS-DOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 = if a c@ FS-DOT = else FS-FALSE then ;

: FS-DOTDOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 2 = if a c@ FS-DOT = a 1 + c@ FS-DOT = and else FS-FALSE then ;

: FS-SKIP-DIR? ( ptr u8 n -- bool )
   BASENAME
   2dup s" .jj" FS-PATH= if 2drop FS-TRUE exit then
   2dup s" .git" FS-PATH= if 2drop FS-TRUE exit then
   s" .dots" FS-PATH= ;

: FS-SKIP-ENTRY? ( ptr u8 n -- bool )
   2dup FS-DOT-ENTRY? if 2drop FS-TRUE exit then
   2dup FS-DOTDOT-ENTRY? if 2drop FS-TRUE exit then
   FS-SKIP-DIR? ;

: FS-OPEN-DIR ( ptr u8 n -- n )
   FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN FS-THROW-WALK 0 then ;

: FS-DIRENT-RECLEN ( ptr u8 -- n )
   FS-DIRENT-RECLEN-OFF + FS-U16@ ;

: FS-DIRENT-NAMELEN ( ptr u8 -- n )
   FS-DIRENT-NAMELEN-OFF + FS-U16@ ;

: FS-DIRENT-NAME ( ptr u8 -- ptr u8 n )
   dup FS-DIRENT-NAME-OFF + swap FS-DIRENT-NAMELEN ;

: FS-CHECK-RECORD ( -- )
   FS-REC@ 0 <= if E-FS-DIR FS-THROW-WALK then
   FS-OFF@ FS-REC@ + FS-N@ > if E-FS-DIR FS-THROW-WALK then
   FS-ENT @ FS-DIRENT-NAMELEN FS-DIRENT-NAME-OFF + FS-REC@ > if
      E-FS-DIR FS-THROW-WALK
   then ;

: FS-READ-DIR ( -- bool )
   FS-FD@ FS-CUR-DIR FS-DIR-CAP FS-BASE@ getdirentries64
   dup 0 < if drop E-FS-DIR FS-THROW-WALK 0 then
   dup FS-N! 0 > ;

: FS-WALK-ROOT! ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a FS-CUR-PATH u BYTE-COPY ;

: FS-WALK-JOIN-LEN ( ptr u8 n n -- n ) {: pa:ptr pu nu :}
   pu 0 < if E-FS-PATH FS-THROW-WALK then
   nu 0 < if E-FS-PATH FS-THROW-WALK then
   pu 0 > if pa pu 1 - + c@ FS-SLASH = else FS-FALSE then if
      pu nu +
   else
      pu 1 + nu +
   then ;

: FS-CHECK-WALK-JOIN-CAP ( ptr u8 n n -- )
   FS-WALK-JOIN-LEN FS-PATH-CAP > if E-FS-CAPACITY FS-THROW-WALK then ;

: FS-WALK-PATH ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   a u FS-SKIP-DIR? if exit then
   a u FILE? if a u q execute exit then
   a u DIR? 0= if E-FS-STAT FS-THROW-WALK then
   FS-DEPTH @ 1 + FS-MAX-DEPTH >= if E-FS-DEPTH FS-THROW-WALK then
   a u FS-OPEN-DIR FS-FD!
   0 FS-BASE@ !
   begin FS-READ-DIR while
      0 FS-OFF!
      begin FS-OFF@ FS-N@ < while
         FS-CUR-DIR FS-OFF@ + FS-ENT !
         FS-ENT @ FS-DIRENT-RECLEN FS-REC!
         FS-CHECK-RECORD
         FS-ENT @ FS-DIRENT-NAME 2dup FS-SKIP-ENTRY? if
            2drop
         else
            FS-NAME-U ! FS-NAME-A !
            a u FS-NAME-U @ FS-CHECK-WALK-JOIN-CAP
            a u FS-NAME-A @ FS-NAME-U @ FS-NEXT-PATH JOIN-PATH FS-CHILD-U !
            FS-DEPTH @ 1 + FS-DEPTH !
            FS-CUR-PATH FS-CHILD-U @ q recurse
            FS-DEPTH @ 1 - FS-DEPTH !
         then
         FS-OFF@ FS-REC@ + FS-OFF!
      repeat
   repeat
   FS-FD@ close
   -1 FS-FD! ;

: WALK-FILES ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   FS-FDS-RESET
   0 FS-DEPTH !
   a u FS-WALK-ROOT!
   FS-CUR-PATH u q FS-WALK-PATH ;
