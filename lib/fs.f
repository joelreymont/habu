\ fs.f - checked filesystem helpers.
\
\ Load after lib/errors.f and lib/string.f.

require lib/adt/option.f                        \ option<n> for FS-TRY-*STAT-MODE (switchover wave A)

1024 constant FS-PATH-CAP
FS-PATH-CAP 1 + constant FS-PATHZ-CAP
4096 constant FS-DIR-CAP
32 constant FS-MAX-DEPTH
256 constant FS-STAT-CAP
8 constant FS-BYTE-BITS
16 constant FS-BYTE-BITS-2
24 constant FS-BYTE-BITS-3
32 constant FS-BYTE-BITS-4
40 constant FS-BYTE-BITS-5
48 constant FS-BYTE-BITS-6
56 constant FS-BYTE-BITS-7
4 constant FS-STAT-MODE-OFF
48 constant FS-STAT-MTIME-SEC-OFF
56 constant FS-STAT-MTIME-NS-OFF
64 constant FS-STAT-CTIME-SEC-OFF
72 constant FS-STAT-CTIME-NS-OFF
96 constant FS-STAT-SIZE-OFF
16 constant FS-DIRENT-RECLEN-OFF
18 constant FS-DIRENT-NAMELEN-OFF
21 constant FS-DIRENT-NAME-OFF
19 constant FS-LINUX-DIRENT-NAME-OFF
1 constant FS-O-WRONLY
2 constant FS-O-RDWR
$8 constant FS-O-APPEND
$200 constant FS-O-CREAT
$400 constant FS-O-TRUNC
420 constant FS-MODE-0644
1 constant FS-X-OK
1 constant FS-READ-PROBE-CAP

$F000 constant S-IFMT
$4000 constant S-IFDIR
$8000 constant S-IFREG
$A000 constant S-IFLNK

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

: FS-BYTE@ ( ptr u8 n -- n )
   BYTE+ c@ ;

: FS-U16@ ( ptr u8 -- n ) {: a:ptr :}
   a 0 FS-BYTE@ a 1 FS-BYTE@ FS-BYTE-BITS lshift or ;

: FS-U64@ ( ptr u8 -- n ) {: a:ptr :}
   a 0 FS-BYTE@
   a 1 FS-BYTE@ FS-BYTE-BITS lshift or
   a 2 FS-BYTE@ FS-BYTE-BITS-2 lshift or
   a 3 FS-BYTE@ FS-BYTE-BITS-3 lshift or
   a 4 FS-BYTE@ FS-BYTE-BITS-4 lshift or
   a 5 FS-BYTE@ FS-BYTE-BITS-5 lshift or
   a 6 FS-BYTE@ FS-BYTE-BITS-6 lshift or
   a 7 FS-BYTE@ FS-BYTE-BITS-7 lshift or ;

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

: FS-STAT-MODE@ ( -- n )
   FS-STAT-BUF FS-STAT-MODE-OFF + FS-U16@ ;

: FS-STAT-SIZE@ ( -- n )
   FS-STAT-BUF FS-STAT-SIZE-OFF + FS-U64@ ;

: FS-STAT-MTIME-SEC@ ( -- n )
   FS-STAT-BUF FS-STAT-MTIME-SEC-OFF + FS-U64@ ;

: FS-STAT-MTIME-NS@ ( -- n )
   FS-STAT-BUF FS-STAT-MTIME-NS-OFF + FS-U64@ ;

: FS-STAT-CTIME-SEC@ ( -- n )
   FS-STAT-BUF FS-STAT-CTIME-SEC-OFF + FS-U64@ ;

: FS-STAT-CTIME-NS@ ( -- n )
   FS-STAT-BUF FS-STAT-CTIME-NS-OFF + FS-U64@ ;

: FS-TRY-STAT ( ptr u8 n -- bool )
   FS-PATHZ FS-STAT-BUF stat64 0 < if FS-FALSE exit then
   FS-TRUE ;

: FS-TRY-LSTAT ( ptr u8 n -- bool )
   FS-PATHZ FS-STAT-BUF lstat64 0 < if FS-FALSE exit then
   FS-TRUE ;

: FS-TRY-STAT-MODE ( ptr u8 n -- option<n> )   \ SOME stat mode, NONE if missing/unstatable
   FS-TRY-STAT if FS-STAT-MODE@ OPTION:SOME else OPTION:NONE then ;

: FS-TRY-LSTAT-MODE ( ptr u8 n -- option<n> )   \ SOME lstat mode, NONE if missing/unstatable
   FS-TRY-LSTAT if FS-STAT-MODE@ OPTION:SOME else OPTION:NONE then ;

: STAT-MODE ( ptr u8 n -- n )
   FS-TRY-STAT-MODE MATCH option
     none OF E-FS-STAT throw ENDOF
     some OF ENDOF
   ;MATCH ;

: FILE-SIZE ( ptr u8 n -- n )
   FS-TRY-STAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then
   FS-STAT-SIZE@ ;

: FILE-META ( ptr u8 n -- n n n n n )
   FS-TRY-STAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and S-IFREG <> if E-FS-STAT throw then
   FS-STAT-SIZE@
   FS-STAT-MTIME-SEC@ FS-STAT-MTIME-NS@
   FS-STAT-CTIME-SEC@ FS-STAT-CTIME-NS@ ;

: FILE? ( ptr u8 n -- bool )
   FS-TRY-STAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFREG = ENDOF
   ;MATCH ;

: DIR? ( ptr u8 n -- bool )
   FS-TRY-STAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFDIR = ENDOF
   ;MATCH ;

: SYMLINK? ( ptr u8 n -- bool )
   FS-TRY-LSTAT-MODE MATCH option
     none OF FS-FALSE ENDOF
     some OF S-IFMT and S-IFLNK = ENDOF
   ;MATCH ;

: EXECUTABLE? ( ptr u8 n -- bool )
   FS-PATHZ FS-X-OK access 0= ;

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

: READ-LINK ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu dst:ptr cap :}
   cap 0 < if E-FS-CAPACITY throw then
   pa pu FS-TRY-LSTAT 0= if E-FS-STAT throw then
   FS-STAT-MODE@ S-IFMT and S-IFLNK <> if E-FS-STAT throw then
   FS-STAT-SIZE@ cap > if E-FS-CAPACITY throw then
   pa pu FS-PATHZ dst cap readlink {: n :}
   n 0 < if E-FS-IO throw then
   n ;

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
   FS-IO-FD @ 0 < if E-FS-OPEN throw then
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

: OPEN-APPEND-FD ( ptr u8 n -- n ) {: pa:ptr pu :}
   pa pu EXISTS? if pa pu FILE? 0= if E-FS-OPEN throw then then
   pa pu FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-APPEND or FS-MODE-0644 open {: fd :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: FS-DOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 = if a c@ FS-DOT = else FS-FALSE then ;

: FS-DOTDOT-ENTRY? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 2 = if a c@ FS-DOT = a 1 + c@ FS-DOT = and else FS-FALSE then ;

: FS-SKIP-DIR? ( ptr u8 n -- bool )
   BASENAME
   2dup s" .jj" FS-PATH= if 2drop FS-TRUE exit then
   2dup s" .git" FS-PATH= if 2drop FS-TRUE exit then
   s" .dots" FS-PATH= ;

: FS-SKIP-SELF-ENTRY? ( ptr u8 n -- bool )
   2dup FS-DOT-ENTRY? if 2drop FS-TRUE exit then
   FS-DOTDOT-ENTRY? ;

: FS-SKIP-ENTRY? ( ptr u8 n -- bool )
   2dup FS-SKIP-SELF-ENTRY? if 2drop FS-TRUE exit then
   FS-SKIP-DIR? ;

: FS-OPEN-DIR ( ptr u8 n -- n )
   FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN FS-THROW-WALK then ;

: FS-DIRENT-RECLEN ( ptr u8 -- n )
   FS-DIRENT-RECLEN-OFF BYTE+ FS-U16@ ;

: FS-TARGET-UNKNOWN ( -- )
   E-FS-DIR throw ;

: FS-DIRENT-NAME-OFFSET ( -- n )
   HB-TARGET-LINUX? if
      FS-LINUX-DIRENT-NAME-OFF exit
   then
   HB-TARGET-MACOS? if
      FS-DIRENT-NAME-OFF exit
   then
   FS-TARGET-UNKNOWN ;

: FS-LINUX-DIRENT-NAMELEN-SCAN ( ptr u8 n -- n ) {: ent:ptr rec :}
   0 begin dup FS-LINUX-DIRENT-NAME-OFF + rec < while
      ent over FS-LINUX-DIRENT-NAME-OFF + FS-BYTE@ 0= if exit then
      1+
   repeat
   E-FS-DIR FS-THROW-WALK ;

: FS-LINUX-DIRENT-NAMELEN ( ptr u8 -- n ) {: ent:ptr :}
   ent ent FS-DIRENT-RECLEN FS-LINUX-DIRENT-NAMELEN-SCAN ;

: FS-DIRENT-NAMELEN ( ptr u8 -- n )
   HB-TARGET-LINUX? if FS-LINUX-DIRENT-NAMELEN exit then
   HB-TARGET-MACOS? if FS-DIRENT-NAMELEN-OFF BYTE+ FS-U16@ exit then
   FS-TARGET-UNKNOWN ;

: FS-DIRENT-NAME ( ptr u8 -- ptr u8 n )
   dup FS-DIRENT-NAME-OFFSET BYTE+ swap FS-DIRENT-NAMELEN ;

: FS-DIRENT-NAME-END ( ptr u8 -- n ) {: ent:ptr :}
   FS-DIRENT-NAME-OFFSET ent FS-DIRENT-NAMELEN + ;

: FS-CHECK-RECORD ( -- )
   FS-REC@ 0 <= if E-FS-DIR FS-THROW-WALK then
   FS-OFF@ FS-REC@ + FS-N@ > if E-FS-DIR FS-THROW-WALK then
   FS-ENT @ FS-DIRENT-NAME-END FS-REC@ > if
      E-FS-DIR FS-THROW-WALK
   then ;

: FS-READ-DIR ( -- bool )
   FS-FD@ FS-CUR-DIR FS-DIR-CAP FS-BASE@ getdirentries64
   dup 0 < if drop E-FS-DIR FS-THROW-WALK then
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

: FS-CHECK-WALK-DESCEND ( -- )
   FS-DEPTH @ 1 + FS-MAX-DEPTH >= if E-FS-DEPTH FS-THROW-WALK then ;

: FS-OPEN-WALK-DIR ( ptr u8 n -- )
   FS-CHECK-WALK-DESCEND
   FS-OPEN-DIR FS-FD!
   0 FS-BASE@ ! ;

: FS-CLOSE-CUR-DIR ( -- )
   FS-FD@ close
   -1 FS-FD! ;

: FS-DIR-BLOCK-BEGIN ( -- )
   0 FS-OFF! ;

: FS-DIR-MORE? ( -- bool )
   FS-OFF@ FS-N@ < ;

: FS-LOAD-ENTRY ( -- )
   FS-CUR-DIR FS-OFF@ + FS-ENT !
   FS-ENT @ FS-DIRENT-RECLEN FS-REC!
   FS-CHECK-RECORD ;

: FS-ADVANCE-ENTRY ( -- )
   FS-OFF@ FS-REC@ + FS-OFF! ;

: FS-DESCEND-PATH ( ptr u8 n ptr u8 n -- ptr u8 n ) {: pa:ptr pu na:ptr nu :}
   pa pu nu FS-CHECK-WALK-JOIN-CAP
   pa pu na nu FS-NEXT-PATH JOIN-PATH FS-CHILD-U !
   FS-DEPTH @ 1 + FS-DEPTH !
   FS-CUR-PATH FS-CHILD-U @ ;

: FS-ASCEND-PATH ( -- )
   FS-DEPTH @ 1 - FS-DEPTH ! ;

: FS-WALK-PATH ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   a u FS-SKIP-DIR? if exit then
   a u FILE? if a u q execute exit then
   a u DIR? 0= if E-FS-STAT FS-THROW-WALK then
   a u FS-OPEN-WALK-DIR
   begin FS-READ-DIR while
      FS-DIR-BLOCK-BEGIN
      begin FS-DIR-MORE? while
         FS-LOAD-ENTRY
         FS-ENT @ FS-DIRENT-NAME 2dup FS-SKIP-ENTRY? if
            2drop
         else
            a u 2swap FS-DESCEND-PATH q recurse
            FS-ASCEND-PATH
         then
         FS-ADVANCE-ENTRY
      repeat
   repeat
   FS-CLOSE-CUR-DIR ;

: WALK-FILES ( ptr u8 n [ ptr u8 n -- ] -- ) {: a:ptr u q :}
   FS-FDS-RESET
   0 FS-DEPTH !
   a u FS-WALK-ROOT!
   FS-CUR-PATH u q FS-WALK-PATH ;
