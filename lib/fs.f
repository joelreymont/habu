\ fs.f - checked filesystem helpers.
\
\ Load after lib/errors.f and lib/string.f.

1024 constant FS-PATH-CAP
FS-PATH-CAP 1 + constant FS-PATHZ-CAP
256 constant FS-STAT-CAP
8 constant FS-BYTE-BITS
4 constant FS-STAT-MODE-OFF

$F000 constant S-IFMT
$4000 constant S-IFDIR
$8000 constant S-IFREG

$2F constant FS-SLASH

create FS-PATHZ-BUF FS-PATHZ-CAP allot
create FS-STAT-BUF FS-STAT-CAP allot

: FS-FALSE ( -- bool )
   0 0= 0= ;

: FS-TRUE ( -- bool )
   0 0= ;

: FS-U16@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ a 1 + c@ FS-BYTE-BITS lshift or ;

: FS-CHECK-JOIN-CAP ( n -- )
   dup FS-PATH-CAP > if E-FS-CAPACITY throw then drop ;

: FS-PATHZ ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a FS-PATHZ-BUF u BYTE-COPY
   0 FS-PATHZ-BUF u + c!
   FS-PATHZ-BUF ;

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
