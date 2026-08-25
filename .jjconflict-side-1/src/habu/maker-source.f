\ maker-source.f - stat-sized source input shared by the REPL and AOT makers.

package MAKER-SOURCE

256 constant STAT-CAP
96 constant STAT-SIZE-OFF
$1002 constant MAP-ANON

create STAT-BUF STAT-CAP allot
create PROBE 1 allot
PTR-VARIABLE BUF-A
variable BUF-CAP
variable FD
variable OFF

: BUF ( -- ptr u8 )
   BUF-A @ ;

: SIZE ( ptr u8 n -- n )
   PATH0 STAT-BUF stat64 0 < IF s" maker-source: cannot stat source" 74 die THEN
   STAT-BUF STAT-SIZE-OFF + @ dup 0 <= IF s" maker-source: empty source" 74 die THEN ;

\ mmap returns an untyped syscall result; this is the reader's one pointer boundary.
TRUSTED: ALLOC ( n -- ptr u8 ) {: cap:n :}
   0 cap 3 MAP-ANON -1 0 mmap dup 0 < IF s" maker-source: mmap failed" 74 die THEN ;

: INSTALL ( n -- ) {: cap:n :}
   BUF {: old:ptr :}
   BUF-CAP @ {: oldcap:n :}
   cap ALLOC BUF-A !
   cap BUF-CAP !
   oldcap 0 > IF old oldcap munmap 0 < IF s" maker-source: munmap failed" 74 die THEN THEN ;

: CHECK-NEED ( n n -- n ) {: size:n slack:n :}
   slack 0 < IF s" maker-source: negative slack" 74 die THEN
   size slack + dup size < IF s" maker-source: size overflow" 74 die THEN ;

: READ-EXACT ( ptr u8 n n -- ) {: path:ptr pathu:n size:n :}
   path pathu PATH0 0 0 open FD !
   FD @ 0 < IF s" maker-source: cannot open source" 74 die THEN
   0 OFF !
   BEGIN OFF @ size < WHILE
      FD @ BUF OFF @ + size OFF @ - read {: got:n :}
      got 0 < IF FD @ close s" maker-source: read failed" 74 die THEN
      got 0= IF FD @ close s" maker-source: source size changed" 74 die THEN
      OFF @ got + OFF !
   REPEAT
   FD @ PROBE 1 read dup 0 < IF FD @ close s" maker-source: read failed" 74 die THEN
   0 <> IF FD @ close s" maker-source: source size changed" 74 die THEN
   FD @ close ;

public

: READ ( ptr u8 n n -- n ) {: path:ptr pathu:n slack:n :}
   path pathu SIZE {: size:n :}
   size slack CHECK-NEED INSTALL
   path pathu size READ-EXACT
   size ;

: SOURCE ( -- ptr u8 )
   BUF ;

: CAPACITY ( -- n )
   BUF-CAP @ ;

;package
