\ source.f - checked source materialization helpers.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, and lib/fs.f.

$20000 constant SOURCE-CAP
1 constant SOURCE-PROBE-CAP
4096 constant SOURCE-LS-READ-CAP
9 constant SOURCE-TAB
10 constant SOURCE-LF
13 constant SOURCE-CR
32 constant SOURCE-SPACE

create SOURCE-PROBE SOURCE-PROBE-CAP allot
create SOURCE-LS-READ-BUF SOURCE-LS-READ-CAP allot

variable SOURCE-BUF-A
variable SOURCE-LEN
variable SOURCE-RD
variable SOURCE-I
variable SOURCE-J
variable SOURCE-CUT
variable SOURCE-SKIP
variable SOURCE-END
variable SOURCE-LS-FD
variable SOURCE-LS-RD
variable SOURCE-LS-LEN
variable SOURCE-LS-LINE#

: SOURCE-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: SOURCE-PTR-U8@ ( ptr a -- ptr u8 )
   SOURCE-PTR-U8-FIELD @ ;

: SOURCE-PTR-U8! ( ptr u8 ptr a -- )
   SOURCE-PTR-U8-FIELD ! ;

: SOURCE-ALLOC-BUF ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

: SOURCE-BUF ( -- ptr u8 )
   SOURCE-BUF-A @ 0= if SOURCE-CAP SOURCE-ALLOC-BUF SOURCE-BUF-A SOURCE-PTR-U8! then
   SOURCE-BUF-A SOURCE-PTR-U8@ ;

: SOURCE-READ-PROBE ( -- )
   0 SOURCE-PROBE SOURCE-PROBE-CAP read SOURCE-RD !
   SOURCE-RD @ 0 < if E-FS-IO throw then
   SOURCE-RD @ 0 > if E-FS-CAPACITY throw then ;

: READ-STDIN-ALL ( ptr u8 len -- len ) {: dst:ptr cap :}
   cap LEN>N 0 < if E-FS-CAPACITY throw then
   0 >LEN SOURCE-LEN !
   begin
      cap LEN>N SOURCE-LEN @ LEN>N - dup 0 <= if
         drop SOURCE-READ-PROBE SOURCE-LEN @ exit
      then
      0 dst SOURCE-LEN @ LEN>N + rot read SOURCE-RD !
      SOURCE-RD @ 0 < if E-FS-IO throw then
      SOURCE-RD @ 0= if SOURCE-LEN @ exit then
      SOURCE-LEN @ LEN>N SOURCE-RD @ + >LEN SOURCE-LEN !
   again ;

: SOURCE-APPEND-BYTES ( ptr u8 len ptr u8 len ptr len -- )
   {: src:ptr u dst:ptr cap lenp:ptr :}
   u LEN>N 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N cap LEN>N > if E-FS-CAPACITY throw then
   u LEN>N cap LEN>N lenp @ LEN>N - > if E-FS-CAPACITY throw then
   src dst lenp @ LEN>N + u LEN>N BYTE-COPY
   lenp @ LEN>N u LEN>N + >LEN lenp ! ;

: SOURCE-PATH-A@ ( ptr a idx -- ptr u8 ) {: table:ptr idx :}
   idx IDX>N cells table + @ ;

: SOURCE-PATH-U@ ( ptr a idx -- len ) {: table:ptr idx :}
   idx IDX>N cells table + @ >LEN ;

: SOURCE-APPEND-FILE ( ptr u8 len ptr u8 len ptr len -- )
   {: path:ptr pathu dst:ptr cap lenp:ptr :}
   path pathu LEN>N dst lenp @ LEN>N + cap LEN>N lenp @ LEN>N - READ-ALL SOURCE-RD !
   lenp @ LEN>N SOURCE-RD @ + >LEN lenp ! ;

: CONCAT-FILES ( ptr a ptr a count ptr u8 len -- len )
   {: paths:ptr lens:ptr count dst:ptr cap :}
   count COUNT>N 0 < if E-FS-CAPACITY throw then
   cap LEN>N 0 < if E-FS-CAPACITY throw then
   0 >LEN SOURCE-LEN !
   0 begin dup count COUNT>N < while
      dup SOURCE-I !
      paths SOURCE-I @ >IDX SOURCE-PATH-A@ lens SOURCE-I @ >IDX SOURCE-PATH-U@ dst cap SOURCE-LEN SOURCE-APPEND-FILE
      1+
   repeat drop
   SOURCE-LEN @ ;

: WRITE-SOURCE-LIST ( ptr a ptr a count ptr u8 len -- )
   {: paths:ptr lens:ptr count out:ptr outu :}
   paths lens count SOURCE-BUF SOURCE-CAP >LEN CONCAT-FILES SOURCE-LEN !
   out outu LEN>N SOURCE-BUF SOURCE-LEN @ LEN>N WRITE-ALL ;

: SOURCE-FINAL-LINE-START ( ptr u8 len -- off ) {: src:ptr u :}
   u LEN>N 0= if 0 >OFF exit then
   u LEN>N >OFF SOURCE-I !
   src u LEN>N 1 - + c@ SOURCE-LF = if u LEN>N 1 - >OFF SOURCE-I ! then
   begin SOURCE-I @ OFF>N 0 > while
      src SOURCE-I @ OFF>N 1 - + c@ SOURCE-LF = if SOURCE-I @ exit then
      SOURCE-I @ OFF>N 1 - >OFF SOURCE-I !
   repeat
   0 >OFF ;

: INSERT-BEFORE-FINAL-LINE ( ptr u8 len ptr u8 len ptr u8 len -- len )
   {: src:ptr u ins:ptr insu dst:ptr cap :}
   src u SOURCE-FINAL-LINE-START SOURCE-CUT !
   0 >LEN SOURCE-LEN !
   src SOURCE-CUT @ OFF>N >LEN dst cap SOURCE-LEN SOURCE-APPEND-BYTES
   ins insu dst cap SOURCE-LEN SOURCE-APPEND-BYTES
   src SOURCE-CUT @ OFF>N + u LEN>N SOURCE-CUT @ OFF>N - >LEN dst cap SOURCE-LEN SOURCE-APPEND-BYTES
   SOURCE-LEN @ ;

: SOURCE-LINE-END ( ptr u8 len off -- off ) {: src:ptr u start :}
   start SOURCE-J !
   begin SOURCE-J @ OFF>N u LEN>N < while
      src SOURCE-J @ OFF>N + c@ SOURCE-LF = if SOURCE-J @ OFF>N 1 + >OFF exit then
      SOURCE-J @ OFF>N 1 + >OFF SOURCE-J !
   repeat
   u LEN>N >OFF ;

: SOURCE-LINE-SKIP-WS ( ptr u8 len -- off ) {: src:ptr u :}
   0 >OFF SOURCE-J !
   begin SOURCE-J @ OFF>N u LEN>N < while
      src SOURCE-J @ OFF>N + c@ dup SOURCE-SPACE = swap SOURCE-TAB = or if
         SOURCE-J @ OFF>N 1 + >OFF SOURCE-J !
      else
         SOURCE-J @ exit
      then
   repeat
   SOURCE-J @ ;

: SOURCE-EXPORT-LINE? ( ptr u8 len -- bool ) {: line:ptr lineu :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   line SOURCE-SKIP @ OFF>N + lineu LEN>N SOURCE-SKIP @ OFF>N - s" EXPORT " STARTS-WITH? ;

: SOURCE-APPEND-COMMENTED-EXPORT ( ptr u8 len ptr u8 len ptr len -- )
   {: line:ptr lineu dst:ptr cap lenp:ptr :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   s" \ " >LEN dst cap lenp SOURCE-APPEND-BYTES
   line SOURCE-SKIP @ OFF>N + lineu LEN>N SOURCE-SKIP @ OFF>N - >LEN dst cap lenp SOURCE-APPEND-BYTES ;

: SOURCE-APPEND-COMMENT-LINE ( ptr u8 len ptr u8 len ptr len -- )
   {: line:ptr lineu dst:ptr cap lenp:ptr :}
   line lineu SOURCE-EXPORT-LINE? if
      line lineu dst cap lenp SOURCE-APPEND-COMMENTED-EXPORT
   else
      line lineu dst cap lenp SOURCE-APPEND-BYTES
   then ;

: COMMENT-EXPORTS ( ptr u8 len ptr u8 len -- len ) {: src:ptr u dst:ptr cap :}
   0 >LEN SOURCE-LEN !
   0 >OFF SOURCE-I !
   begin SOURCE-I @ OFF>N u LEN>N < while
      src u SOURCE-I @ SOURCE-LINE-END SOURCE-END !
      src SOURCE-I @ OFF>N + SOURCE-END @ OFF>N SOURCE-I @ OFF>N - >LEN dst cap SOURCE-LEN SOURCE-APPEND-COMMENT-LINE
      SOURCE-END @ SOURCE-I !
   repeat
   SOURCE-LEN @ ;

: SOURCE-LS-CLOSE ( -- )
   SOURCE-LS-FD @ dup 0 >= if close else drop then
   -1 SOURCE-LS-FD ! ;

: SOURCE-LS-THROW ( n -- )
   SOURCE-LS-CLOSE
   throw ;

: SOURCE-LS-OPEN ( ptr u8 n -- )
   -1 SOURCE-LS-FD !
   FS-PATHZ open-rd SOURCE-LS-FD !
   SOURCE-LS-FD @ 0 < if E-FS-OPEN throw then ;

: SOURCE-LS-READ ( -- n )
   SOURCE-LS-FD @ SOURCE-LS-READ-BUF SOURCE-LS-READ-CAP read SOURCE-LS-RD !
   SOURCE-LS-RD @ 0 < if E-FS-IO SOURCE-LS-THROW then
   SOURCE-LS-RD @ SOURCE-LS-READ-CAP > if E-FS-IO SOURCE-LS-THROW then
   SOURCE-LS-RD @ ;

: SOURCE-LS-TRIM-CR ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u 0 > if
      a u 1 - + c@ SOURCE-CR = if a u 1 - exit then
   then
   a u ;

: SOURCE-LS-APPEND ( n ptr u8 n -- ) {: c line:ptr cap :}
   SOURCE-LS-LEN @ cap >= if E-FS-CAPACITY SOURCE-LS-THROW then
   c line SOURCE-LS-LEN @ + c!
   SOURCE-LS-LEN @ 1+ SOURCE-LS-LEN ! ;

: SOURCE-LS-EMIT ( ptr u8 [ ptr u8 n n -- ] -- ) {: line:ptr q :}
   SOURCE-LS-LINE# @ 1+ SOURCE-LS-LINE# !
   line SOURCE-LS-LEN @ SOURCE-LS-TRIM-CR SOURCE-LS-LINE# @ q execute
   0 SOURCE-LS-LEN ! ;

: SOURCE-LS-BYTE ( n ptr u8 n [ ptr u8 n n -- ] -- ) {: c line:ptr cap q :}
   c SOURCE-LF = if line q SOURCE-LS-EMIT exit then
   c line cap SOURCE-LS-APPEND ;

: SOURCE-LS-CHUNK ( n ptr u8 n [ ptr u8 n n -- ] -- ) {: got line:ptr cap q :}
   0 begin dup got < while
      SOURCE-LS-READ-BUF over + c@ line cap q SOURCE-LS-BYTE
      1+
   repeat drop ;

: SOURCE-LS-DRAIN ( ptr u8 n [ ptr u8 n n -- ] -- ) {: line:ptr cap q :}
   cap 0 <= if E-FS-CAPACITY SOURCE-LS-THROW then
   0 SOURCE-LS-LEN !
   0 SOURCE-LS-LINE# !
   begin SOURCE-LS-READ dup 0 > while
      line cap q SOURCE-LS-CHUNK
   repeat drop
   SOURCE-LS-LEN @ 0 > if line q SOURCE-LS-EMIT then
   SOURCE-LS-CLOSE ;

: SOURCE-FILE-LINES ( ptr u8 n ptr u8 n [ ptr u8 n n -- ] -- ) {: path:ptr pathu line:ptr cap q :}
   path pathu SOURCE-LS-OPEN
   line cap q SOURCE-LS-DRAIN ;
