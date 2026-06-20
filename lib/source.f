\ source.f - checked source materialization helpers.
\
\ Load after lib/errors.f, lib/string.f, and lib/fs.f.

$20000 constant SOURCE-CAP
1 constant SOURCE-PROBE-CAP
9 constant SOURCE-TAB
10 constant SOURCE-LF
32 constant SOURCE-SPACE

create SOURCE-BUF SOURCE-CAP allot
create SOURCE-PROBE SOURCE-PROBE-CAP allot

variable SOURCE-LEN
variable SOURCE-RD
variable SOURCE-I
variable SOURCE-J
variable SOURCE-CUT
variable SOURCE-SKIP
variable SOURCE-END

: SOURCE-READ-PROBE ( -- )
   0 SOURCE-PROBE SOURCE-PROBE-CAP read SOURCE-RD !
   SOURCE-RD @ 0 < if E-FS-IO throw then
   SOURCE-RD @ 0 > if E-FS-CAPACITY throw then ;

: READ-STDIN-ALL ( ptr u8 n -- n ) {: dst:ptr cap :}
   cap 0 < if E-FS-CAPACITY throw then
   0 SOURCE-LEN !
   begin
      cap SOURCE-LEN @ - dup 0 <= if
         drop SOURCE-READ-PROBE SOURCE-LEN @ exit
      then
      0 dst SOURCE-LEN @ + rot read SOURCE-RD !
      SOURCE-RD @ 0 < if E-FS-IO throw then
      SOURCE-RD @ 0= if SOURCE-LEN @ exit then
      SOURCE-LEN @ SOURCE-RD @ + SOURCE-LEN !
   again ;

: SOURCE-APPEND-BYTES ( ptr u8 n ptr u8 n ptr n -- )
   {: src:ptr u dst:ptr cap lenp:ptr :}
   u 0 < if E-FS-CAPACITY throw then
   lenp @ u + cap > if E-FS-CAPACITY throw then
   src dst lenp @ + u BYTE-COPY
   lenp @ u + lenp ! ;

: SOURCE-PATH-A@ ( ptr a n -- ptr u8 ) {: table:ptr idx :}
   idx cells table + @ ;

: SOURCE-PATH-U@ ( ptr a n -- n ) {: table:ptr idx :}
   idx cells table + @ ;

: SOURCE-APPEND-FILE ( ptr u8 n ptr u8 n ptr n -- )
   {: path:ptr pathu dst:ptr cap lenp:ptr :}
   path pathu dst lenp @ + cap lenp @ - READ-ALL SOURCE-RD !
   lenp @ SOURCE-RD @ + lenp ! ;

: CONCAT-FILES ( ptr a ptr a n ptr u8 n -- n )
   {: paths:ptr lens:ptr count dst:ptr cap :}
   count 0 < if E-FS-CAPACITY throw then
   cap 0 < if E-FS-CAPACITY throw then
   0 SOURCE-LEN !
   0 begin dup count < while
      dup SOURCE-I !
      paths SOURCE-I @ SOURCE-PATH-A@ lens SOURCE-I @ SOURCE-PATH-U@ dst cap SOURCE-LEN SOURCE-APPEND-FILE
      1+
   repeat drop
   SOURCE-LEN @ ;

: WRITE-SOURCE-LIST ( ptr a ptr a n ptr u8 n -- )
   {: paths:ptr lens:ptr count out:ptr outu :}
   paths lens count SOURCE-BUF SOURCE-CAP CONCAT-FILES SOURCE-LEN !
   out outu SOURCE-BUF SOURCE-LEN @ WRITE-ALL ;

: SOURCE-FINAL-LINE-START ( ptr u8 n -- n ) {: src:ptr u :}
   u 0= if 0 exit then
   u SOURCE-I !
   src u 1 - + c@ SOURCE-LF = if u 1 - SOURCE-I ! then
   begin SOURCE-I @ 0 > while
      src SOURCE-I @ 1 - + c@ SOURCE-LF = if SOURCE-I @ exit then
      SOURCE-I @ 1 - SOURCE-I !
   repeat
   0 ;

: INSERT-BEFORE-FINAL-LINE ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: src:ptr u ins:ptr insu dst:ptr cap :}
   src u SOURCE-FINAL-LINE-START SOURCE-CUT !
   0 SOURCE-LEN !
   src SOURCE-CUT @ dst cap SOURCE-LEN SOURCE-APPEND-BYTES
   ins insu dst cap SOURCE-LEN SOURCE-APPEND-BYTES
   src SOURCE-CUT @ + u SOURCE-CUT @ - dst cap SOURCE-LEN SOURCE-APPEND-BYTES
   SOURCE-LEN @ ;

: SOURCE-LINE-END ( ptr u8 n n -- n ) {: src:ptr u start :}
   start SOURCE-J !
   begin SOURCE-J @ u < while
      src SOURCE-J @ + c@ SOURCE-LF = if SOURCE-J @ 1 + exit then
      SOURCE-J @ 1 + SOURCE-J !
   repeat
   u ;

: SOURCE-LINE-SKIP-WS ( ptr u8 n -- n ) {: src:ptr u :}
   0 SOURCE-J !
   begin SOURCE-J @ u < while
      src SOURCE-J @ + c@ dup SOURCE-SPACE = swap SOURCE-TAB = or if
         SOURCE-J @ 1 + SOURCE-J !
      else
         SOURCE-J @ exit
      then
   repeat
   SOURCE-J @ ;

: SOURCE-EXPORT-LINE? ( ptr u8 n -- bool ) {: line:ptr lineu :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   line SOURCE-SKIP @ + lineu SOURCE-SKIP @ - s" EXPORT " STARTS-WITH? ;

: SOURCE-APPEND-COMMENTED-EXPORT ( ptr u8 n ptr u8 n ptr n -- )
   {: line:ptr lineu dst:ptr cap lenp:ptr :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   s" \ " dst cap lenp SOURCE-APPEND-BYTES
   line SOURCE-SKIP @ + lineu SOURCE-SKIP @ - dst cap lenp SOURCE-APPEND-BYTES ;

: SOURCE-APPEND-COMMENT-LINE ( ptr u8 n ptr u8 n ptr n -- )
   {: line:ptr lineu dst:ptr cap lenp:ptr :}
   line lineu SOURCE-EXPORT-LINE? if
      line lineu dst cap lenp SOURCE-APPEND-COMMENTED-EXPORT
   else
      line lineu dst cap lenp SOURCE-APPEND-BYTES
   then ;

: COMMENT-EXPORTS ( ptr u8 n ptr u8 n -- n ) {: src:ptr u dst:ptr cap :}
   0 SOURCE-LEN !
   0 SOURCE-I !
   begin SOURCE-I @ u < while
      src u SOURCE-I @ SOURCE-LINE-END SOURCE-END !
      src SOURCE-I @ + SOURCE-END @ SOURCE-I @ - dst cap SOURCE-LEN SOURCE-APPEND-COMMENT-LINE
      SOURCE-END @ SOURCE-I !
   repeat
   SOURCE-LEN @ ;
