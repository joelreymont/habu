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
   lenp @ LEN>N u LEN>N + cap LEN>N > if E-FS-CAPACITY throw then
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
