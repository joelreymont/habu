\ source.f - checked source materialization helpers.
\
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f

$20000 constant SOURCE-CAP
1 constant SOURCE-PROBE-CAP
9 constant SOURCE-TAB
10 constant SOURCE-LF
13 constant SOURCE-CR
32 constant SOURCE-SPACE
34 constant SOURCE-DQ
92 constant SOURCE-BACKSLASH

create SOURCE-PROBE SOURCE-PROBE-CAP allot

variable SOURCE-BUF-A
variable SOURCE-LEN
variable SOURCE-RD
variable SOURCE-I
variable SOURCE-J
variable SOURCE-CUT
variable SOURCE-SKIP
variable SOURCE-END

: SOURCE-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: SOURCE-PTR-U8@ ( ptr a -- ptr u8 )
   SOURCE-PTR-U8-FIELD @ ;

: SOURCE-PTR-U8! ( ptr u8 ptr a -- )
   SOURCE-PTR-U8-FIELD ! ;

\ Fixed-size buffer allocation. SOURCE-CAP is a positive library constant;
\ MEM:BYTES-ALLOC-LEN narrows the raw size to the validated alloc role before
\ MEM:ALLOC-BYTES, throwing E-MEM-SIZE on any refusal (unreachable for the constant).
: SOURCE-ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

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

: SOURCE-APPEND-C ( n ptr u8 len ptr len -- )
   {: c:n dst:ptr cap:len lenp:ptr :}
   lenp @ LEN>N 0 < if E-FS-CAPACITY throw then
   lenp @ LEN>N cap LEN>N >= if E-FS-CAPACITY throw then
   c dst lenp @ LEN>N + c!
   lenp @ LEN>N 1 + >LEN lenp ! ;

\ One shared checked path-string emitter. Path bytes that would change source
\ structure (`"`, `\`, LF, CR) are rejected fail-closed so materialized loader
\ lines and diagnostic prefix labels cannot be broken or injected by a path.
: SOURCE-PATH-BYTE-SAFE? ( n -- bool ) {: c:n :}
   c SOURCE-DQ = if STR-FALSE exit then
   c SOURCE-BACKSLASH = if STR-FALSE exit then
   c SOURCE-LF = if STR-FALSE exit then
   c SOURCE-CR = if STR-FALSE exit then
   STR-TRUE ;

: SOURCE-PATH-SAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ SOURCE-PATH-BYTE-SAFE? 0= if drop STR-FALSE exit then
      1+
   repeat drop STR-TRUE ;

: SOURCE-QPATH-CHECK ( ptr u8 n -- )
   SOURCE-PATH-SAFE? 0= if E-FS-PATH-UNSAFE throw then ;

: SOURCE-APPEND-QPATH ( ptr u8 len ptr u8 len ptr len -- )
   {: path:ptr pathu:len dst:ptr cap:len lenp:ptr :}
   path pathu LEN>N SOURCE-QPATH-CHECK
   s" s" >LEN dst cap lenp SOURCE-APPEND-BYTES
   SOURCE-DQ dst cap lenp SOURCE-APPEND-C
   SOURCE-SPACE dst cap lenp SOURCE-APPEND-C
   path pathu dst cap lenp SOURCE-APPEND-BYTES
   SOURCE-DQ dst cap lenp SOURCE-APPEND-C ;

: SOURCE-PATH-A@ ( ptr ptr u8 idx -- ptr u8 ) {: table:ptr idx :}
   idx IDX>N cells table + @ ;

: SOURCE-PATH-U@ ( ptr n idx -- len ) {: table:ptr idx :}
   idx IDX>N cells table + @ >LEN ;

: SOURCE-APPEND-FILE ( ptr u8 len ptr u8 len ptr len -- )
   {: path:ptr pathu:len dst:ptr cap:len lenp:ptr :}
   path pathu LEN>N dst lenp @ LEN>N + cap LEN>N lenp @ LEN>N - READ-ALL SOURCE-RD !
   lenp @ LEN>N SOURCE-RD @ + >LEN lenp ! ;

: SOURCE-APPEND-PROVIDED ( ptr u8 len ptr u8 len ptr len -- )
   {: path:ptr pathu:len dst:ptr cap:len lenp:ptr :}
   path pathu dst cap lenp SOURCE-APPEND-QPATH
   SOURCE-SPACE dst cap lenp SOURCE-APPEND-C
   s" provided" >LEN dst cap lenp SOURCE-APPEND-BYTES
   SOURCE-LF dst cap lenp SOURCE-APPEND-C ;

: SOURCE-APPEND-SOURCE-FILE ( ptr u8 len ptr u8 len ptr len -- )
   {: path:ptr pathu:len dst:ptr cap:len lenp:ptr :}
   path pathu dst cap lenp SOURCE-APPEND-PROVIDED
   path pathu dst cap lenp SOURCE-APPEND-FILE
   SOURCE-LF dst cap lenp SOURCE-APPEND-C ;

: CONCAT-FILES ( ptr ptr u8 ptr n count ptr u8 len -- len )
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

: WRITE-SOURCE-LIST ( ptr ptr u8 ptr n count ptr u8 len -- )
   {: paths:ptr lens:ptr count out:ptr outu :}
   count COUNT>N 0 < if E-FS-CAPACITY throw then
   0 >LEN SOURCE-LEN !
   0 begin dup count COUNT>N < while
      dup SOURCE-I !
      paths SOURCE-I @ >IDX SOURCE-PATH-A@ lens SOURCE-I @ >IDX SOURCE-PATH-U@ SOURCE-BUF SOURCE-CAP >LEN SOURCE-LEN SOURCE-APPEND-SOURCE-FILE
      1+
   repeat drop
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

\ Package-context tracking for the directive strip (dot
\ habu-compiler-pkg-re-688212c1): `EXPORT NAME` INSIDE an open package block is
\ the re-export declaration and must reach the compiler; only TOP-LEVEL
\ `EXPORT ` lines are the hb-build --repl directive to comment out. The
\ tracker is line-based like the strip itself (canonical line-leading
\ `package NAME` openers and `;package` closers). Both miss
\ modes fail safe: an uncommented top-level directive compiles as the engine
\ keyword's no-op, and a wrongly-commented in-package re-export leaves the
\ alias undefined so the build rejects loudly.
variable SOURCE-PKG-DEPTH

: SOURCE-LINE-LEAD$ ( ptr u8 len -- ptr u8 n ) {: line:ptr lineu:len :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   line SOURCE-SKIP @ OFF>N + lineu LEN>N SOURCE-SKIP @ OFF>N - ;

: SOURCE-PACKAGE-OPEN-LINE? ( ptr u8 len -- bool )
   SOURCE-LINE-LEAD$ s" package " STARTS-WITH? ;

: SOURCE-PACKAGE-CLOSE-LINE? ( ptr u8 len -- bool )
   SOURCE-LINE-LEAD$ {: a:ptr u:n :}
   a u s" ;package" STARTS-WITH? ;

: SOURCE-LINE-PKG-TRACK ( ptr u8 len -- ) {: line:ptr lineu:len :}
   line lineu SOURCE-PACKAGE-OPEN-LINE? if
      SOURCE-PKG-DEPTH @ 1 + SOURCE-PKG-DEPTH ! exit
   then
   line lineu SOURCE-PACKAGE-CLOSE-LINE? if
      SOURCE-PKG-DEPTH @ 0 > if SOURCE-PKG-DEPTH @ 1 - SOURCE-PKG-DEPTH ! then
   then ;

: SOURCE-APPEND-COMMENTED-EXPORT ( ptr u8 len ptr u8 len ptr len -- )
   {: line:ptr lineu dst:ptr cap lenp:ptr :}
   line lineu SOURCE-LINE-SKIP-WS SOURCE-SKIP !
   s" \ " >LEN dst cap lenp SOURCE-APPEND-BYTES
   line SOURCE-SKIP @ OFF>N + lineu LEN>N SOURCE-SKIP @ OFF>N - >LEN dst cap lenp SOURCE-APPEND-BYTES ;

: SOURCE-APPEND-COMMENT-LINE ( ptr u8 len ptr u8 len ptr len -- )
   {: line:ptr lineu dst:ptr cap lenp:ptr :}
   line lineu SOURCE-LINE-PKG-TRACK
   line lineu SOURCE-EXPORT-LINE? SOURCE-PKG-DEPTH @ 0 = and if
      line lineu dst cap lenp SOURCE-APPEND-COMMENTED-EXPORT
   else
      line lineu dst cap lenp SOURCE-APPEND-BYTES
   then ;

: COMMENT-EXPORTS ( ptr u8 len ptr u8 len -- len ) {: src:ptr u dst:ptr cap :}
   0 >LEN SOURCE-LEN !
   0 >OFF SOURCE-I !
   0 SOURCE-PKG-DEPTH !
   begin SOURCE-I @ OFF>N u LEN>N < while
      src u SOURCE-I @ SOURCE-LINE-END SOURCE-END !
      src SOURCE-I @ OFF>N + SOURCE-END @ OFF>N SOURCE-I @ OFF>N - >LEN dst cap SOURCE-LEN SOURCE-APPEND-COMMENT-LINE
      SOURCE-END @ SOURCE-I !
   repeat
   SOURCE-LEN @ ;

\ Line scanner: streams a file through a caller-owned line buffer, handing each
\ line to the caller's quotation. SOURCE-LS:FILE-LINES is the whole interface;
\ every step behind it is private.
\
\ CLOSE-FD, CLOSE-THROW, FILL-BUF and EMIT-LINE keep a distinguishing tail
\ because a package member shadows a global for every member defined after it:
\ members named CLOSE, THROW, READ or EMIT would silently capture the core
\ words these bodies call.
package SOURCE-LS
4096 constant READ-CAP
create READ-BUF READ-CAP allot
variable FD
variable RD
variable LEN
variable LINE#

: CLOSE-FD ( -- )
   FD @ dup 0 >= if close else drop then
   -1 FD ! ;

: CLOSE-THROW ( n -- )
   CLOSE-FD
   throw ;

: FILL-BUF ( -- n )
   FD @ READ-BUF READ-CAP read RD !
   RD @ 0 < if E-FS-IO CLOSE-THROW then
   RD @ READ-CAP > if E-FS-IO CLOSE-THROW then
   RD @ ;

: TRIM-CR ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0 > if
      a u 1 - + c@ SOURCE-CR = if a u 1 - exit then
   then
   a u ;

: APPEND ( n ptr u8 n -- ) {: c:n line:ptr cap:n :}
   LEN @ cap >= if E-FS-CAPACITY CLOSE-THROW then
   c line LEN @ + c!
   LEN @ 1+ LEN ! ;

\ typed-local-lint: allow-bare-local - q carries a quotation effect, which the
\ local annotation form cannot express.
: EMIT-LINE ( ptr u8 [ ptr u8 n n -- ] -- ) {: line:ptr q :}
   LINE# @ 1+ LINE# !
   line LEN @ TRIM-CR LINE# @ q execute
   0 LEN ! ;

\ typed-local-lint: allow-bare-local - q carries a quotation effect, which the
\ local annotation form cannot express.
: BYTE ( n ptr u8 n [ ptr u8 n n -- ] -- ) {: c:n line:ptr cap:n q :}
   c SOURCE-LF = if line q EMIT-LINE exit then
   c line cap APPEND ;

\ The byte index rides the return stack (`?do` / `i`), not the data stack: a
\ counter left under the arguments would join the row that `q` is executed on,
\ so every caller would then have to pass a quotation carrying that extra cell.
\ typed-local-lint: allow-bare-local - q carries a quotation effect, which the
\ local annotation form cannot express.
: CHUNK ( n ptr u8 n [ ptr u8 n n -- ] -- ) {: got:n line:ptr cap:n q :}
   got 0 ?do
      READ-BUF i + c@ line cap q BYTE
   loop ;

: OPEN ( ptr u8 n -- )
   -1 FD !
   FS-PATHZ open-rd FD !
   FD @ 0 < if E-FS-OPEN throw then ;

\ typed-local-lint: allow-bare-local - q carries a quotation effect, which the
\ local annotation form cannot express.
: DRAIN ( ptr u8 n [ ptr u8 n n -- ] -- ) {: line:ptr cap:n q :}
   cap 0 <= if E-FS-CAPACITY CLOSE-THROW then
   0 LEN !
   0 LINE# !
   begin FILL-BUF dup 0 > while
      line cap q CHUNK
   repeat drop
   LEN @ 0 > if line q EMIT-LINE then
   CLOSE-FD ;

public

\ typed-local-lint: allow-bare-local - q carries a quotation effect, which the
\ local annotation form cannot express.
: FILE-LINES ( ptr u8 n ptr u8 n [ ptr u8 n n -- ] -- ) {: path:ptr pathu:n line:ptr cap:n q :}
   path pathu OPEN
   line cap q DRAIN ;

;package
