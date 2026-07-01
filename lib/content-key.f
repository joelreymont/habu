\ content-key.f - manifest-hashed content cache keys.
\
\ Requires SHA256 words; native bin/hb already carries src/core/sha256.f.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, and lib/fs.f.

$40000 constant CK-CAP
$100000 constant CK-CACHE-CAP
FS-PATH-CAP 160 + constant CK-ROW-CAP
64 constant CK-HEX-LEN
$54 constant CK-TEXT-TAG
$46 constant CK-FILE-TAG
$44 constant CK-DIGEST-TAG
$41 constant CK-HEX-UP-A
$47 constant CK-HEX-UP-G
$61 constant CK-HEX-LOW-A
$67 constant CK-HEX-LOW-G

create CK-BUF CK-CAP allot
create CK-DG 40 allot
create CK-FILE-DG 40 allot
create CK-FILE-HEX 80 allot
create CK-CACHE-PATH-BUF FS-PATH-CAP allot
create CK-ROW-BUF CK-ROW-CAP allot

variable CK-U
variable CK-CACHE-BUF-A
variable CK-CACHE-U
variable CK-CACHE-PATH-U
variable CK-ROW-U
variable CK-PREFIX-U
variable CK-CACHE-LOADED

: CK-TRUE ( -- bool )
   0 0= ;

: CK-FALSE ( -- bool )
   CK-TRUE 0= ;

: CK-RESET ( -- )
   0 CK-U ! ;

: CK-CAP-CHECK ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   CK-U @ n + CK-CAP > if E-STR-CAPACITY throw then ;

: CK-U8+ ( n -- ) {: c:n :}
   1 CK-CAP-CHECK
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   c CK-BUF CK-U @ + c!
   CK-U @ 1+ CK-U ! ;

: CK-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u CK-CAP-CHECK
   a CK-BUF CK-U @ + u BYTE-COPY
   CK-U @ u + CK-U ! ;

: CK-FRAG+ ( n ptr u8 n -- ) {: tag:n a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   tag CK-U8+
   u CK-U8+
   a u CK-BYTES+ ;

: CK-TEXT+ ( ptr u8 n -- )
   CK-TEXT-TAG -rot CK-FRAG+ ;

: CK-DIGEST+ ( ptr u8 -- )
   CK-DIGEST-TAG CK-U8+
   32 CK-U8+
   32 CK-BYTES+ ;

: CK-CACHE-CLEAR! ( -- )
   0 CK-CACHE-PATH-U !
   0 CK-CACHE-LOADED ! ;

: CK-CACHE-BUF-FIELD ( -- ptr ptr u8 )
   CK-CACHE-BUF-A 0 ptr-field ;

: CK-CACHE-BUF@ ( -- ptr u8 )
   CK-CACHE-BUF-FIELD @ ;

: CK-CACHE-BUF! ( ptr u8 -- )
   CK-CACHE-BUF-FIELD ! ;

: CK-CACHE-BUF ( -- ptr u8 )
   CK-CACHE-BUF@ 0= if
      CK-CACHE-CAP MEM-ALLOC-BYTES drop CK-CACHE-BUF!
   then
   CK-CACHE-BUF@ ;

: CK-CACHE-PATH$ ( -- ptr u8 n )
   CK-CACHE-PATH-BUF CK-CACHE-PATH-U @ ;

: CK-CACHE-PATH? ( -- bool )
   CK-CACHE-PATH-U @ 0 > ;

: CK-CACHE-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a CK-CACHE-PATH-BUF u BYTE-COPY
   u CK-CACHE-PATH-U !
   0 CK-CACHE-LOADED ! ;

: CK-CACHE-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" content-key.cache" CK-CACHE-PATH-BUF JOIN-PATH CK-CACHE-PATH-U !
   0 CK-CACHE-LOADED ! ;

: CK-CACHE-AUTO? ( -- bool )
   CK-CACHE-PATH? if CK-TRUE exit then
   CK-FALSE ;

: CK-CACHE-LOAD? ( -- bool )
   CK-CACHE-AUTO? 0= if CK-FALSE exit then
   CK-CACHE-LOADED @ 0 <> if CK-TRUE exit then
   CK-CACHE-PATH$ FILE? 0= if CK-FALSE exit then
   CK-CACHE-PATH$ FILE-SIZE CK-CACHE-CAP > if CK-FALSE exit then
   CK-CACHE-PATH$ CK-CACHE-BUF CK-CACHE-CAP READ-ALL CK-CACHE-U !
   -1 CK-CACHE-LOADED !
   CK-TRUE ;

: CK-ROW-RESET ( -- )
   0 CK-ROW-U ! ;

: CK-ROW-CHECK ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   CK-ROW-U @ n + CK-ROW-CAP > if E-STR-CAPACITY throw then ;

: CK-ROW-C+ ( n -- ) {: c:n :}
   1 CK-ROW-CHECK
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   c CK-ROW-BUF CK-ROW-U @ + c!
   CK-ROW-U @ 1+ CK-ROW-U ! ;

: CK-ROW+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u CK-ROW-CHECK
   a CK-ROW-BUF CK-ROW-U @ + u BYTE-COPY
   CK-ROW-U @ u + CK-ROW-U ! ;

: CK-ROW-N+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + CK-ROW-C+ ;

: CK-ROW-FILE-PREFIX ( ptr u8 n n n n n n -- )
   {: a:ptr u:n sz:n mt:n mn:n ct:n cn:n :}
   CK-ROW-RESET
   a u CK-ROW+
   STR-TAB CK-ROW-C+
   sz CK-ROW-N+
   STR-TAB CK-ROW-C+
   mt CK-ROW-N+
   STR-TAB CK-ROW-C+
   mn CK-ROW-N+
   STR-TAB CK-ROW-C+
   ct CK-ROW-N+
   STR-TAB CK-ROW-C+
   cn CK-ROW-N+
   STR-TAB CK-ROW-C+
   CK-ROW-U @ CK-PREFIX-U ! ;

: CK-HEX-NIB ( n -- n ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and if c STR-ZERO - exit then
   c CK-HEX-LOW-A >= c CK-HEX-LOW-G < and if c 87 - exit then
   c CK-HEX-UP-A >= c CK-HEX-UP-G < and if c 55 - exit then
   E-STR-BOUNDS throw ;

: CK-HEX-BYTE@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ CK-HEX-NIB 4 lshift
   a 1 + c@ CK-HEX-NIB or ;

: CK-HEX>DIGEST ( ptr u8 -- ) {: a:ptr :}
   32 0 DO
      a i 2 * + CK-HEX-BYTE@ CK-FILE-DG i + c!
   LOOP ;

: CK-LINE-END ( n -- n ) {: off:n :}
   off begin dup CK-CACHE-U @ < while
      CK-CACHE-BUF over + c@ STR-LF = if exit then
      1+
   repeat ;

: CK-CACHE-LINE? ( n -- bool ) {: off:n :}
   off CK-LINE-END {: ed:n :}
   ed off - CK-PREFIX-U @ CK-HEX-LEN + <> if CK-FALSE exit then
   CK-CACHE-BUF off + CK-PREFIX-U @ CK-ROW-BUF CK-PREFIX-U @ STR= 0= if CK-FALSE exit then
   CK-CACHE-BUF off + CK-PREFIX-U @ + CK-HEX>DIGEST
   CK-TRUE ;

: CK-CACHE-FIND? ( -- bool )
   0 begin dup CK-CACHE-U @ < while
      dup CK-CACHE-LINE? if drop CK-TRUE exit then
      CK-LINE-END 1+
   repeat drop CK-FALSE ;

: CK-ROW-DIGEST+ ( -- )
   CK-FILE-DG CK-FILE-HEX SHA256>HEX
   CK-FILE-HEX CK-HEX-LEN CK-ROW+
   STR-LF CK-ROW-C+ ;

: CK-CACHE-APPEND ( -- )
   CK-CACHE-AUTO? if
      CK-ROW-DIGEST+
      CK-CACHE-PATH$ CK-ROW-BUF CK-ROW-U @ APPEND-FILE
      CK-CACHE-LOADED @ 0 <> if
         CK-ROW-BUF CK-ROW-U @ >LEN CK-CACHE-BUF CK-CACHE-CAP >LEN CK-CACHE-U BUF-APPEND-LEN
      then
   then ;

: CK-FILE-DIGEST! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CK-FILE-DG SHA256-FILE dup 0 <> if throw then drop ;

: CK-FILE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CK-FILE-TAG a u CK-FRAG+
   a u FILE-META {: sz:n mt:n mn:n ct:n cn:n :}
   a u sz mt mn ct cn CK-ROW-FILE-PREFIX
   CK-CACHE-LOAD? if
      CK-CACHE-FIND? if CK-FILE-DG CK-DIGEST+ exit then
   then
   a u CK-FILE-DIGEST!
   CK-FILE-DG CK-DIGEST+
   CK-CACHE-APPEND ;

: CK-FINAL ( ptr u8 -- ) {: dst:ptr :}
   CK-BUF CK-U @ dst SHA256 ;

: CK-FINAL-HEX ( ptr u8 -- ) {: hex:ptr :}
   CK-DG CK-FINAL
   CK-DG hex SHA256>HEX ;
