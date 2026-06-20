\ bundle-lib.f - concatenate stdlib modules and one driver source.

64 constant BL-USAGE-RC
66 constant BL-NOINPUT-RC
74 constant BL-IO-RC
76 constant BL-INTERNAL-RC

64 constant BL-MOD-MAX
1024 constant BL-PATH-CAP
8192 constant BL-IO-CAP
256 constant BL-STAT-CAP

1 constant BL-O-WRONLY
$200 constant BL-O-CREAT
$400 constant BL-O-TRUNC
420 constant BL-MODE-0644

$0A constant BL-LF
$2D constant BL-DASH
$30 constant BL-ZERO
$39 constant BL-NINE
$61 constant BL-A-LOW
$7A constant BL-Z-LOW
10 constant BL-DEC-BASE
4 constant BL-STAT-MODE-OFF
8 constant BL-BYTE-BITS
$F000 constant BL-S-IFMT
$8000 constant BL-S-IFREG

create BL-MOD-A BL-MOD-MAX cells allot
create BL-MOD-U BL-MOD-MAX cells allot
create BL-PATH BL-PATH-CAP allot
create BL-TMP BL-PATH-CAP allot
create BL-Z1 BL-PATH-CAP 1 + allot
create BL-Z2 BL-PATH-CAP 1 + allot
create BL-IO BL-IO-CAP allot
create BL-STAT BL-STAT-CAP allot

variable BL-NMOD
variable BL-OUT-A
variable BL-OUT-U
variable BL-OUT-SET
variable BL-SCRIPT-A
variable BL-SCRIPT-U
variable BL-PATH-LEN
variable BL-TMP-LEN
variable BL-OUT-FD
variable BL-IN-FD
variable BL-FILE-OUT
variable BL-WR-OFF
variable BL-WR-N
variable BL-RD-N

: BL-TRUE ( -- bool )
   0 0= ;

: BL-FALSE ( -- bool )
   BL-TRUE 0= ;

: BL-DIE ( ptr u8 n n -- )
   die ;

: BL-USAGE ( -- )
   s" usage: bin/hb tools/bundle-lib.f [-o out.f] module... -- script.f" BL-USAGE-RC BL-DIE ;

: BL-BYTE-COPY ( ptr u8 ptr u8 n -- ) {: src:ptr dst:ptr u :}
   0 begin dup u < while
      dup src + c@ over dst + c!
      1+
   repeat drop ;

: BL-STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if BL-FALSE exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop BL-FALSE exit then
      1+
   repeat drop BL-TRUE ;

: BL-BUF+ ( ptr u8 n ptr u8 n ptr a -- ) {: src:ptr u dst:ptr cap lenp:ptr :}
   u 0 < if s" bundle-lib: negative length" BL-INTERNAL-RC BL-DIE then
   lenp @ u + cap > if s" bundle-lib: path too long" BL-INTERNAL-RC BL-DIE then
   src dst lenp @ + u BL-BYTE-COPY
   lenp @ u + lenp ! ;

: BL-BUF-C+ ( n ptr u8 n ptr a -- ) {: c dst:ptr cap lenp:ptr :}
   lenp @ 1 + cap > if s" bundle-lib: path too long" BL-INTERNAL-RC BL-DIE then
   c dst lenp @ + c!
   lenp @ 1+ lenp ! ;

: BL-PATH-RESET ( -- )
   0 BL-PATH-LEN ! ;

: BL-PATH+ ( ptr u8 n -- )
   BL-PATH BL-PATH-CAP BL-PATH-LEN BL-BUF+ ;

: BL-PATH$ ( -- ptr u8 n )
   BL-PATH BL-PATH-LEN @ ;

: BL-TMP-RESET ( -- )
   0 BL-TMP-LEN ! ;

: BL-TMP+ ( ptr u8 n -- )
   BL-TMP BL-PATH-CAP BL-TMP-LEN BL-BUF+ ;

: BL-TMP-C+ ( n -- )
   BL-TMP BL-PATH-CAP BL-TMP-LEN BL-BUF-C+ ;

: BL-TMP$ ( -- ptr u8 n )
   BL-TMP BL-TMP-LEN @ ;

: BL-APPEND-U ( n -- )
   dup 0 < if s" bundle-lib: negative number" BL-INTERNAL-RC BL-DIE then
   dup BL-DEC-BASE >= if dup BL-DEC-BASE / recurse then
   BL-DEC-BASE mod BL-ZERO + BL-TMP-C+ ;

: BL-ZCOPY ( ptr u8 n ptr u8 -- ptr u8 ) {: a:ptr u dst:ptr :}
   u 0 < if s" bundle-lib: path too long" BL-INTERNAL-RC BL-DIE then
   u BL-PATH-CAP > if s" bundle-lib: path too long" BL-INTERNAL-RC BL-DIE then
   a dst u BL-BYTE-COPY
   0 dst u + c!
   dst ;

: BL-Z1$ ( ptr u8 n -- ptr u8 )
   BL-Z1 BL-ZCOPY ;

: BL-Z2$ ( ptr u8 n -- ptr u8 )
   BL-Z2 BL-ZCOPY ;

: BL-U16@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ a 1 + c@ BL-BYTE-BITS lshift or ;

: BL-TRY-STAT-MODE ( ptr u8 n -- n ) {: a:ptr u :}
   a u BL-Z1$ BL-STAT stat64 0 < if -1 exit then
   BL-STAT BL-STAT-MODE-OFF + BL-U16@ ;

: BL-FILE? ( ptr u8 n -- bool )
   BL-TRY-STAT-MODE dup 0 < if
      drop BL-FALSE
   else
      BL-S-IFMT and BL-S-IFREG =
   then ;

: BL-LOWER? ( n -- bool )
   dup BL-A-LOW 1 - > swap BL-Z-LOW 1 + < and ;

: BL-DIGIT? ( n -- bool )
   dup BL-ZERO 1 - > swap BL-NINE 1 + < and ;

: BL-MOD-CHAR? ( n -- bool )
   dup BL-LOWER? if drop BL-TRUE exit then
   dup BL-DIGIT? if drop BL-TRUE exit then
   BL-DASH = ;

: BL-MODULE? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0= if BL-FALSE exit then
   a c@ BL-LOWER? 0= if BL-FALSE exit then
   1 begin dup u < while
      dup a + c@ BL-MOD-CHAR? 0= if drop BL-FALSE exit then
      1+
   repeat drop BL-TRUE ;

: BL-DASH? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 1 > if a c@ BL-DASH = else BL-FALSE then ;

: BL-MOD-A! ( ptr u8 n -- ) {: a:ptr idx :}
   a idx cells BL-MOD-A + ! ;

: BL-MOD-U! ( n n -- ) {: u idx :}
   u idx cells BL-MOD-U + ! ;

: BL-MOD-A@ ( n -- ptr u8 ) {: idx :}
   idx cells BL-MOD-A + @ ;

: BL-MOD-U@ ( n -- n ) {: idx :}
   idx cells BL-MOD-U + @ ;

: BL-MOD$ ( n -- ptr u8 n ) {: idx :}
   idx BL-MOD-A@ idx BL-MOD-U@ ;

: BL-MOD+ ( ptr u8 n -- ) {: a:ptr u :}
   a u BL-MODULE? 0= if s" bundle-lib: invalid module" BL-USAGE-RC BL-DIE then
   BL-NMOD @ BL-MOD-MAX >= if s" bundle-lib: too many modules" BL-USAGE-RC BL-DIE then
   a BL-NMOD @ BL-MOD-A!
   u BL-NMOD @ BL-MOD-U!
   BL-NMOD @ 1+ BL-NMOD ! ;

: BL-OUT! ( ptr u8 n -- ) {: a:ptr u :}
   a BL-OUT-A !
   u BL-OUT-U !
   -1 BL-OUT-SET ! ;

: BL-SCRIPT! ( ptr u8 n -- ) {: a:ptr u :}
   a BL-SCRIPT-A !
   u BL-SCRIPT-U ! ;

: BL-RESET ( -- )
   0 BL-NMOD !
   0 BL-OUT-SET !
   0 BL-OUT-A !
   0 BL-OUT-U !
   0 BL-SCRIPT-A !
   0 BL-SCRIPT-U !
   -1 BL-OUT-FD !
   -1 BL-IN-FD !
   0 BL-FILE-OUT ! ;

: BL-PARSE-SCRIPT ( n -- ) {: idx :}
   idx SCRIPT-ARGC 1 - <> if BL-USAGE then
   BL-NMOD @ 0= if BL-USAGE then
   idx SCRIPT-ARGV$ BL-SCRIPT! ;

: BL-REQUIRE-ARG-INDEX ( n -- n ) {: idx :}
   idx SCRIPT-ARGC >= if BL-USAGE then
   idx ;

: BL-REQUIRE-NOT-DASH ( ptr u8 n -- )
   BL-DASH? if BL-USAGE then ;

: BL-PARSE ( -- )
   BL-RESET
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ s" --" BL-STR= if
         1+ BL-PARSE-SCRIPT exit
      then
      dup SCRIPT-ARGV$ s" -o" BL-STR= if
         1+ BL-REQUIRE-ARG-INDEX
         dup SCRIPT-ARGV$ BL-OUT!
         1+
      else
         dup SCRIPT-ARGV$ BL-REQUIRE-NOT-DASH
         dup SCRIPT-ARGV$ BL-MOD+
         1+
      then
   repeat drop BL-USAGE ;

: BL-MODULE-PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   BL-PATH-RESET
   s" lib/" BL-PATH+
   a u BL-PATH+
   s" .f" BL-PATH+
   BL-PATH$ ;

: BL-CLOSE-OUT ( -- )
   BL-FILE-OUT @ if
      BL-OUT-FD @ dup 0 >= if close else drop then
      -1 BL-OUT-FD !
      0 BL-FILE-OUT !
   then ;

: BL-CLOSE-IN ( -- )
   BL-IN-FD @ dup 0 >= if close else drop then
   -1 BL-IN-FD ! ;

: BL-FAIL-READ ( -- )
   BL-CLOSE-IN
   BL-CLOSE-OUT
   s" bundle-lib: read failed" BL-IO-RC BL-DIE ;

: BL-FAIL-WRITE ( -- )
   BL-CLOSE-IN
   BL-CLOSE-OUT
   s" bundle-lib: write failed" BL-IO-RC BL-DIE ;

: BL-WRITE ( ptr u8 n -- ) {: a:ptr u :}
   u 0= if exit then
   0 BL-WR-OFF !
   begin BL-WR-OFF @ u < while
      BL-OUT-FD @ a BL-WR-OFF @ + u BL-WR-OFF @ - write BL-WR-N !
      BL-WR-N @ 0 <= if BL-FAIL-WRITE then
      BL-WR-N @ u BL-WR-OFF @ - > if BL-FAIL-WRITE then
      BL-WR-OFF @ BL-WR-N @ + BL-WR-OFF !
   repeat ;

: BL-WRITE-LF ( -- )
   BL-IO BL-LF swap c!
   BL-IO 1 BL-WRITE ;

: BL-COPY-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u BL-Z1$ open-rd BL-IN-FD !
   BL-IN-FD @ 0 < if BL-FAIL-READ then
   begin
      BL-IN-FD @ BL-IO BL-IO-CAP read BL-RD-N !
      BL-RD-N @ 0 < if BL-FAIL-READ then
      BL-RD-N @ BL-IO-CAP > if BL-FAIL-READ then
      BL-RD-N @ 0= if BL-CLOSE-IN exit then
      BL-IO BL-RD-N @ BL-WRITE
   again ;

: BL-EMIT-BEGIN ( ptr u8 n -- ) {: a:ptr u :}
   s" \ BEGIN " BL-WRITE
   a u BL-WRITE
   BL-WRITE-LF ;

: BL-EMIT-END ( ptr u8 n -- ) {: a:ptr u :}
   BL-WRITE-LF
   s" \ END " BL-WRITE
   a u BL-WRITE
   BL-WRITE-LF ;

: BL-EMIT-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u BL-EMIT-BEGIN
   a u BL-COPY-FILE
   a u BL-EMIT-END ;

: BL-VERIFY-MODULE ( n -- ) {: idx :}
   idx BL-MOD$ BL-MODULE-PATH BL-FILE? 0= if
      s" bundle-lib: missing module" BL-NOINPUT-RC BL-DIE
   then ;

: BL-VERIFY ( -- )
   BL-SCRIPT-A @ BL-SCRIPT-U @ BL-FILE? 0= if
      s" bundle-lib: missing script" BL-NOINPUT-RC BL-DIE
   then
   0 begin dup BL-NMOD @ < while
      dup BL-VERIFY-MODULE
      1+
   repeat drop ;

: BL-TEMP-PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   BL-TMP-RESET
   a u BL-TMP+
   s" .bundle-lib." BL-TMP+
   mono-ns BL-APPEND-U
   s" .tmp" BL-TMP+
   BL-TMP$ ;

: BL-RENAME ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu BL-Z1$ dst dstu BL-Z2$ rename 0 < if
      s" bundle-lib: rename failed" BL-IO-RC BL-DIE
   then ;

: BL-OPEN-OUTPUT ( -- )
   BL-OUT-SET @ if
      BL-OUT-A @ BL-OUT-U @ BL-TEMP-PATH BL-Z1$
      BL-O-WRONLY BL-O-CREAT or BL-O-TRUNC or BL-MODE-0644 open BL-OUT-FD !
      BL-OUT-FD @ 0 < if s" bundle-lib: cannot open output" BL-IO-RC BL-DIE then
      -1 BL-FILE-OUT !
   else
      1 BL-OUT-FD !
      0 BL-FILE-OUT !
   then ;

: BL-FINISH-OUTPUT ( -- )
   BL-FILE-OUT @ if
      BL-CLOSE-OUT
      BL-TMP BL-TMP-LEN @ BL-OUT-A @ BL-OUT-U @ BL-RENAME
   then ;

: BL-EMIT-BUNDLE ( -- )
   BL-OPEN-OUTPUT
   s" \ generated by tools/bundle-lib.f" BL-WRITE
   BL-WRITE-LF
   0 begin dup BL-NMOD @ < while
      dup BL-MOD$ BL-MODULE-PATH BL-EMIT-FILE
      1+
   repeat drop
   BL-SCRIPT-A @ BL-SCRIPT-U @ BL-EMIT-FILE
   BL-FINISH-OUTPUT ;

: BUNDLE-LIB-MAIN ( -- )
   BL-PARSE
   BL-VERIFY
   BL-EMIT-BUNDLE ;

BUNDLE-LIB-MAIN
