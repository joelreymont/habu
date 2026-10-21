\ source-test.f - focused tests for checked source materialization helpers.
\ Run: bin/hb --load src/lib/source-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/source.f

package SOURCE

2048 constant ST-CAP
92 constant ST-BACKSLASH

variable ST-APPEND-LEN
variable ST-QP-U

create ST-BUF ST-CAP allot
create ST-QP-BUF ST-CAP allot

: ST-READ-STDIN-ALL ( ptr u8 n -- n )
   >LEN READ-STDIN-ALL LEN>N ;

: TEST-READ-STDIN-ALL ( -- )
   ST-BUF ST-CAP ST-READ-STDIN-ALL 0 T= ;

: TEST-SOURCE-APPEND-NEG-LEN ( -- )
   -1 ST-APPEND-LEN !
   s" x" >LEN ST-BUF 4 >LEN ST-APPEND-LEN SOURCE-APPEND-BYTES ;

: TEST-SOURCE-APPEND-HIGH-LEN ( -- )
   5 ST-APPEND-LEN !
   s" x" >LEN ST-BUF 4 >LEN ST-APPEND-LEN SOURCE-APPEND-BYTES ;

: ST-QP$ ( -- ptr u8 n )
   ST-QP-BUF ST-QP-U @ LEN>N ;

: ST-QPATH ( ptr u8 n -- ) {: a:ptr u:n :}
   0 >LEN ST-QP-U !
   a u >LEN ST-QP-BUF ST-CAP >LEN ST-QP-U SOURCE-APPEND-QPATH ;

: ST-QPATH-WANT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" s" SB-APPEND
   34 SB-APPEND-C
   32 SB-APPEND-C
   a u SB-APPEND
   34 SB-APPEND-C
   SB$ ;

: ST-QP-UNSAFE! ( n -- ) {: c:n :}
   SB-RESET
   s" a" SB-APPEND
   c SB-APPEND-C
   s" b.f" SB-APPEND
   SB$ ST-QPATH ;

: TEST-QPATH-SAFE ( -- )
   s" src/lib/foo.f" ST-QPATH
   ST-QP$ s" src/lib/foo.f" ST-QPATH-WANT$ T$= ;

: TEST-QPATH-SPACE ( -- )
   s" a dir/b.f" ST-QPATH
   ST-QP$ s" a dir/b.f" ST-QPATH-WANT$ T$= ;

: TEST-QPATH-DQ ( -- )
   [: 34 ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: TEST-QPATH-BS ( -- )
   [: ST-BACKSLASH ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: TEST-QPATH-LF ( -- )
   [: 10 ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: TEST-QPATH-CR ( -- )
   [: 13 ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: TEST-QPATH-NUL ( -- )
   [: 0 ST-QP-UNSAFE! ;] E-FS-PATH-UNSAFE TTHROWSQ ;

: ST-MAIN ( -- )
   T-RESET
   TEST-READ-STDIN-ALL
   TEST-QPATH-SAFE
   TEST-QPATH-SPACE
   TEST-QPATH-DQ
   TEST-QPATH-BS
   TEST-QPATH-LF
   TEST-QPATH-CR
   TEST-QPATH-NUL
   [: TEST-SOURCE-APPEND-NEG-LEN ;] E-FS-CAPACITY TTHROWSQ
   [: TEST-SOURCE-APPEND-HIGH-LEN ;] E-FS-CAPACITY TTHROWSQ
   T-REPORT
   s" source-test: ok" type cr ;

ST-MAIN

;package
