\ lower-txn-protection.f - active lowering-transaction span regressions.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package LOWER-TXN-PROTECTION-TEST

$800 constant CAP
10000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
variable OUT-U

: LINE ( ptr u8 n -- )
   SB-APPEND
   $A SB-APPEND-C ;

: PRELUDE ( -- )
   s" require lib/ffi-abi.f" LINE
   s" package DTP" LINE
   s" variable SEEN" LINE
   s" SUMTYPE res 2" LINE
   s"   VARIANT ok a ;VARIANT" LINE
   s"   VARIANT err b ;VARIANT" LINE
   s" ;SUMTYPE" LINE
   s" 1 LAYOUT-BUFFER MEM res<n,n>" LINE ;

: PROBE-HEAD ( -- )
   s" TRUSTED: STATE ( -- n n )" LINE
   s"   data-base TXN-BLOB-A-CELL + @" LINE
   s"   data-base TXN-BLOB-CAP-CELL + @ ;" LINE
   s" TRUSTED: >PTR ( n -- ptr u8 ) ;" LINE
   s" TRUSTED: N-PTR ( n -- ptr n ) ;" LINE
   s" TRUSTED: CALL-FOREIGN ( -- )" LINE
   s"   FFI:ARGS FFI:REG-LENS 1 0 ffi-call-bounded drop ;" LINE
   s" : PROBE ( -- )" LINE
   s"   STATE {: raw:n cap:n :}" LINE
   s"   raw 0= if exit then" LINE
   s"   raw >PTR {: a:ptr :}" LINE
   S\"   s\" DTP-ARMED\" type cr" LINE ;

: PROBE-TAIL ( -- )
   s"   -1 SEEN ! ; immediate" LINE
   s" : CLEAN? ( -- bool )" LINE
   s"   STATE {: a:n cap:n :}" LINE
   s"   a 0= cap 0= and ;" LINE
   s" public" LINE
   \ PROBE is a signature-carrying immediate, so a checked RUN would reject rc
   \ 70 (p5 wrong-certificate reject) BEFORE the engine-level protection this
   \ fixture proves. Declaring it an effect-neutral immediate (parse-imm 0,
   \ payload-free GRID: class) keeps RUN checked and the compile-time attack +
   \ seal trap (rc 83) exercised: PROBE's ( -- ) certificate is true of its
   \ empty runtime step; the compile-time attack is exactly what the
   \ transaction seal exists to catch.
   S\" s\" PROBE\" 0 parse-imm" LINE
   s" : RUN ( -- ) PROBE 37 construct res ok 0 MEM ! ;" LINE
   s" : CHECK ( -- )" LINE
   s"   SEEN @ 0= if 1 throw then" LINE
   s"   CLEAN? 0= if 1 throw then ;" LINE
   s" private" LINE
   s" ;package" LINE
   s" DTP:CHECK" LINE
   s" DTP:RUN" LINE ;

: SOURCE$ ( ptr u8 n -- ptr u8 n ) {: body:ptr bodyu:n :}
   SB-RESET
   PRELUDE
   PROBE-HEAD
   body bodyu LINE
   PROBE-TAIL
   SB$ ;

: CAPTURE ( ptr u8 n -- len len outcome ) {: src:ptr srcu:n :}
   src srcu OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN ;

: EXPECT ( ptr u8 n n -- ) {: src:ptr srcu:n want:n :}
   src srcu CAPTURE want T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N OUT-U !
   OUT OUT-U @ s" DTP-ARMED" CONTAINS? TTRUE ;

: REJECTS ( ptr u8 n -- )
   SOURCE$ ENGINE-ERROR:SEAL-VIOLATION EXPECT ;

: ACCEPTS ( ptr u8 n -- )
   SOURCE$ 0 EXPECT ;

: TEST-READ ( -- )
   s" exact dynamic lower neighbor remains readable" T-LABEL
   s"   -1 a 16 - 16 read drop" ACCEPTS
   s" dynamic lower crossing is rejected" T-LABEL
   s"   -1 a 8 - 16 read drop" REJECTS
   s" dynamic interior is rejected" T-LABEL
   s"   -1 a 1 read drop" REJECTS
   s" dynamic upper crossing is rejected" T-LABEL
   s"   -1 a cap + 8 - 16 read drop" REJECTS
   s" exact dynamic upper neighbor remains readable" T-LABEL
   s"   -1 a cap + 16 read drop" ACCEPTS ;

: TEST-SINKS ( -- )
   s" scalar stores cannot enter the active transaction" T-LABEL
   s"   0 a FFI:>CELL N-PTR !" REJECTS
   s" MAP_FIXED cannot replace the active transaction" T-LABEL
   s"   a FFI:>CELL 16 3 $1012 -1 0 mmap drop" REJECTS
   s" FFI writable extent cannot enter the active transaction" T-LABEL
   s"   a 16 0 FFI:WRITABLE! CALL-FOREIGN" REJECTS
   s" snap-rebase cannot straddle the active transaction" T-LABEL
   s"   a 8 - FFI:>CELL a cap + 8 + FFI:>CELL 0 0 0 0 snap-rebase" REJECTS ;

public

: RUN ( -- )
   T-RESET
   TEST-READ
   TEST-SINKS
   T-REPORT
   s" lower-txn-protection: ok" type cr ;

;package

LOWER-TXN-PROTECTION-TEST:RUN
