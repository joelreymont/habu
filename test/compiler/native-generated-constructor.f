\ native-generated-constructor.f - generated SUM/ENUM bodies use construct.
\
\ A payload constructor is the smallest shape whose raw runtime cells differ
\ from its one logical output bundle. The declaration drives the production
\ generator and native compiler at genuine top level, where RESULT:OK-shaped
\ definitions can collide with an older bare binding.

require src/compiler/native/compiler.f
require lib/json-write.f
require lib/test.f
require lib/errors.f
require test/compiler/native-eval-fixture.f
require tools/codegen-tail-probe.f

s" ENUM nctorresult 0 VARIANT ok FIELD value n ;VARIANT VARIANT err FIELD error n ;VARIANT ;ENUM"
INCLUDE-EVALUATE

package NCTOR-WIDE-TEST
public

STRUCTURE nctorstructure 0
   FIELD x n
   FIELD y n
;STRUCTURE

PRODUCT nctorproduct 0
   FIELD x n
   FIELD y n
;PRODUCT

\ The two writer fields each occupy four cells. The inner constructor takes
\ 33 cells; nesting it with one more field gives the outer constructor 34.
STRUCTURE nctorinner 0
   FIELD first JSON-WRITE:writer
   FIELD second JSON-WRITE:writer
   FIELD n01 n FIELD n02 n FIELD n03 n FIELD n04 n FIELD n05 n
   FIELD n06 n FIELD n07 n FIELD n08 n FIELD n09 n FIELD n10 n
   FIELD n11 n FIELD n12 n FIELD n13 n FIELD n14 n FIELD n15 n
   FIELD n16 n FIELD n17 n FIELD n18 n FIELD n19 n FIELD n20 n
   FIELD n21 n FIELD n22 n FIELD n23 n FIELD n24 n FIELD n25 n
;STRUCTURE

STRUCTURE nctorouter 0
   FIELD inner nctorinner
   FIELD tail n
;STRUCTURE

4 BUFFER: NCTOR-OUT1
4 BUFFER: NCTOR-OUT2
TYPED-VARIABLE NCTOR-BUF1 n
TYPED-VARIABLE NCTOR-BUF2 n

: NCTOR-KEEP-OUTER ( nctorouter -- nctorouter )
   {: value :}
   value ;

: NCTOR-FORWARD-OUTER ( nctorouter -- nctorouter )
   NCTOR-KEEP-OUTER ;

: NCTOR-ID32 ( n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n -- n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n ) ;

: NCTOR-TAIL32 ( n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n -- n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n n )
   NCTOR-ID32 ;

: NCTOR-ID20 ( n n n n n n n n n n n n n n n n n n n n -- n n n n n n n n n n n n n n n n n n n n ) ;

: NCTOR-LOOP20 ( n n n n n n n n n n n n n n n n n n n n n -- n n n n n n n n n n n n n n n n n n n n )
   {: a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 k :}
   a20 a19 a18 a17 a16 a15 a14 a13 a12 a11
   a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
   k 0 ?do NCTOR-ID20 1+ loop ;

: NCTOR-ID12 ( n n n n n n n n n n n n -- n n n n n n n n n n n n ) ;
: NCTOR-ID1 ( n -- n ) ;

: NCTOR-LOOP12 ( n n n n n n n n n n n n n -- n n n n n n n n n n n n )
   {: a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 k :}
   a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
   k 0 ?do NCTOR-ID12 1+ loop ;

: NCTOR-SPILL12 ( n n n n n n n n n n n n n bool -- n n n n n n n n n n n n )
   {: a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 k f :}
   0 NCTOR-ID1 drop
   k 0 ?do
      f if
         a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
      else
         a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
      then
      NCTOR-ID12 1+ drop
      drop drop drop drop drop drop drop drop drop drop drop
   loop
   f if
      a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
   else
      a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
   then NCTOR-ID12 ;

private

: NCTOR-STRUCTURE-ROUNDTRIP ( n n -- n n )
   NCTOR--WIDE--TEST-NCTORSTRUCTURE:MAKE
   NCTOR--WIDE--TEST-NCTORSTRUCTURE:UNMAKE ;

: NCTOR-PRODUCT-ROUNDTRIP ( n n -- n n )
   NCTOR--WIDE--TEST-NCTORPRODUCT:MAKE
   NCTOR--WIDE--TEST-NCTORPRODUCT:UNMAKE ;

: NCTOR-WIDE-ROUNDTRIPS? ( -- bool )
   3 4 NCTOR-STRUCTURE-ROUNDTRIP 4 = swap 3 = and
   5 6 NCTOR-PRODUCT-ROUNDTRIP 6 = swap 5 = and
   and ;

: NCTOR-RECORD-ROUNDTRIP ( -- )
   T-RESET
   NCTOR-OUT1 101 102 NCTOR-BUF1 JSON--WRITE-WRITER:MAKE
   NCTOR-OUT2 201 202 NCTOR-BUF2 JSON--WRITE-WRITER:MAKE
   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
   NCTOR--WIDE--TEST-NCTORINNER:MAKE
   26 NCTOR--WIDE--TEST-NCTOROUTER:MAKE NCTOR-FORWARD-OUTER
   NCTOR--WIDE--TEST-NCTOROUTER:UNMAKE 26 T=
   NCTOR--WIDE--TEST-NCTORINNER:UNMAKE
   25 T= 24 T= 23 T= 22 T= 21 T= 20 T= 19 T= 18 T= 17 T= 16 T=
   15 T= 14 T= 13 T= 12 T= 11 T= 10 T= 9 T= 8 T= 7 T= 6 T=
   5 T= 4 T= 3 T= 2 T= 1 T=
   JSON--WRITE-WRITER:UNMAKE NCTOR-BUF2 = TTRUE 202 T= 201 T= NCTOR-OUT2 = TTRUE
   JSON--WRITE-WRITER:UNMAKE NCTOR-BUF1 = TTRUE 102 T= 101 T= NCTOR-OUT1 = TTRUE
   tier@ 1 = if
      s" a 34-cell record forwards through an ordinary call" T-LABEL
      s" NCTOR-WIDE-TEST:NCTOR-FORWARD-OUTER" NTAILPROBE:CALLS 1 T=
      s" NCTOR-WIDE-TEST:NCTOR-FORWARD-OUTER" NTAILPROBE:TAIL-BRANCH? TFALSE
      s" a legal 32-cell forwarding call remains a tail branch" T-LABEL
      s" NCTOR-WIDE-TEST:NCTOR-TAIL32" NTAILPROBE:TAIL-BRANCH? TTRUE
      s" NCTOR-WIDE-TEST:NCTOR-TAIL32" NTAILPROBE:CALLS 0 T=
   then
   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
   17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 NCTOR-TAIL32
   32 T= 31 T= 30 T= 29 T= 28 T= 27 T= 26 T= 25 T=
   24 T= 23 T= 22 T= 21 T= 20 T= 19 T= 18 T= 17 T=
   16 T= 15 T= 14 T= 13 T= 12 T= 11 T= 10 T= 9 T=
   8 T= 7 T= 6 T= 5 T= 4 T= 3 T= 2 T= 1 T=
   T-REPORT ;

: NCTOR-SPILL-ROUNDTRIP ( -- )
   T-RESET
   s" twenty loop-carried values keep their order through a wide call" T-LABEL
   4 7 10 13 16 19 22 25 28 31 34 37 40 43 46 49 52 55 58 61
   3 NCTOR-LOOP20
   7 T= 7 T= 10 T= 13 T= 16 T= 19 T= 22 T= 25 T= 28 T= 31 T=
   34 T= 37 T= 40 T= 43 T= 46 T= 49 T= 52 T= 55 T= 58 T= 61 T=
   s" spilled values survive a branch and back edge around a wide call" T-LABEL
   4 7 10 13 16 19 22 25 28 31 34 37 3 true NCTOR-SPILL12
   4 T= 7 T= 10 T= 13 T= 16 T= 19 T= 22 T= 25 T= 28 T= 31 T= 34 T= 37 T=
   4 7 10 13 16 19 22 25 28 31 34 37 3 false NCTOR-SPILL12
   37 T= 34 T= 31 T= 28 T= 25 T= 22 T= 19 T= 16 T= 13 T= 10 T= 7 T= 4 T=
   s" twelve values stay in registers across a wide call and back edge" T-LABEL
   4 7 10 13 16 19 22 25 28 31 34 37 3 NCTOR-LOOP12
   7 T= 7 T= 10 T= 13 T= 16 T= 19 T= 22 T= 25 T= 28 T= 31 T= 34 T= 37 T=
   T-REPORT ;

: NCTOR-ENTRY-POOL ( -- )
   tier@ 1 = HB-TARGET-MACOS? and if
      T-RESET
      s" twenty-four consumed entry cells compile and sum correctly" T-LABEL
      s" : NCTOR-SUM24 ( n n n n n n n n n n n n n n n n n n n n n n n n -- n ) {: a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 :} a1 a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + ; : NCTOR-CHECK24 ( -- ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 NCTOR-SUM24 300 T= ; NCTOR-CHECK24"
      NATIVE-EVAL:DEFINE-RC 0 T=
      s" twenty-five simultaneously consumed entry cells refuse at the register pool" T-LABEL
      s" : NCTOR-SUM25 ( n n n n n n n n n n n n n n n n n n n n n n n n n -- n ) {: a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 :} a1 a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 + ;"
      NATIVE-EVAL:DEFINE-RC E-A64RA-POOL T=
      T-REPORT
   then ;

: NCTOR-REPORT ( -- )
   NCTOR-WIDE-ROUNDTRIPS? 0= if
      s" native-generated-constructor: wide round-trip failed" 1 die
   then
   s" test: ok" type cr ;

NCTOR-REPORT
NCTOR-RECORD-ROUNDTRIP
NCTOR-SPILL-ROUNDTRIP
NCTOR-ENTRY-POOL

;package
