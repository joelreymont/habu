\ Exact decimal conversion through its public API and the JSON reader.
\ Run: bin/hb --load src/lib/decimal-test.f

require src/lib/string.f
require src/lib/decimal.f
require src/lib/json-read.f
require src/lib/memory.f
require src/lib/test.f

package DECIMAL-TEST
private

TYPED-VARIABLE READER-SPACE JR:cursor
create DECODE-BUF 64 allot
1048577 constant LONG-ZEROS
create LONG-SRC 1048600 allot

: SOME= ( option<i64> i64 -- )
   {: want:i64 :}
   MATCH option
      none OF false TTRUE ENDOF
      some OF want T= ENDOF
   ;MATCH ;

: NONE? ( option<i64> -- )
   MATCH option
      none OF true TTRUE ENDOF
      some OF drop false TTRUE ENDOF
   ;MATCH ;

: EXACT= ( ptr u8 n i64 -- )
   {: a:ptr u:n want:i64 :}
   a u DECIMAL:EXACT want SOME= ;

: EXACT-NONE ( ptr u8 n -- )
   DECIMAL:EXACT NONE? ;

: INTEGER= ( ptr u8 n i64 -- )
   {: a:ptr u:n want:i64 :}
   a u DECIMAL:INTEGER want SOME= ;

: INTEGER-NONE ( ptr u8 n -- )
   DECIMAL:INTEGER NONE? ;

: PRIORITY ( ptr u8 n -- option<i64> )
   {: a:ptr u:n :}
   a u DECIMAL:INTEGER MATCH option
      none OF a u DECIMAL:EXACT ENDOF
      some OF OPTION:SOME ENDOF
   ;MATCH ;

: STRING= ( ptr u8 n i64 -- )
   {: a:ptr u:n want:i64 :}
   READER-SPACE a u JR:INIT
   JR:NEXT JR:T-STR T=
   DECODE-BUF 64 JR:STR {: size:n :}
   DECODE-BUF size PRIORITY want SOME=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: BAD-DECIMAL-LEN ( -- )
   s" 1" drop -1 DECIMAL:EXACT drop ;

: BAD-DECIMAL-PTR ( -- )
   NULL-PTR 1 DECIMAL:EXACT drop ;

: LONG-CANCELLATION ( -- )
   48 LONG-SRC c!
   46 LONG-SRC 1+ c!
   LONG-ZEROS 0 ?do 48 LONG-SRC 2 i + + c! loop
   s" 1e1048578" {: suffix:ptr size:n :}
   suffix LONG-SRC 2 LONG-ZEROS + + size BYTE-COPY
   LONG-SRC 2 LONG-ZEROS + size + 1 EXACT= ;

: CASES ( -- )
   s" 1e2" 100 EXACT=
   s" 100.0" 100 EXACT=
   s" 1.5e1" 15 EXACT=
   s" 1.05e1" EXACT-NONE
   s" +.5e1" 5 EXACT=
   s" 1." 1 EXACT=
   s" -0.000e999999999999999999999" 0 EXACT=
   s" 0e-999999999999999999999" 0 EXACT=
   s" 1e999999999999999999999" EXACT-NONE
   s" 1e-999999999999999999999" EXACT-NONE
   LONG-CANCELLATION
   s" 9007199254740993" 9007199254740993 EXACT=
   s" 9223372036854775807" STR-MAX-I64 EXACT=
   s" -9223372036854775808" STR-MIN-I64 EXACT=
   s" 9223372036854775808" EXACT-NONE
   s" -9223372036854775809" EXACT-NONE
   s" 9.223372036854775807e18" STR-MAX-I64 EXACT=
   s" -9.223372036854775808e18" STR-MIN-I64 EXACT=
   s" 9.223372036854775808e18" EXACT-NONE
   s" 0001_2.0_0e+1" 120 EXACT=
   s" 1e1_0" 10000000000 EXACT=
   s" 1__2" EXACT-NONE
   s" 1_.0" EXACT-NONE
   s" 1._0" EXACT-NONE
   s" 1e_2" EXACT-NONE
   s" 1e2_" EXACT-NONE
   s"  1" EXACT-NONE
   s" 1 " EXACT-NONE
   s" 0x10" EXACT-NONE
   s" NaN" EXACT-NONE
   s" inf" EXACT-NONE
   s" +" EXACT-NONE
   s" ." EXACT-NONE
   s" 1e" EXACT-NONE
   s" 1__2" 12 INTEGER=
   s" +00012" 12 INTEGER=
   s" -9223372036854775808" STR-MIN-I64 INTEGER=
   s" 9223372036854775808" INTEGER-NONE
   s" 1_" INTEGER-NONE
   s" _1" INTEGER-NONE
   s" 1.0" INTEGER-NONE
   s" 1e2" INTEGER-NONE
   s\" \"1__2\"" 12 STRING=
   s\" \"1.5e1\"" 15 STRING=
   [: BAD-DECIMAL-LEN ;] DECIMAL:E-SOURCE TTHROWSQ
   [: BAD-DECIMAL-PTR ;] DECIMAL:E-SOURCE TTHROWSQ ;

public
: RUN ( -- ) CASES ;
;package

T-RESET
DECIMAL-TEST:RUN
T-REPORT
s" decimal-test: ok" type cr
