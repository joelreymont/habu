\ codegen-test.f - the shared codegen byte buffer (src/lib/codegen.f): a text
\ past the first mapping is kept whole, and a negative decimal keeps its sign.
\ Run: bin/hb --load src/lib/codegen.f src/lib/codegen-test.f

require src/lib/test/assert.f
require src/lib/codegen.f

CODEGEN:BUFFER CGT-D
CODEGEN:BUFFER CGT-W

\ 1000 bytes, each the low digit of its index, past the 256-byte first mapping.
1000 constant CGT-LONG
: CGT-DIGIT ( n -- n )  10 mod [char] 0 + ;
: CGT-LONG-FILL ( -- )
   CGT-D CODEGEN:RESET
   CGT-LONG 0 ?do  i CGT-DIGIT CGT-D CODEGEN:APPEND-BYTE  loop ;
: CGT-LONG-KEPT? ( -- bool )
   CGT-D CODEGEN:CONTENTS {: s:str :}
   true  s STR:LEN 0 ?do  s STR:DATA i + c@ i CGT-DIGIT = and  loop ;

: CGT-GROW ( -- )
   CGT-LONG-FILL
   CGT-D CODEGEN:CONTENTS STR:LEN CGT-LONG T=
   CGT-LONG-KEPT? TTRUE ;

: CGT-DEC$ ( n -- str )
   CGT-W CODEGEN:RESET  CGT-W CODEGEN:APPEND-DECIMAL  CGT-W CODEGEN:CONTENTS ;

: CGT-DECIMAL ( -- )
   0 CGT-DEC$ s" 0" T$=
   1234 CGT-DEC$ s" 1234" T$=
   -5 CGT-DEC$ s" -5" T$=
   -1230 CGT-DEC$ s" -1230" T$=
   $8000000000000000 CGT-DEC$ s" -9223372036854775808" T$= ;

: CGT-RUN ( -- )
   T-RESET
   CGT-GROW
   CGT-DECIMAL
   T-REPORT ;

CGT-RUN
