\ Parsing literals and data-stack calls use the production native compiler.
require src/lib/test.f
require src/lib/ieee754.f

package NATIVE-LITERALS-TEST

: CHARACTER ( -- n ) [char] Z ;
: LOCAL-IMMEDIATE ( n -- n ) {: immediate:n :} immediate ;
: LOCAL-MATCH ( n -- n ) {: match:n :} match ;
: SAME-BRANCH ( n -- n ) {: x:n :} x 0= if 32768 exit then 32768 ;
: SAME-REAL ( r -- n ) {: x:r :} x 0.0 f= if 42 exit then 42 ;
: LOCAL-REBOUND ( -- n ) 1 {: x:n :} x 41 + {: x:n :} x ;
\ Scalar halves must survive native selection, allocation and execution.
: UPPER-ONE ( -- n ) $10000 ;
: UPPER-TWO ( -- n ) $100000000 ;
: UPPER-THREE ( -- n ) $1000000000000 ;
: UPPER-MIXED ( -- n ) $1234005600000000 ;
: ONES-MIXED ( -- n ) $FFFFFFFF0000FFFF ;
: ZERO-SCALAR ( -- n ) 0 ;
: ONES-SCALAR ( -- n ) -1 ;
: UPPER-USE ( n -- n ) $100000000 + ;

: DECIMAL ( -- r ) 9.9996 ;
: NEG-DECIMAL ( -- r ) -9.9996 ;
: POS-ZERO ( -- r ) 0.0 ;
: NEG-ZERO ( -- r ) -0.0 ;
: INT-TIE ( -- r ) 9007199254740993.0 ;
: INT-FRACTION ( -- r ) 9007199254740993.5 ;
: EVEN-TIE ( -- r ) 4503599627370496.5 ;
: ODD-TIE ( -- r ) 4503599627370497.5 ;
: SIG-CARRY ( -- r ) 9007199254740991.5 ;
: MIN-FRACTION ( -- r ) 0.000000000000000001 ;
: MAX-INTEGER ( -- r ) 9223372036854775807.0 ;

\ Parse the oracle at runtime so a selector error cannot change both values.
: MATCH-PARSED ( n ptr u8 n -- )
   {: got:n src:ptr len:n :}
   src len num-parse {: want:n flt:bool ok:bool :}
   ok TTRUE flt TFALSE got want T= ;
: LONG-BODY ( n -- n )
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
   1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 
;

: MANY-BLOCKS ( n -- n )
   case
      0 of 1000 endof
      1 of 1001 endof
      2 of 1002 endof
      3 of 1003 endof
      4 of 1004 endof
      5 of 1005 endof
      6 of 1006 endof
      7 of 1007 endof
      8 of 1008 endof
      9 of 1009 endof
      10 of 1010 endof
      11 of 1011 endof
      12 of 1012 endof
      13 of 1013 endof
      14 of 1014 endof
      15 of 1015 endof
      16 of 1016 endof
      17 of 1017 endof
      18 of 1018 endof
      19 of 1019 endof
      20 of 1020 endof
      21 of 1021 endof
      22 of 1022 endof
      23 of 1023 endof
      24 of 1024 endof
      25 of 1025 endof
      26 of 1026 endof
      27 of 1027 endof
      28 of 1028 endof
      29 of 1029 endof
      30 of 1030 endof
      31 of 1031 endof
      32 of 1032 endof
      33 of 1033 endof
      34 of 1034 endof
      35 of 1035 endof
      36 of 1036 endof
      37 of 1037 endof
      38 of 1038 endof
      39 of 1039 endof
      -1 swap
   endcase ;
: MANY ( n n n n n n n n n n n -- n )
   + + + + + + + + + + ;

\ The image's defining words must retain their relocated signature metadata.
PTR-VARIABLE SLOT
create VALUE 42 ,

: RUN ( -- )
   T-RESET
   DECIMAL IEEE754:F64>BITS $4023FFCB923A29C7 T=
   NEG-DECIMAL IEEE754:F64>BITS $C023FFCB923A29C7 T=
   POS-ZERO IEEE754:F64>BITS 0 T=
   NEG-ZERO IEEE754:F64>BITS $8000000000000000 T=
   INT-TIE IEEE754:F64>BITS $4340000000000000 T=
   INT-FRACTION IEEE754:F64>BITS $4340000000000001 T=
   EVEN-TIE IEEE754:F64>BITS $4330000000000000 T=
   ODD-TIE IEEE754:F64>BITS $4330000000000002 T=
   SIG-CARRY IEEE754:F64>BITS $4340000000000000 T=
   MIN-FRACTION IEEE754:F64>BITS $3C32725DD1D243AC T=
   MAX-INTEGER IEEE754:F64>BITS $43E0000000000000 T=
   CHARACTER 90 T=
   0 SAME-BRANCH 32768 T=
   1 SAME-BRANCH 32768 T=
   0.0 SAME-REAL 42 T=
   1.0 SAME-REAL 42 T=
   LOCAL-REBOUND 42 T=
   UPPER-ONE s" $10000" MATCH-PARSED
   UPPER-TWO s" $100000000" MATCH-PARSED
   UPPER-THREE s" $1000000000000" MATCH-PARSED
   UPPER-MIXED s" $1234005600000000" MATCH-PARSED
   ONES-MIXED s" $FFFFFFFF0000FFFF" MATCH-PARSED
   ZERO-SCALAR s" 0" MATCH-PARSED
   ONES-SCALAR s" -1" MATCH-PARSED
   7 UPPER-USE s" $100000007" MATCH-PARSED
   0 LONG-BODY 600 T=
   0 MANY-BLOCKS 1000 T=
   20 MANY-BLOCKS 1020 T=
   39 MANY-BLOCKS 1039 T=
   40 MANY-BLOCKS -1 T=
   42 LOCAL-IMMEDIATE 42 T=
   42 LOCAL-MATCH 42 T=
   1 2 3 4 5 6 7 8 9 10 11 MANY 66 T=
   VALUE SLOT !
   SLOT @ @ 42 T=
   T-REPORT ;

RUN
;package
