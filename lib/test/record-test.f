\ record-test.f - focused tests for lib/test/record.f failure records.
\ Run: bin/hb --load lib/test.f lib/test/record-test.f
\ Expected literals are bound in compiled words: top-level (interpret-mode)
\ escaped literals corrupt positionally (dot habu-interpret-mode-escaped-d8dad34b).

require lib/test.f

variable RCT-I

: RCT-WANT-BOOM$ ( -- ptr u8 n )
   S\" TFAIL\trunner\t3\tboom" ;

: RCT-WANT-NEG$ ( -- ptr u8 n )
   S\" TFAIL\tassert\t-12\tneg-id" ;

: RCT-WANT-EMPTY-LABEL$ ( -- ptr u8 n )
   S\" TFAIL\tsnap\t0\t" ;

: RCT-WANT-DIGITS$ ( -- ptr u8 n )
   S\" TFAIL\tassert\t105\tdigits case" ;

: RCT-FILL-OVER ( -- )
   TREC-RESET
   0 RCT-I !
   begin RCT-I @ TREC-CAP 2 + < while
      STR-ZERO TREC-C+
      RCT-I @ 1+ RCT-I !
   repeat ;

T-RESET

\ record format: TFAIL <layer> <id> <label> separated by tabs
s" runner" 3 s" boom" TREC-FAIL$ RCT-WANT-BOOM$ T$=
s" assert" -12 s" neg-id" TREC-FAIL$ RCT-WANT-NEG$ T$=
s" snap" 0 s" " TREC-FAIL$ RCT-WANT-EMPTY-LABEL$ T$=
s" assert" 105 s" digits case" TREC-FAIL$ RCT-WANT-DIGITS$ T$=

\ builder primitives
TREC-RESET TREC$ s" " T$=
TREC-RESET s" ab" TREC-$+ TREC$ s" ab" T$=
TREC-RESET -407 TREC-N+ TREC$ s" -407" T$=

\ payload sanitization: tab/CR/LF in a label degrade to spaces so the
\ record stays one TSV line with exactly four columns
create RCT-DIRTY 5 allot
char a RCT-DIRTY c!  9 RCT-DIRTY 1 + c!  char b RCT-DIRTY 2 + c!
10 RCT-DIRTY 3 + c!  13 RCT-DIRTY 4 + c!
: RCT-DIRTY$ ( -- ptr u8 n ) RCT-DIRTY 5 ;
TREC-RESET RCT-DIRTY$ TREC-$+ TREC$ s" a b  " T$=

\ capacity guard fails closed with a named error
' RCT-FILL-OVER E-STR-CAPACITY TTHROWS
TREC-RESET

T-CASES 9 T=
T-FAILURES 0 T=
T-REPORT
