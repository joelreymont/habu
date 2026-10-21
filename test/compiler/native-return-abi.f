\ A value a native body parks with `>r` survives the calls the body makes before
\ it takes the value back: a divide's generated helper call (on x86-64 the
\ division helper, src/compiler/native/select-x64.f), an ordinary final call, a
\ quotation's own calls and a does> clause's. No signature declares a
\ return-stack cell (docs/effects.md "The return-stack clause"), so every case
\ parks and takes back within one definition.
require src/lib/test.f

1 set-tier

package NRABI-FIXTURE
public

: DIV-OK ( -- n n ) 8 >r 21 3 / r> ;

: FINAL ( n -- n ) 1+ ;
: BRIDGED-TAIL ( n -- n ) >r 21 3 / r> + FINAL ;
: QUOTED ( n -- n ) [: ( n -- n ) >r 21 3 / r> + FINAL ;] execute ;
: MAKER ( -- ) create does> ( -- n ) drop 5 >r 21 3 / r> + ;
MAKER MADE

;package

T-RESET
s" a returning divide leaves the parked return cell in place" T-LABEL
NRABI-FIXTURE:DIV-OK 8 T= 7 T=
s" a generated division call precedes a final ordinary call" T-LABEL
8 NRABI-FIXTURE:BRIDGED-TAIL 16 T=
s" a quotation's generated call keeps its parked cell" T-LABEL
8 NRABI-FIXTURE:QUOTED 16 T=
s" a does clause's generated call keeps its parked cell" T-LABEL
NRABI-FIXTURE:MADE 12 T=
T-REPORT
