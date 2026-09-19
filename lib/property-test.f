\ property-test.f - focused tests for property helper library.
\ Run: cat lib/errors.f lib/test.f lib/property.f lib/property-test.f | bin/hb

require lib/errors.f
require lib/test.f
require lib/property.f

: PT-BAD-SEED ( -- )
   -1 1 PROP:RUN-RESET ;

: PT-BAD-COUNT ( -- )
   1 PROP:MAX-COUNT 1+ PROP:RUN-RESET ;

: PT-ZERO-MOD ( -- )
   0 PROP:RND% drop ;

: PT-BAD-ROOM ( -- )
   PROP:BUF-CAP 1+ PROP:BUF-CHECK-ROOM ;

: PT-BAD-DIGIT ( -- )
   10 PROP:DIGIT+ ;

: PT-BAD-STEP ( -- )
   0 PROP:GEN-START
   s" drop " 1 -1 PROP:GEN-STEP ;

: PT-SHRINK-KEEP? ( -- bool )
   PROP:BUF$ nip 4 >= ;

: PT-SHRINK-FALSE? ( -- bool )
   0 0= 0= ;

: PT-BAD-SHRINK ( -- )
   PROP:BUF-RESET
   s" x " PROP:BUF+
   [: PT-SHRINK-FALSE? ;] PROP:SHRINK ;

: PT-EXAMPLE-PROP ( -- )
   7 16 PROP:RUN-RESET
   0 begin dup PROP:COUNT@ < while
      100 PROP:RND% dup * 0 >= TTRUE
      1+
   repeat drop ;

create PT-PICKS 8 cells allot

\ Check both adjacent draws and the fixed-stride sampling used by generators.
\ Low-bit LCG picks cover 0..7 but repeat every eight draws.
: PT-PICK-COVERAGE ( -- )
   1 PROP:SEED!
   0 0 false
   4096 0 ?do
      {: seen:n stride:n changed:bool :}
      8 PROP:RND% {: pick:n :}
      PT-PICKS i 7 and cells + {: slot:ptr :}
      seen 1 pick lshift or
      i 7 and 0= if stride 1 pick lshift or else stride then
      i 8 >= if changed slot @ pick <> or else changed then
      pick slot !
   loop
   TTRUE 255 T= 255 T= ;

\ Over 4096 transitions, repeats and alternations each occupy 40--60%.
\ The raw LCG low bit has no repeats at all.
: PT-COIN-RUNS ( -- )
   1 PROP:SEED!
   2 PROP:RND% 0
   4096 0 ?do
      {: previous:n repeats:n :}
      2 PROP:RND% {: pick:n :}
      pick repeats previous pick = if 1+ then
   loop
   nip dup 1639 >= TTRUE 2457 <= TTRUE ;

T-RESET

PROP:DEFAULTS PROP:DEFAULT-COUNT T= PROP:DEFAULT-SEED T=
1 5 PROP:RUN-RESET
PROP:SEED@ 1 T=
PROP:COUNT@ 5 T=
PROP:RND 1103527590 T=
PROP:SEED@ 1103527590 T=

1 PROP:SEED!
10 PROP:RND% 2 T=
8 PROP:RND% 1 T=

PROP:BUF-RESET
65 PROP:BUF-C+
7 PROP:DIGIT+
PROP:BUF$ s" A7" T$=

PROP:BUF-RESET
s" abc   " PROP:BUF+
PROP:TRIM-TRAIL
PROP:BUF$ s" abc" T$=

PROP:BUF-RESET
s" abc def " PROP:BUF+
PROP:DROP-LAST TTRUE
PROP:BUF$ s" abc " T$=

0 PROP:GEN-START
s" 7 " 0 1 PROP:GEN-STEP
s" drop " 1 -1 PROP:GEN-STEP
PROP:GEN-DEPTH@ 0 T=
PROP:BUF$ s" 7 drop " T$=

PROP:BUF-RESET
s" dup drop 1+ " PROP:BUF+
' PT-SHRINK-KEEP? PROP:SHRINK
PROP:BUF$ s" dup " T$=

' PT-BAD-SEED E-PROP-SEED TTHROWS
' PT-BAD-COUNT E-PROP-SEED TTHROWS
' PT-ZERO-MOD E-PROP-GENERATOR TTHROWS
' PT-BAD-ROOM E-PROP-CAPACITY TTHROWS
' PT-BAD-DIGIT E-PROP-GENERATOR TTHROWS
' PT-BAD-STEP E-PROP-GENERATOR TTHROWS
' PT-BAD-SHRINK E-PROP-SHRINK TTHROWS

PT-EXAMPLE-PROP
PT-PICK-COVERAGE
PT-COIN-RUNS

T-REPORT
