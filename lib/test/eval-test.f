\ eval-test.f - typed values and throw codes out of evaluated test text.

require lib/errors.f
require lib/fmt.f
require lib/test.f

package EVAL-TEST

\ The texts run after this package has closed, so the words they define land
\ in the caller's wordlist and carry an EVAL-TEST- prefix.

: SUM ( -- )
   s" a text leaving one cell gives that cell" T-LABEL
   s" 40 2 +" TEST-EVAL:N 42 T= ;

: DEFINES ( -- )
   s" a text defines a word, then leaves a value through it" T-LABEL
   s" : EVAL-TEST-SIX ( -- n ) 6 ; EVAL-TEST-SIX 7 *" TEST-EVAL:N 42 T=
   s" the text's definition outlives the text" T-LABEL
   s" EVAL-TEST-SIX" TEST-EVAL:N 6 T= ;

: NESTED ( -- )
   s" N runs inside the text of N" T-LABEL
   S\" s\" 40\" TEST-EVAL:N 2 +" TEST-EVAL:N 42 T= ;

: CALLER ( -- )
   s" N leaves the caller's cells" T-LABEL
   7 s" 40 2 +" TEST-EVAL:N 42 T=
   7 T= ;

: FLAGS ( -- )
   s" FLAG reads a true text" T-LABEL
   s" 1 1 =" TEST-EVAL:FLAG TTRUE
   s" FLAG reads a false text" T-LABEL
   s" 1 2 =" TEST-EVAL:FLAG TFALSE ;

: EMPTY ( -- )
   s" N refuses a text that leaves no cell" T-LABEL
   [: s" " TEST-EVAL:N drop ;] 70 TTHROWSQ ;

: RESIDUE ( -- )
   s" N refuses a text that leaves two cells" T-LABEL
   [: s" 1 2" TEST-EVAL:N drop ;] E-EVAL-RESIDUE TTHROWSQ ;

: UNFINISHED ( -- )
   s" N refuses a text that ends inside a definition it opened, rc 74" T-LABEL
   [: s" : EVAL-TEST-OPEN ( -- n ) 1" TEST-EVAL:N drop ;] 74 TTHROWSQ ;

: UNDER ( -- n )
   7 [: s" drop 1" TEST-EVAL:N drop ;] catch 70 T= ;

: FLOOR ( -- )
   s" N's text reaching below its floor throws 70" T-LABEL
   UNDER
   s" the refused text leaves the caller's cell" T-LABEL
   7 T= ;

: CODES ( -- )
   s" RC is 0 for a text that loads" T-LABEL
   s" : EVAL-TEST-SEVEN ( -- n ) 7 ;" TEST-EVAL:RC 0 T=
   s" RC is 0 for an empty text" T-LABEL
   s" " TEST-EVAL:RC 0 T=
   s" RC names residue" T-LABEL
   s" 1 2" TEST-EVAL:RC E-EVAL-RESIDUE T=
   s" RC names a definition the checker refuses" T-LABEL
   s" : EVAL-TEST-B ( -- n ) 1 2 ;" TEST-EVAL:RC 70 T= ;

\ The twins differ only in the declared output, so the refusal is the type of
\ N's result and not the text around it.
: REJECTED ( -- )
   s" a word consuming N's n as a bool is refused" T-LABEL
   S\" : EVAL-TEST-BAD ( -- bool ) s\" 1\" TEST-EVAL:N ;" TEST-EVAL:RC 70 T=
   s" its twin declaring n certifies" T-LABEL
   S\" : EVAL-TEST-GOOD ( -- n ) s\" 1\" TEST-EVAL:N ;" TEST-EVAL:RC 0 T=
   s" and runs" T-LABEL
   s" EVAL-TEST-GOOD" TEST-EVAL:N 1 T= ;

public

: TEST ( -- )
   T-RESET
   SUM DEFINES NESTED CALLER FLAGS EMPTY RESIDUE UNFINISHED FLOOR CODES REJECTED
   T-REPORT
   s" eval-test: ok, " type T-CASES FMT:.INT s"  cases" type cr ;

;package

EVAL-TEST:TEST
