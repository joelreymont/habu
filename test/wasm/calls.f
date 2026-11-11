\ calls.f - WCALLS: execute, catch and finally through xts, quotations and rows.
\ CHECKS throws the number of the first entry whose code is not the one it throws
\ natively; test/wasm/calls-test.f runs CHECKS natively and as a module's MAIN, and
\ each trapping word as a module of its own.

package WCALLS
private
CAST: >CELL ( n -- ptr n )
CAST: >VOID ( n -- [ -- ] )
CAST: >BITS ( [ -- ] -- n )
: MARK ( -- ) 37 throw ;
variable RUNS
: CLEANUP ( -- ) 1 RUNS +! ;
: FAILING ( -- ) CLEANUP RUNS @ 0<> if 47 throw then ;
variable DEPTH0
: W17 ( n n n n n n n n n n n n n n n n n -- n )
   3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * +
   3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + ;
: DIRECT ( -- ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 W17 throw ;
: VIA-EXECUTE ( -- ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 ['] W17 execute throw ;
: VIA-LANES ( -- ) 5 [: 1 + ;] execute [: 1 2 ;] execute + + throw ;
: CAUGHT ( -- ) 7 [: $8000000000000001 throw ;] catch + throw ;
: RETURNED ( -- ) [: 9 throw ;] catch [: ;] catch + throw ;
: DEPTHS ( -- ) depth DEPTH0 !  1 2 3 depth DEPTH0 @ - throw ;
: FINALLY-KEPT ( -- ) 0 RUNS ! 5 [: 2 * 3 ;] [: CLEANUP ;] finally + RUNS @ 100 * + throw ;
: FINALLY-PASSED ( -- )
   0 RUNS !
   [: [: $8000000000000010 throw ;] [: CLEANUP ;] finally ;] catch RUNS @ + throw ;
: FINALLY-SUPERSEDED ( -- )
   0 RUNS !
   [: [: 41 throw ;] [: FAILING ;] finally ;] catch RUNS @ 100 * + throw ;
: FINALLY-RESTORED ( -- )
   0 RUNS !
   [: [: 41 throw ;] [: [: 9 throw ;] catch RUNS +! ;] finally ;] catch RUNS @ 100 * + throw ;
: FINALLY-THROWN ( -- )
   0 RUNS !
   [: 5 [: 1 + ;] [: FAILING ;] finally drop ;] catch RUNS @ 100 * + throw ;
: FINALLY-DEPTH ( -- )
   0 RUNS !  depth DEPTH0 !
   [: 1 2 3 [: RUNS @ 0= if 4 5 41 throw then ;] [: depth DEPTH0 @ - RUNS ! ;] finally drop drop drop ;] catch RUNS @ 100 * + throw ;
variable RES
: ROWS ( R n [ R n -- S ] -- S ) execute ;
: R2-S1 ( -- ) 1 2 3 [: + + ;] ROWS throw ;
: R0-S1 ( -- ) 3 [: 4 + ;] ROWS throw ;
: AFTER ( R n [ R n -- S ] -- S ) execute [: ;] execute ;
: AFTER-GROW ( -- )
   depth DEPTH0 !
   5 [: dup 3 + ;] AFTER swap 10 * +  depth DEPTH0 @ - 100 * + throw ;
CAST: >GROW ( [ R n [ R n -- S ] -- S ] -- [ n [ n -- n n ] -- n n ] )
: AFTER-VIA-EXECUTE ( -- )
   depth DEPTH0 !
   5 [: dup 3 + ;] ['] AFTER >GROW execute swap 10 * +  depth DEPTH0 @ - 100 * + throw ;
: FIN-BODY ( R n [ R n -- S ] -- S ) execute ;
: FIN-ROWS ( R n [ R n -- S ] -- S )
   {: body :}
   body [: FIN-BODY ;] [: CLEANUP ;] finally ;
: FIN-SHRINK ( -- )
   0 RUNS !  depth DEPTH0 !
   1 2 3 [: + + RES ! ;] FIN-ROWS
   RES @  RUNS @ 1000 * +  depth DEPTH0 @ - 100 * + throw ;
: FIN-GROW ( -- )
   0 RUNS !  depth DEPTH0 !
   5 [: dup 3 + ;] FIN-ROWS swap 10 * +
   RUNS @ 1000 * +  depth DEPTH0 @ - 100 * + throw ;
: ROWS-N ( R n [ R n -- S n ] -- S n ) execute ;
: ROWS-N-SHRINK ( -- )
   depth DEPTH0 !
   1 2 3 [: + + ;] ROWS-N  depth DEPTH0 @ - 100 * + throw ;
: ROWS-N-GROW ( -- )
   depth DEPTH0 !
   5 [: dup 3 + ;] ROWS-N swap 10 * +  depth DEPTH0 @ - 100 * + throw ;
: FIN-N-BODY ( R n [ R n -- S n ] -- S n ) execute ;
: FIN-N ( R n [ R n -- S n ] -- S n ) [: FIN-N-BODY ;] [: ;] finally ;
: FIN-N-SHRINK ( -- )
   depth DEPTH0 !
   1 2 3 [: + + ;] FIN-N  depth DEPTH0 @ - 100 * + throw ;
PERSISTED-PTR-VARIABLE DATA-CELL
s" data cell text" drop DATA-CELL !
: TEXT ( -- ) DATA-CELL @ c@ 100 *  DATA-CELL @ 13 + c@ + throw ;
package OWN
: execute ( n -- n ) 1 + ;
: catch ( n -- n ) 1 + ;
public
: EXECUTED ( -- ) 4 execute throw ;
: CAUGHT ( -- ) 4 catch throw ;
;package
\ The entry's caught code is want, else the check's number k is thrown.
: WANT ( [ -- ] n n -- )
   {: word want:n k:n :}
   word catch want <> if k throw then ;

public

: CHECKS ( -- )
   [: DIRECT ;] 1065406345 1 WANT  [: VIA-EXECUTE ;] 1065406345 2 WANT
   [: VIA-LANES ;] 9 3 WANT  [: CAUGHT ;] -9223372036854775800 4 WANT
   [: RETURNED ;] 9 5 WANT  [: DEPTHS ;] 3 6 WANT
   [: FINALLY-KEPT ;] 113 7 WANT  [: FINALLY-PASSED ;] -9223372036854775791 8 WANT
   [: FINALLY-SUPERSEDED ;] 147 9 WANT  [: FINALLY-RESTORED ;] 941 10 WANT
   [: FINALLY-THROWN ;] 147 11 WANT  [: FINALLY-DEPTH ;] 341 12 WANT
   [: R2-S1 ;] 6 13 WANT  [: R0-S1 ;] 7 14 WANT
   [: AFTER-GROW ;] 158 15 WANT  [: AFTER-VIA-EXECUTE ;] 158 16 WANT
   [: FIN-SHRINK ;] 1106 17 WANT  [: FIN-GROW ;] 1158 18 WANT
   [: ROWS-N-SHRINK ;] 106 19 WANT  [: ROWS-N-GROW ;] 158 20 WANT
   [: FIN-N-SHRINK ;] 106 21 WANT
   [: OWN:EXECUTED ;] 5 22 WANT  [: OWN:CAUGHT ;] 5 23 WANT
   [: TEXT ;] 10116 24 WANT ;

: TRAPPED ( -- ) [: 0 >CELL @ drop ;] catch throw ;
: ZERO ( -- ) 0 >VOID execute ;
: ZERO-CAUGHT ( -- ) 0 >VOID catch throw ;
: ZERO-CLEANUP ( -- ) [: 1 . ;] 0 >VOID finally ;
: HIGH ( -- ) ['] MARK >BITS $100000000 or >VOID execute ;
: HIGH-TAGGED ( -- ) ['] MARK >BITS $8000000100000000 or >VOID execute ;

;package
