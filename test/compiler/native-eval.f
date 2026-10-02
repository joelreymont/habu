\ The closed evaluation boundary: `evaluate-closed` runs source on a data
\ stack of its own and refuses residue by name, which is what lets a checked
\ word evaluate source. Tier-neutral by design: the boundary asserted is the
\ engine primitive and the checker's row for it, which no compiler tier moves.
require src/core/engine-error.f
require src/habu/stack-abi.f
require lib/errors.f
require lib/test.f
require lib/test/subject.f

package NATIVE-EVAL-TEST

public

\ The nested cases' inner texts. A text cannot spell a string literal inside
\ its own literal, so it reaches an inner text through a word, and the word is
\ public because the texts run after this package has closed.
: INNER-DEPTH$ ( -- ptr u8 n ) s" depth 0 T=" ;
: INNER-RESIDUE$ ( -- ptr u8 n ) s" 1 2" ;

\ The data-stack base each text of a nested pair sees.
variable OUTER-BASE
variable INNER-BASE
: INNER-BASE$ ( -- ptr u8 n )
   s" data-base STACK-ABI:BASE-CELL + @ NATIVE-EVAL-TEST:INNER-BASE !" ;

private

: DEFINES ( -- )
   s" a definitions text loads" T-LABEL
   s" : NATIVE-EVAL-ANSWER ( -- n ) 42 ;" evaluate-closed
   s" NATIVE-EVAL-ANSWER 42 T=" evaluate-closed ;

: RESIDUE ( -- )
   s" a text that leaves cells is refused by name" T-LABEL
   [: s" 1 2" evaluate-closed ;] E-EVAL-RESIDUE TTHROWSQ ;

: UNDER ( -- n )
   7 [: s" drop" evaluate-closed ;] catch 70 T= ;

: FLOOR ( -- )
   s" a text reaching below the caller's depth throws 70" T-LABEL
   UNDER
   s" the refused text leaves the caller's cell" T-LABEL
   7 T= ;

: REFUSED ( -- )
   s" a definition the checker refuses throws 70" T-LABEL
   [: s" : NATIVE-EVAL-BAD ( -- n ) 1 2 ;" evaluate-closed ;] 70 TTHROWSQ ;

: DEPTH0 ( -- )
   s" depth inside a closed text starts at zero" T-LABEL
   7 8 s" depth 0 T=" evaluate-closed
   s" the caller's cells survive the text" T-LABEL
   8 T= 7 T= ;

: NESTED ( -- )
   s" a nested closed text runs at its own floor, the outer one at its" T-LABEL
   9 s" 5 NATIVE-EVAL-TEST:INNER-DEPTH$ evaluate-closed depth 1 T= 5 T="
   evaluate-closed
   9 T=
   s" an inner text's residue is refused through the outer text" T-LABEL
   [: s" NATIVE-EVAL-TEST:INNER-RESIDUE$ evaluate-closed" evaluate-closed ;]
   E-EVAL-RESIDUE TTHROWSQ ;

: OUTER-BASE$ ( -- ptr u8 n )
   s" data-base STACK-ABI:BASE-CELL + @ NATIVE-EVAL-TEST:OUTER-BASE ! NATIVE-EVAL-TEST:INNER-BASE$ evaluate-closed" ;

: STACK-BASE ( -- n ) data-base STACK-ABI:BASE-CELL + @ ;

\ A closed text runs on a guarded stack of its own, so the cell under its
\ floor is a guard page, never a caller's cell. The stacks come from a pool:
\ a second run at the same nesting reuses the first run's pair.
: FLOOR-POOL ( -- )
   s" a closed text runs on a stack that is not the caller's" T-LABEL
   OUTER-BASE$ evaluate-closed
   OUTER-BASE @ STACK-BASE <> TTRUE
   s" a nested text runs on a stack that is not the outer text's" T-LABEL
   INNER-BASE @ OUTER-BASE @ <> TTRUE
   s" a second run reuses the same two stacks" T-LABEL
   OUTER-BASE @ INNER-BASE @ {: outer:n inner:n :}
   OUTER-BASE$ evaluate-closed
   OUTER-BASE @ outer T=  INNER-BASE @ inner T= ;

: AGREEMENT ( -- )
   s" E-EVAL-RESIDUE matches the engine's own spelling" T-LABEL
   E-EVAL-RESIDUE STACK-ABI:E-EVAL-RESIDUE T= ;

$1000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ RATCHET grows the stack one cell per level (recurse consumes the declared
\ input and nothing pops it), so the text pushes until a push crosses the top
\ of its stack's mapping.
: OVERFLOW$ ( -- ptr u8 n )
   S\" : RATCHET ( n -- ) begin dup recurse again ; s\" 1 RATCHET\" evaluate-closed" ;

: OVERFLOW ( -- )
   s" an overflow inside a closed text exits STACK-BOUNDS" T-LABEL
   OVERFLOW$ OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROC-OUTCOME>RC RC>N ENGINE-ERROR:STACK-BOUNDS T=
   nip LEN>N {: erru:n :}
   s" and names the data stack" T-LABEL
   ERR erru S\" hb: stack bounds exceeded (data)\n" T$= ;

\ The text jumps one cell under its floor. That cell lies in its own stack's
\ low guard page, not among the caller's cells, so the fetch faults there and
\ the crash handler names the data stack.
: FLOOR-JUMP$ ( -- ptr u8 n )
   S\" 7 s\" data-base STACK-ABI:BASE-CELL + @ cell - execute\" evaluate-closed" ;

: FLOOR-JUMP ( -- )
   s" a jump under a closed text's floor exits STACK-BOUNDS" T-LABEL
   FLOOR-JUMP$ OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROC-OUTCOME>RC RC>N ENGINE-ERROR:STACK-BOUNDS T=
   nip LEN>N {: erru:n :}
   s" and names the data stack" T-LABEL
   ERR erru s" hb: stack bounds exceeded (data)" CONTAINS? TTRUE ;

$4F constant TASK-LIVE                  \ habu1.f B-TASK-LIVE-GUARD's exit

\ The text would print; a live task must stop it before it is read.
: LIVE$ ( -- ptr u8 n )
   S\" 1 data-base TASKS-LIVE-CELL + ! s\" 1 .\" evaluate-closed" ;

: LIVE ( -- )
   s" a live task stops a closed text before it runs" T-LABEL
   LIVE$ OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROC-OUTCOME>RC RC>N TASK-LIVE T=
   s" and nothing is printed" T-LABEL
   LEN>N 0 T=  LEN>N 0 T= ;

: CHECKED ( -- )
   s" a checked body naming evaluate is refused" T-LABEL
   s" NATIVE-EVAL-OPEN ( ptr u8 n -- ) evaluate" CHECK! 0 T=
   s" a checked body naming evaluate-closed certifies" T-LABEL
   s" NATIVE-EVAL-CLOSED ( ptr u8 n -- ) evaluate-closed" CHECK! -1 T= ;

: RUN ( -- )
   T-RESET
   DEFINES RESIDUE FLOOR REFUSED DEPTH0 NESTED FLOOR-POOL AGREEMENT OVERFLOW
   FLOOR-JUMP LIVE CHECKED
   T-REPORT ;

' RUN
;package
execute
