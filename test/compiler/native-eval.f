\ The closed evaluation boundary: `evaluate-closed` runs source on a data
\ stack of its own and refuses residue by name, which is what lets a checked
\ word evaluate source. Tier-neutral by design: the boundary asserted is the
\ engine primitive and the checker's row for it, which no compiler tier moves.
require src/core/engine-error.f
require src/habu/stack-abi.f
require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/subject.f
require src/lib/task.f

package NATIVE-EVAL-TEST

public

\ The nested cases' inner texts. A text cannot spell a string literal inside
\ its own literal, so it reaches an inner text through a word, and the word is
\ public because the texts run after this package has closed.
: INNER-DEPTH$ ( -- ptr u8 n ) s" depth 0 T=" ;
: INNER-RESIDUE$ ( -- ptr u8 n ) s" 1 2" ;

\ W claims one cell and adds two, the lie a trusted body can tell: a checked text
\ that hands it one is admitted and reaches one cell under its floor at run
\ time. SEEN holds W's last sum, so a W that never finished leaves it alone.
\ GETW reaches W through a quotation xt, which has no record a tick could
\ find, and CLEAN is an empty cleanup for finally. PLUS-XT tells the same lie
\ for a primitive, whose own row cannot: it types `+` as taking one cell.
variable SEEN
TRUSTED: W ( n -- n ) + dup SEEN ! ;
: GETW ( -- [ n -- n ] ) [: W ;] ;
: CLEAN ( -- [ -- ] ) [: ;] ;
TRUSTED: PLUS-XT ( -- [ n -- n ] ) ['] + ;
: INNER-UNDER$ ( -- ptr u8 n ) s" 1 ' NATIVE-EVAL-TEST:W execute" ;

: INNER-OPEN$ ( -- ptr u8 n ) s" : NATIVE-EVAL-E ( -- n ) 1" ;

\ The text EVAL-BODY evaluates would print; on a task's thread evaluation stops
\ before it is read. The child activates the task: tasks do not cross a fork.
TASK:MIN-STACK TASK:TASK EVALUATOR
: EVAL-BODY ( -- ) s" 1 ." evaluate-closed ;
: EVAL-IN-TASK ( -- ) ['] EVAL-BODY EVALUATOR TASK:ACTIVATE  EVALUATOR TASK:KILL ;

\ The data-stack base each text of a nested pair sees.
variable OUTER-BASE
variable INNER-BASE
: INNER-BASE$ ( -- ptr u8 n )
   s" data-base STACK-ABI:BASE-CELL + @ NATIVE-EVAL-TEST:INNER-BASE !" ;
: INNER-THROW$ ( -- ptr u8 n )
   s" data-base STACK-ABI:BASE-CELL + @ NATIVE-EVAL-TEST:INNER-BASE ! 7 throw" ;

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

: OUTER-THROW$ ( -- ptr u8 n )
   s" data-base STACK-ABI:BASE-CELL + @ NATIVE-EVAL-TEST:OUTER-BASE ! NATIVE-EVAL-TEST:INNER-THROW$ evaluate-closed" ;

\ A throw out of a nested pair gives both stacks back. The caught delivery runs
\ compiled code, the checker's package resync, before the handler's extent is
\ restored, so that code must not run on a stack the pool now holds: the pool
\ links an idle stack through its first cell, and the next pair takes both
\ stacks back only if that link survived.
: FLOOR-POOL-THROW ( -- )
   s" a throw out of a nested pair gives both its stacks back" T-LABEL
   [: OUTER-THROW$ evaluate-closed ;] 7 TTHROWSQ
   s" and the next pair reuses both" T-LABEL
   OUTER-BASE @ INNER-BASE @ {: outer:n inner:n :}
   OUTER-BASE$ evaluate-closed
   OUTER-BASE @ outer T=  INNER-BASE @ inner T= ;

\ Each consumer of an xt reaches under the floor the same way, and each answer
\ is the same throw 70 with the caller's 5 intact and W unfinished.
: XT-EXECUTE ( -- n )
   5 [: s" 1 ' NATIVE-EVAL-TEST:W execute" evaluate-closed ;] catch 70 T= ;

: XT-PRIMITIVE ( -- n )
   5 [: s" 1 NATIVE-EVAL-TEST:PLUS-XT execute" evaluate-closed ;] catch 70 T= ;

: XT-FINALLY ( -- n )
   5 [: s" 1 ' NATIVE-EVAL-TEST:W NATIVE-EVAL-TEST:CLEAN finally" evaluate-closed ;]
   catch 70 T= ;

: XT-QUOTATION ( -- n )
   5 [: s" 1 NATIVE-EVAL-TEST:GETW execute" evaluate-closed ;] catch 70 T= ;

: XT-NESTED ( -- n )
   5 [: s" 3 NATIVE-EVAL-TEST:INNER-UNDER$ evaluate-closed" evaluate-closed ;]
   catch 70 T= ;

: XT-UNDER ( -- )
   0 SEEN !
   s" a compiled word executed below the floor throws 70" T-LABEL
   XT-EXECUTE
   s" and leaves the caller's cell, the word unfinished" T-LABEL
   5 T=  SEEN @ 0 T=
   s" a primitive executed below the floor throws 70" T-LABEL
   XT-PRIMITIVE 5 T=
   s" a catch inside the text receives that 70 itself" T-LABEL
   5 s" 1 ' NATIVE-EVAL-TEST:W catch 70 T= 1 T=" evaluate-closed
   5 T=  SEEN @ 0 T=
   s" finally's body reaching below the floor throws 70" T-LABEL
   XT-FINALLY 5 T=  SEEN @ 0 T=
   s" a quotation xt reaching below the floor throws 70" T-LABEL
   XT-QUOTATION 5 T=  SEEN @ 0 T=
   s" an inner text's reach throws 70 through the outer text" T-LABEL
   XT-NESTED 5 T=  SEEN @ 0 T= ;

74 constant UNCLOSED                    \ interpreter.f RC-UNCLOSED: a source ended inside a definition it opened

\ A text that ends inside a definition it opened is refused as every buffer
\ is, and the open definition goes with it: the code it emitted is taken back
\ (a pending definition has no counted record, so ndict@ cannot show it), the
\ next text is interpreted rather than compiled into it, and its name resolves
\ to nothing until it is defined again. The interpretation probe runs before
\ any other text because a throw out of a text resets the compile state on
\ its own.
: UNFIN ( -- n )
   9 [: s" : NATIVE-EVAL-D ( -- n ) 42" evaluate-closed ;] catch
   UNCLOSED T= ;

: UNFIN-NESTED ( -- n )
   9 [: s" NATIVE-EVAL-TEST:INNER-OPEN$ evaluate-closed" evaluate-closed ;] catch
   UNCLOSED T= ;

: UNFINISHED ( -- )
   s" a text that ends inside a definition is refused, rc 74" T-LABEL
   0 SEEN !
   cp@ {: code :}
   UNFIN
   s" and leaves the caller's cell" T-LABEL
   9 T=
   s" the definition's code is rolled back" T-LABEL
   cp@ code - 0 T=
   s" the next text is interpreted" T-LABEL
   s" 5 NATIVE-EVAL-TEST:SEEN !" evaluate-closed
   SEEN @ 5 T=
   s" the definition's name resolves to nothing" T-LABEL
   [: s" NATIVE-EVAL-D" evaluate-closed ;] 70 TTHROWSQ
   s" and can be defined again" T-LABEL
   s" : NATIVE-EVAL-D ( -- n ) 7 ;" evaluate-closed
   s" NATIVE-EVAL-D 7 T=" evaluate-closed ;

: UNFINISHED-NESTED ( -- )
   s" an inner text's unfinished definition throws through the outer text" T-LABEL
   cp@ {: code :}
   UNFIN-NESTED
   s" and leaves the caller's cell" T-LABEL
   9 T=
   s" the inner text's definition is rolled back" T-LABEL
   cp@ code - 0 T=
   [: s" NATIVE-EVAL-E" evaluate-closed ;] 70 TTHROWSQ ;

\ An immediate that runs a text while the caller's definition is open: the
\ text compiles into that definition and ends with it still open, which is
\ not a definition the text opened, so it is not refused.
: BUMP ( -- ) SEEN @ 1 + SEEN ! ;
: CALL-BUMP ( -- ) s" BUMP" evaluate-closed ; immediate
s" CALL-BUMP" 0 parse-imm
: BUMPED ( -- ) CALL-BUMP ;

: CALLER-OPEN ( -- )
   s" a text run inside the caller's open definition compiles into it" T-LABEL
   0 SEEN !  BUMPED  SEEN @ 1 T= ;

\ The same immediate with a text whose token the compile refuses: the refusal,
\ caught inside the immediate, leaves the caller's definition compiling, as it
\ does for any buffer begun inside one (habu2.f EM-EVAL-THROW-RECOVER), and
\ the refused text's stack still goes back to the pool: the next pair takes
\ the same two stacks as the pair before it.
variable CAUGHT
: TRY-BAD ( -- ) [: s" NATIVE-EVAL-NO-WORD" evaluate-closed ;] catch CAUGHT ! ; immediate
s" TRY-BAD" 0 parse-imm

: CALLER-OPEN-REFUSED ( -- )
   s" a text refused inside the caller's open definition is caught there" T-LABEL
   OUTER-BASE$ evaluate-closed
   OUTER-BASE @ INNER-BASE @ {: outer:n inner:n :}
   0 CAUGHT !
   s" package NATIVE-EVAL-TEST public : HOST ( -- n ) TRY-BAD 5 ; ;package" evaluate-closed
   CAUGHT @ 70 T=
   s" and the caller's definition goes on compiling" T-LABEL
   s" NATIVE-EVAL-TEST:HOST 5 T=" evaluate-closed
   s" and the refused text's stack goes back to the pool" T-LABEL
   OUTER-BASE$ evaluate-closed
   OUTER-BASE @ outer T=  INNER-BASE @ inner T= ;

\ `is` and its target are captured in two passes, so a refused target takes
\ back `is` too: a caller left holding `is` refuses at its own `;`.
: TRY-BAD-IS ( -- ) [: s" is NATIVE-EVAL-NO-DEFER" evaluate-closed ;] catch CAUGHT ! ; immediate
s" TRY-BAD-IS" 0 parse-imm

: CALLER-OPEN-IS-REFUSED ( -- )
   s" a refused `is` target is taken back with its `is`" T-LABEL
   0 CAUGHT !
   s" package NATIVE-EVAL-TEST public : HOST-IS ( -- n ) TRY-BAD-IS 5 ; ;package" evaluate-closed
   CAUGHT @ 70 T=
   s" NATIVE-EVAL-TEST:HOST-IS 5 T=" evaluate-closed ;

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
   PROCESS:ENDING>RC RC>N ENGINE-ERROR:STACK-BOUNDS T=
   nip LEN>N {: erru:n :}
   s" and names the data stack" T-LABEL
   ERR erru s" E-STACK-BOUNDS habu: stack bounds exceeded (data) lr " STARTS-WITH?
   TTRUE ;

\ The same reach outside any closed text: the child's text runs a compiled
\ word through execute with one cell on the stack it was given.
: TOP-LEVEL$ ( -- ptr u8 n )
   s" TRUSTED: NATIVE-EVAL-W ( n -- n ) + ; 1 ' NATIVE-EVAL-W execute" ;

: TOP-LEVEL ( -- )
   s" a word reaching below the base outside a closed text throws 70" T-LABEL
   TOP-LEVEL$ OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROCESS:ENDING>RC RC>N 70 T=
   nip LEN>N {: erru:n :}
   s" and names the token the interpreter was running" T-LABEL
   ERR erru s" interpret stack underdepth: execute" CONTAINS? TTRUE
   s" and is no stack bounds exit" T-LABEL
   ERR erru s" stack bounds exceeded" CONTAINS? TFALSE ;

\ UNFINISHED's text with no catch around it. The child inherits this
\ process's dictionary, which UNFINISHED left holding NATIVE-EVAL-D.
: UNFINISHED-LINE$ ( -- ptr u8 n )
   S\" s\" : NATIVE-EVAL-F ( -- n ) 42\" evaluate-closed" ;

: UNFINISHED-LINE ( -- )
   s" an uncaught unfinished text exits 74" T-LABEL
   UNFINISHED-LINE$ OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROCESS:ENDING>RC RC>N UNCLOSED T=
   nip LEN>N {: erru:n :}
   s" and names the definition and where the text ran" T-LABEL
   ERR erru s" E-VS-UNTERMINATED-DEFINITION habu: " CONTAINS? TTRUE
   ERR erru s" source ended inside definition: NATIVE-EVAL-F" CONTAINS? TTRUE
   ERR erru s" E-VS-UNTERMINATED-DEFINITION habu: source" CONTAINS? 0= TTRUE ;

$4F constant TASK-THREAD                \ habu1.f B-TASK-GUARD's exit

: IN-TASK ( -- )
   s" evaluation in a task exits before it reads the text" T-LABEL
   s" NATIVE-EVAL-TEST:EVAL-IN-TASK" OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROCESS:ENDING>RC RC>N TASK-THREAD T=
   LEN>N {: erru:n :}
   s" and names evaluate-closed, printing nothing else" T-LABEL
   LEN>N 0 T=
   ERR erru s" E-TASK-COMPILE habu: evaluate-closed from a task lr " STARTS-WITH? TTRUE ;

\ The set-tier refusal's whole line up to the lr's 16 hex digits and its newline.
: SET-TIER-LINE$ ( -- ptr u8 n )
   s" E-SET-TIER habu: set-tier: tier must be 0 or 1 lr " ;

: SET-TIER-PAST ( -- )
   s" set-tier past 1 exits 70" T-LABEL
   s" 2 set-tier" OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROCESS:ENDING>RC RC>N 70 T=
   nip LEN>N {: erru:n :}
   s" and prints its one line" T-LABEL
   erru SET-TIER-LINE$ nip 17 + T=
   ERR erru SET-TIER-LINE$ STARTS-WITH? TTRUE
   true  erru 1-  SET-TIER-LINE$ nip ?do ERR i + c@ STR-HEX-DIGIT? and loop TTRUE
   ERR erru + 1- c@ 10 T= ;

\ ---- a definition a thrown text takes back keeps no row ----------------------
\ A text that throws takes back what it defined, and its frame cuts the
\ checker's records back to where the text began, so a later word is checked
\ against the word the engine binds. The texts outrun a source line, so each
\ is assembled in TEXT.
create TEXT IO-CAP allot
variable TEXT-U
variable OUT-U
variable ERR-U

: TEXT+ ( ptr u8 n -- ) TEXT IO-CAP TEXT-U BUF-APPEND ;

: TEXT-RC ( -- n )
   TEXT TEXT-U BUF-LEN@ OUT IO-CAP >LEN ERR IO-CAP >LEN 10000 >MS SUBJECT:RUN
   PROCESS:ENDING>RC RC>N {: rc:n :}
   LEN>N ERR-U !  LEN>N OUT-U !  rc ;

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;

: SAYS ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR ERR-U @ a u CONTAINS? TTRUE ;

\ A text that defines X-FORGET and then throws takes X-FORGET back, row and
\ all, so the next X-FORGET is no duplicate.
: NAME-THROWN ( -- )
   s" a definition its text threw back leaves its name free" T-LABEL
   TEXT-U BUF-RESET
   S\" TRUSTED: EVAL-X-FORGET ( -- ) s\" : X-FORGET ( -- n ) 1 ; 77 throw\" evaluate ; " TEXT+
   s" ' EVAL-X-FORGET catch . cr " TEXT+
   s" : X-FORGET ( -- n ) 2 ; X-FORGET . cr" TEXT+
   TEXT-RC 0 T=
   OUT$ S\" 77\n\n2\n\n" T$= ;

\ The same text inside Q-FORGET: T-FORGET's X-FORGET is the global ( -- n ),
\ not the thrown-back Q-FORGET:X-FORGET ( -- n n ), so T-FORGET ( -- n n ) is
\ refused; typed against the thrown row it would certify and run one cell short.
: ROW-THROWN ( -- )
   s" a definition its text threw back does not type a later caller" T-LABEL
   TEXT-U BUF-RESET
   s" : X-FORGET ( -- n ) 1 ; " TEXT+
   S\" TRUSTED: EVAL-X-FORGET ( -- ) s\" : X-FORGET ( -- n n ) 1 2 ; 77 throw\" evaluate ; " TEXT+
   s" package Q-FORGET public ' EVAL-X-FORGET catch . cr " TEXT+
   s" : T-FORGET ( -- n n ) X-FORGET ; ;package Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT-RC 70 T=
   OUT$ S\" 77\n\n" T$=
   s" habu: in t-forget: at 'X-FORGET' expected: n n actual: n" SAYS ;

: CHECKED ( -- )
   s" a checked body naming evaluate is refused" T-LABEL
   s" NATIVE-EVAL-OPEN ( ptr u8 n -- ) evaluate" CHECK! 0 T=
   s" a checked body naming evaluate-closed certifies" T-LABEL
   s" NATIVE-EVAL-CLOSED ( ptr u8 n -- ) evaluate-closed" CHECK! -1 T= ;

: RUN ( -- )
   T-RESET
   DEFINES RESIDUE FLOOR REFUSED DEPTH0 NESTED FLOOR-POOL FLOOR-POOL-THROW XT-UNDER
   UNFINISHED
   UNFINISHED-NESTED CALLER-OPEN CALLER-OPEN-REFUSED CALLER-OPEN-IS-REFUSED AGREEMENT OVERFLOW
   TOP-LEVEL
   UNFINISHED-LINE IN-TASK SET-TIER-PAST
   NAME-THROWN ROW-THROWN
   CHECKED
   T-REPORT ;

' RUN
;package
execute
