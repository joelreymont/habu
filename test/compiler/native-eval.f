split-str
\ The closed evaluation boundary: `evaluate-closed` runs source on a data
\ stack of its own and rejects residue with a record, which is what lets a
\ checked word evaluate source.
require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/subject.f

package NATIVE-EVAL-TEST

public

\ The nested cases' inner texts. A text cannot spell a string literal inside
\ its own literal, so it reaches an inner text through a word, and the word is
\ public because the texts run after this package has closed.
: INNER-DEPTH$ ( -- ^u8 n ) s" depth 0 T=" ;
: INNER-RESIDUE$ ( -- ^u8 n ) s" 1 2" ;

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
: INNER-UNDER$ ( -- ^u8 n ) s" 1 ' NATIVE-EVAL-TEST:W execute" ;

: INNER-OPEN$ ( -- ^u8 n ) s" : NATIVE-EVAL-E ( -- n ) 1" ;

: INNER-THROW$ ( -- ^u8 n ) s" depth 0 T= 7 throw" ;

private

: DEFINES ( -- )
   s" a definitions text loads" T-LABEL
   s" : NATIVE-EVAL-ANSWER ( -- n ) 42 ;" evaluate-closed
   s" NATIVE-EVAL-ANSWER 42 T=" evaluate-closed ;

: RESIDUE ( -- )
   s" a text that leaves cells throws 70" T-LABEL
   [: s" 1 2" evaluate-closed ;] 70 TTHROWSQ ;

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
   s" an inner text's residue throws 70 through the outer text" T-LABEL
   [: s" NATIVE-EVAL-TEST:INNER-RESIDUE$ evaluate-closed" evaluate-closed ;]
   70 TTHROWSQ ;

: OUTER-THROW$ ( -- str )
   s" 5 depth 1 T= NATIVE-EVAL-TEST:INNER-THROW$ evaluate-closed" ;

: NESTED-THROW ( -- n n n )
   7 8 [: OUTER-THROW$ evaluate-closed ;] catch ;

\ A throw out of a nested pair: the caught delivery runs compiled code, the
\ checker's package resync, before the handler's extent is restored, so that
\ code must not run on a stack the pool now holds.
: FLOOR-THROW ( -- )
   s" a nested pair's throw reaches the caller, each text at its own floor" T-LABEL
   NESTED-THROW 7 T=
   s" and the throw leaves the caller's cells" T-LABEL
   8 T= 7 T= ;

\ A throw out of a text caught in a loop's body leaves the caller's loops
\ alone, though the delivery's package resync runs a loop of its own.
: CATCH-71 ( -- n ) [: s" 71 throw" evaluate-closed ;] catch ;

: LOOP-CATCH ( -- n ) 0 4 0 ?do CATCH-71 71 T= 1 + loop ;

: LOOP-CATCH-NESTED ( -- n )
   0 8 5 ?do 7 3 ?do CATCH-71 71 T= j 10 * i + + loop loop ;

: LOOP-THROW ( -- )
   s" a loop whose body catches a text's throw runs every pass" T-LABEL
   LOOP-CATCH 4 T=
   s" and an inner loop keeps the outer index" T-LABEL
   LOOP-CATCH-NESTED 774 T= ;

\ Each consumer of an xt reaches under the floor the same way, and each answer
\ is the same throw E-UNDERFLOW with the caller's 5 intact and W unfinished.
: XT-EXECUTE ( -- n )
   5 [: s" 1 ' NATIVE-EVAL-TEST:W execute" evaluate-closed ;] catch E-UNDERFLOW T= ;

: XT-PRIMITIVE ( -- n )
   5 [: s" 1 NATIVE-EVAL-TEST:PLUS-XT execute" evaluate-closed ;] catch E-UNDERFLOW T= ;

: XT-FINALLY ( -- n )
   5 [: s" 1 ' NATIVE-EVAL-TEST:W NATIVE-EVAL-TEST:CLEAN finally" evaluate-closed ;]
   catch E-UNDERFLOW T= ;

: XT-QUOTATION ( -- n )
   5 [: s" 1 NATIVE-EVAL-TEST:GETW execute" evaluate-closed ;] catch E-UNDERFLOW T= ;

: XT-NESTED ( -- n )
   5 [: s" 3 NATIVE-EVAL-TEST:INNER-UNDER$ evaluate-closed" evaluate-closed ;]
   catch E-UNDERFLOW T= ;

: XT-UNDER ( -- )
   0 SEEN !
   s" a compiled word executed below the floor throws E-UNDERFLOW" T-LABEL
   XT-EXECUTE
   s" and leaves the caller's cell, the word unfinished" T-LABEL
   5 T=  SEEN @ 0 T=
   s" a primitive executed below the floor throws E-UNDERFLOW" T-LABEL
   XT-PRIMITIVE 5 T=
   s" a catch inside the text receives that E-UNDERFLOW itself" T-LABEL
   5 s" 1 ' NATIVE-EVAL-TEST:W catch E-UNDERFLOW T= drop" evaluate-closed
   5 T=  SEEN @ 0 T=
   s" finally's body reaching below the floor throws E-UNDERFLOW" T-LABEL
   XT-FINALLY 5 T=  SEEN @ 0 T=
   s" a quotation xt reaching below the floor throws E-UNDERFLOW" T-LABEL
   XT-QUOTATION 5 T=  SEEN @ 0 T=
   s" an inner text's reach throws E-UNDERFLOW through the outer text" T-LABEL
   XT-NESTED 5 T=  SEEN @ 0 T= ;

\ A text that ends inside a definition it opened is refused as every buffer
\ is, and the open definition goes with it: the code it emitted is taken back
\ (a pending definition has no counted record, so ndict@ cannot show it), the
\ next text is interpreted rather than compiled into it, and its name resolves
\ to nothing until it is defined again. The interpretation probe runs before
\ any other text because a throw out of a text resets the compile state on
\ its own.
: UNFIN ( -- n )
   9 [: s" : NATIVE-EVAL-D ( -- n ) 42" evaluate-closed ;] catch
   70 T= ;

: UNFIN-NESTED ( -- n )
   9 [: s" NATIVE-EVAL-TEST:INNER-OPEN$ evaluate-closed" evaluate-closed ;] catch
   70 T= ;

: UNFINISHED ( -- )
   s" a text that ends inside a definition is refused, rc 70" T-LABEL
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

\ A definer that runs a text while its definition is open: the text compiles
\ into that definition and ends with it still open, which is not a definition
\ the text opened, so it is not refused.
: BUMP ( -- ) SEEN @ 1 + SEEN ! ;
: MAKE-BUMPED ( str -- ) EFFECT:EMPTY define-word s" BUMP" evaluate-closed postpone ; ;
s" BUMPED" MAKE-BUMPED

: CALLER-OPEN ( -- )
   s" a text run inside the caller's open definition compiles into it" T-LABEL
   0 SEEN !  BUMPED  SEEN @ 1 T= ;

\ : NAME ( -- n ) 5 ; after a text whose token the compile refuses: the
\ refusal, caught inside the definer, leaves its definition compiling, as it
\ does for any buffer begun inside one (habu2.f EM-EVAL-THROW-RECOVER).
variable CAUGHT
: MAKE-REFUSED ( ^u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   [: s" NATIVE-EVAL-NO-WORD" evaluate-closed ;] catch CAUGHT !
   s" 5" evaluate-closed postpone ; ;

: CALLER-OPEN-REFUSED ( -- )
   s" a text refused inside the caller's open definition is caught there" T-LABEL
   0 CAUGHT !
   S\" package NATIVE-EVAL-TEST public s\" HOST\" MAKE-REFUSED ;package" evaluate-closed
   CAUGHT @ 70 T=
   s" and the caller's definition goes on compiling" T-LABEL
   s" NATIVE-EVAL-TEST:HOST 5 T=" evaluate-closed ;

\ `is` and its target are captured in two passes, so a refused target takes
\ back `is` too: a definition left holding `is` refuses at its `;`.
: MAKE-REFUSED-IS ( ^u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word
   [: s" is NATIVE-EVAL-NO-DEFER" evaluate-closed ;] catch CAUGHT !
   s" 5" evaluate-closed postpone ; ;

: CALLER-OPEN-IS-REFUSED ( -- )
   s" a refused `is` target is taken back with its `is`" T-LABEL
   0 CAUGHT !
   S\" package NATIVE-EVAL-TEST public s\" HOST-IS\" MAKE-REFUSED-IS ;package" evaluate-closed
   CAUGHT @ 70 T=
   s" NATIVE-EVAL-TEST:HOST-IS 5 T=" evaluate-closed ;

$1000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ RATCHET grows the stack one cell per level (recurse consumes the declared
\ input and nothing pops it), so the text pushes until a push crosses the top
\ of its stack's mapping.
: OVERFLOW$ ( -- str )
   S\" : RATCHET ( n -- ) begin dup recurse again ; s\" 1 RATCHET\" evaluate-closed" ;

: OVERFLOW ( -- )
   s" an overflow inside a closed text exits STACK-BOUNDS" T-LABEL
   OVERFLOW$ OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   PROCESS:ENDING>RC RC>N 102 T=
   nip LEN>N {: erru:n :}
   s" and names the data stack" T-LABEL
   ERR erru S\" error[E-STACK-BOUNDS]: the data stack ran past its bounds\n"
   STARTS-WITH? TTRUE ;

\ RESIDUE's text with no catch around it.
: RESIDUE-RECORD ( -- )
   s" an uncaught text that leaves cells exits 70" T-LABEL
   S\" s\" 1 2\" evaluate-closed" OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   PROCESS:ENDING>RC RC>N 70 T=
   nip LEN>N {: erru:n :}
   s" and its record counts the cells it left" T-LABEL
   ERR erru s" error[E-EVAL-RESIDUE]" CONTAINS? TTRUE
   ERR erru s" = cells: 2" CONTAINS? TTRUE ;

\ The same reach outside any closed text: the child's text runs a compiled
\ word through execute with one cell on the stack it was given.
: TOP-LEVEL$ ( -- str )
   s" TRUSTED: NATIVE-EVAL-W ( n -- n ) + ; 1 ' NATIVE-EVAL-W execute" ;

: TOP-LEVEL ( -- )
   s" a word reaching below the base outside a closed text exits 67 uncaught" T-LABEL
   TOP-LEVEL$ OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   PROCESS:ENDING>RC RC>N 67 T=
   nip LEN>N {: erru:n :}
   s" and the uncaught throw is E-UNDERFLOW's -9851" T-LABEL
   ERR erru E-UNDERFLOW TEST-EVAL:THROWN-HEAD$ CONTAINS? TTRUE
   s" and is no stack bounds exit" T-LABEL
   ERR erru s" E-STACK-BOUNDS" CONTAINS? TFALSE ;

\ UNFINISHED's text with no catch around it. The child inherits this
\ process's dictionary, which UNFINISHED left holding NATIVE-EVAL-D.
: UNFINISHED-LINE$ ( -- str )
   S\" s\" : NATIVE-EVAL-F ( -- n ) 42\" evaluate-closed" ;

: UNFINISHED-LINE ( -- )
   s" an uncaught unfinished text exits 70" T-LABEL
   UNFINISHED-LINE$ OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   PROCESS:ENDING>RC RC>N 70 T=
   nip LEN>N {: erru:n :}
   s" and names the definition and where the text ran" T-LABEL
   ERR erru s" error[E-VS-UNTERMINATED-DEFINITION]: " CONTAINS? TTRUE
   ERR erru s" the definition of `NATIVE-EVAL-F`" CONTAINS? TTRUE
   ERR erru s" = evaluated: : NATIVE-EVAL-F ( -- n ) 42" CONTAINS? TTRUE
   ERR erru s" never closed" CONTAINS? TTRUE ;

\ ---- a definition a thrown text takes back keeps no row ----------------------
\ A text that throws takes back what it defined, and its frame cuts the
\ checker's records back to where the text began, so a later word is checked
\ against the word the engine binds. The texts outrun a source line, so each
\ is assembled in TEXT.
create TEXT IO-CAP allot
variable TEXT-U
variable OUT-U
variable ERR-U

: TEXT+ ( ^u8 n -- ) TEXT IO-CAP TEXT-U BUF-APPEND ;

: TEXT-RC ( -- n )
   TEXT TEXT-U BUF-LEN@ OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   PROCESS:ENDING>RC RC>N {: rc:n :}
   LEN>N ERR-U !  LEN>N OUT-U !  rc ;

: OUT$ ( -- ^u8 n ) OUT OUT-U @ ;

: SAYS ( ^u8 n -- ) {: a:^ u:n :}
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
   s" error[E-MISMATCH]: T-FORGET ends with n on the stack; its signature promises n n" SAYS
   s" = evaluated: ...FORGET catch . cr : T-FORGET ( -- n n ) X-FORGET ;" SAYS
   s" the stack holds n after this" SAYS ;

: CHECKED ( -- )
   s" a checked body naming evaluate is refused" T-LABEL
   s" NATIVE-EVAL-OPEN ( ^u8 n -- ) evaluate" CHECK! 0 T=
   s" a checked body naming evaluate-closed certifies" T-LABEL
   s" NATIVE-EVAL-CLOSED ( ^u8 n -- ) evaluate-closed" CHECK! -1 T= ;

: RUN ( -- )
   T-RESET
   DEFINES RESIDUE FLOOR REFUSED DEPTH0 NESTED FLOOR-THROW
   LOOP-THROW XT-UNDER
   UNFINISHED
   UNFINISHED-NESTED CALLER-OPEN CALLER-OPEN-REFUSED CALLER-OPEN-IS-REFUSED OVERFLOW
   RESIDUE-RECORD TOP-LEVEL
   UNFINISHED-LINE
   NAME-THROWN ROW-THROWN
   CHECKED
   T-REPORT ;

' RUN
;package
execute
