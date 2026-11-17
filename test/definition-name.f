\ definition-name.f - the post-seal checker bridge, and a definition no
\ authority can place in a package failing closed with exit status 70 and
\ naming the refused state on fd 2. The suite runs it on HABU_UNDER_TEST.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package DEFINITION-NAME-TEST
private

$800 constant CAP

create OUT CAP allot
create ERR CAP allot

variable OUT-U         \ bytes the last child wrote to fd 1
variable ERR-U         \ bytes the last child wrote to fd 2

: ERR$ ( -- ^u8 n )
   ERR ERR-U @ ;

: OUT$ ( -- ^u8 n )
   OUT OUT-U @ ;

\ The engine under test: the gate's freshly built candidate when it exported
\ one, else the running engine itself (src/lib/engine-candidate.f).
: HB$ ( -- ^u8 n )       ENGINE-CANDIDATE:PATH$ ;

: RUN-SOURCE ( ^u8 n ^u8 n -- n ) {: exe:^ exeu:n src:^ srcu:n :}
   PROC-ARGV-RESET
   exe exeu >LEN src srcu >LEN OUT CAP >LEN ERR CAP >LEN
   PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE
   PCAP-RESULT:>N {: outu:n erru:n rc:n :} outu OUT-U ! erru ERR-U ! rc ;

: PACKAGE-SOURCE$ ( -- ^u8 n )
   s" package BRIDGE public : W ( -- n ) 1 ; ;package BRIDGE:W drop" ;

: PACKAGE-RC ( -- n )
   PACKAGE-SOURCE$ {: src:^ u:n :}
   HB$ src u RUN-SOURCE ;

: MISSING-DEFINITION-NAME ( -- )
   s" a lone colon rejects at the engine reader" T-LABEL
   HB$ s" :" RUN-SOURCE 70 T=
   ERR$ s" error[E-MISSING-NAME]: `:` needs a name" CONTAINS? TTRUE ;

: COMMENT-NAME ( -- )
   s" a colon named by a comment opener rejects at the name" T-LABEL
   HB$ s" : \ ( -- ) ;" RUN-SOURCE 70 T=
   ERR$ s" error[E-MISSING-NAME]: `:` needs a name" CONTAINS? TTRUE
   ERR$ s" a comment, not a name" CONTAINS? TTRUE
   s" a type definer named by a signature opener rejects at the name" T-LABEL
   HB$ s" DEFLINEAR ( -- )" RUN-SOURCE 70 T=
   ERR$ s" error[E-MISSING-NAME]: `DEFLINEAR` needs a name" CONTAINS? TTRUE
   ERR$ s" a signature, not a name" CONTAINS? TTRUE
   s" a typed variable named by a comment opener rejects at the name" T-LABEL
   HB$ s" TYPED-VARIABLE \ n" RUN-SOURCE 70 T=
   ERR$ s" error[E-MISSING-NAME]: `TYPED-VARIABLE` needs a name" CONTAINS? TTRUE
   ERR$ s" a comment, not a name" CONTAINS? TTRUE ;

: KEYWORD-SOURCE$ ( ^u8 n -- ^u8 n ) {: tok:^ toku:n :}
   SB-RESET
   s" package KW-NAME private : " SB-APPEND
   tok toku SB-APPEND
   S\"  ( -- ) ; s\" AFTER\" type ;package" SB-APPEND
   SB$ ;

: KEYWORD-REJECT ( ^u8 n -- ) {: tok:^ toku:n :}
   tok toku KEYWORD-SOURCE$ {: src:^ srcu:n :}
   s" keyword definition rejects at its name" T-LABEL
   HB$ src srcu RUN-SOURCE 70 T=
   s" keyword definition names the constraint" T-LABEL
   SB-RESET s" error[E-RESERVED-DEFINITION]: `" SB-APPEND tok toku SB-APPEND
   s" ` is a keyword" SB-APPEND
   ERR$ SB$ CONTAINS? TTRUE
   s" keyword definition stops before later source" T-LABEL
   OUT$ s" AFTER" CONTAINS? 0= TTRUE ;

: DEFINER-REJECT ( ^u8 n ^u8 n ^u8 n -- )
   {: src:^ srcu:n msg:^ msgu:n at:^ atu:n :}
   s" a definer's own keyword or number name rejects at the name" T-LABEL
   HB$ src srcu RUN-SOURCE 70 T=
   ERR$ msg msgu CONTAINS? TTRUE
   ERR$ at atu CONTAINS? TTRUE
   OUT$ s" AFTER" CONTAINS? 0= TTRUE ;

: KEYWORD-DEFINITION ( -- )
   \ One keyword of each kind a definer rejects: a body's, the top level's,
   \ the body's and locals list's ends, and a top-level parser.
   s" if" KEYWORD-REJECT         s" global" KEYWORD-REJECT
   s" ;" KEYWORD-REJECT          s" :}" KEYWORD-REJECT
   s" char" KEYWORD-REJECT
   \ Definers that make a type or a storage word judge their own name.
   S\" ARENA if n\ns\" AFTER\" type" s" error[E-RESERVED-DEFINITION]: `if`"
      s" <input>:1:7" DEFINER-REJECT
   S\" DEFLINEAR if\ns\" AFTER\" type" s" error[E-RESERVED-DEFINITION]: `if`"
      s" <input>:1:11" DEFINER-REJECT
   S\" VALUE-RECORD if END-VALUE-RECORD\ns\" AFTER\" type"
      s" error[E-RESERVED-DEFINITION]: `if`" s" <input>:1:14" DEFINER-REJECT
   S\" HEAP 42 n\ns\" AFTER\" type" s" error[E-NUMERIC-DEFINITION]: `42`"
      s" <input>:1:6" DEFINER-REJECT
   S\" NEWTYPE if n\ns\" AFTER\" type" s" error[E-RESERVED-DEFINITION]: `if`"
      s" <input>:1:9" DEFINER-REJECT
   S\" SUMTYPE if VARIANT none ;VARIANT ;SUMTYPE\ns\" AFTER\" type"
      s" error[E-RESERVED-DEFINITION]: `if`" s" <input>:1:9" DEFINER-REJECT
   S\" ENUM if a ;ENUM\ns\" AFTER\" type" s" error[E-RESERVED-DEFINITION]: `if`"
      s" <input>:1:6" DEFINER-REJECT
   S\" STRUCTURE if ;STRUCTURE\ns\" AFTER\" type"
      s" error[E-RESERVED-DEFINITION]: `if`" s" <input>:1:11" DEFINER-REJECT
   S\" NEWTYPE 42 n\ns\" AFTER\" type" s" error[E-NUMERIC-DEFINITION]: `42`"
      s" <input>:1:9" DEFINER-REJECT
   \ CASE is the minimal wrong-token reproducer: the diagnostic must include the
   \ guilty spelling itself, not a later body token where syntax finally trips.
   s" keyword definition names CASE" T-LABEL
   HB$ S\" package KW-UPPER private : CASE ( -- ) ; s\" AFTER\" type ;package"
      RUN-SOURCE 70 T=
   ERR$ s" error[E-RESERVED-DEFINITION]: `CASE` is a keyword" CONTAINS? TTRUE
   s" qualified keyword definition rejects at its tail" T-LABEL
   HB$ s" : KW-QUAL:CASE ( -- ) ;" RUN-SOURCE 70 T=
   ERR$ s" error[E-RESERVED-DEFINITION]: `CASE` is a keyword" CONTAINS? TTRUE
   \ FOLD is a normal, non-immediate dictionary word, not engine syntax. Its
   \ same-name private definition must remain callable through ordinary lookup.
   s" ordinary FOLD definition remains callable" T-LABEL
   HB$ s" package KW-FOLD private : FOLD ( n -- n ) 1+ ; : CALL-FOLD ( -- n ) 41 FOLD ; CALL-FOLD . ;package"
      RUN-SOURCE 0 T=
   s" ordinary FOLD definition returns through dictionary lookup" T-LABEL
   OUT$ s" 42" CONTAINS? TTRUE
   s" qualified ordinary FOLD definition remains callable" T-LABEL
   HB$ s" : KW-FOLD-Q:FOLD ( n -- n ) 1+ ; 41 KW-FOLD-Q:FOLD ." RUN-SOURCE 0 T=
   OUT$ s" 42" CONTAINS? TTRUE ;

: POST-SEAL-BRIDGE ( -- )
   s" post-seal package reaches checker bridge" T-LABEL
   PACKAGE-RC 0 T= ;

public

: RUN ( -- )
   T-RESET
   MISSING-DEFINITION-NAME
   COMMENT-NAME
   KEYWORD-DEFINITION
   POST-SEAL-BRIDGE
   T-REPORT
   s" definition-name: ok" type cr ;

;package

DEFINITION-NAME-TEST:RUN
