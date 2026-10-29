\ engine-error-package.f - engine failure ABI: exact exits 86..88, the post-seal
\ checker bridge, and a definition no authority can place in a package failing
\ closed with 70 and naming the refused state on fd 2. Native gates run it on
\ HABU_UNDER_TEST, bootstrap.sh on the Gforth-recovered candidate.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package ENGINE-ERROR-TEST
private

$800 constant CAP
10000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot

variable OUT-U         \ bytes the last child wrote to fd 1
variable ERR-U         \ bytes the last child wrote to fd 2

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

\ The engine under test: the gate's freshly built candidate when it exported
\ one, else the running engine itself (src/lib/engine-candidate.f).
: HB$ ( -- ptr u8 n )       ENGINE-CANDIDATE:PATH$ ;

\ The fault is injected through source: the engine reaches the package registrar
\ through src/habu/layout.f NCOMP-DISPATCH:DECL-CELL, so patching image strings
\ is a no-op.
: SOURCE$ ( ptr u8 n -- ptr u8 n ) {: name:ptr u:n :}
   SB-RESET
   S\" s\" engine-error-test\" " SB-APPEND
   name u SB-APPEND
   s"  die" SB-APPEND
   SB$ ;

: RUN-SOURCE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu:n src:ptr srcu:n :}
   PROC-ARGV-RESET
   exe exeu >LEN src srcu >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   PCAP-RESULT:>N {: outu:n erru:n rc:n :} outu OUT-U ! erru ERR-U ! rc ;

: CHILD-RC ( ptr u8 n -- n )
   SOURCE$ {: src:ptr u:n :}
   HB$ src u RUN-SOURCE ;

: PACKAGE-SOURCE$ ( -- ptr u8 n )
   s" package BRIDGE public : W ( -- n ) 1 ; ;package BRIDGE:W drop" ;

: PACKAGE-RC ( -- n )
   PACKAGE-SOURCE$ {: src:ptr u:n :}
   HB$ src u RUN-SOURCE ;

\ src/core/checker.f CHECKER-RESOLVE:AUTHORITY answers "no package context" when
\ the current wordlist is neither the open package's public nor its private one;
\ CHECKER-PKG-CONTEXT-REJECT names that and throws the reject rc.
: NO-CONTEXT-SOURCE$ ( -- ptr u8 n )
   s" package BRIDGE-GAP public 0 set-current : W ( -- n ) 1 ; ;package" ;

: NO-CONTEXT-RC ( -- n )
   NO-CONTEXT-SOURCE$ {: src:ptr u:n :}
   HB$ src u RUN-SOURCE ;

\ Inside a nested INCLUDE-EVALUATE: with TRY's catch the reject arrives as 70;
\ without it the throw leaves the evaluate and the process exits 70. BARE
\ restores the package's wordlist itself.
: BARE-PREFIX ( ptr u8 n -- ) {: pkg:ptr pkgu:n :}
   SB-RESET
   s" package " SB-APPEND
   pkg pkgu SB-APPEND
   S\"  public variable CUR get-current CUR !\n" SB-APPEND
   S\" : SRC ( -- ptr u8 n ) s\q : W ( -- n ) 1 ;\q ;\n" SB-APPEND ;

: CAUGHT-SOURCE$ ( -- ptr u8 n )
   s" BRIDGE-CATCH" BARE-PREFIX
   S\" : TRY ( -- n ) [: SRC INCLUDE-EVALUATE ;] catch ;\n" SB-APPEND
   S\" : BARE ( -- n ) 0 set-current TRY CUR @ set-current ;\n" SB-APPEND
   S\" s\q caught=\q type BARE .\n" SB-APPEND
   S\" s\q AFTER\q type\n;package\n" SB-APPEND
   SB$ ;

: UNCAUGHT-SOURCE$ ( -- ptr u8 n )
   s" BRIDGE-RAW" BARE-PREFIX
   S\" : BARE ( -- ) 0 set-current SRC INCLUDE-EVALUATE CUR @ set-current ;\n" SB-APPEND
   S\" BARE\n" SB-APPEND
   S\" s\q AFTER\q type\n;package\n" SB-APPEND
   SB$ ;

: MISSING-DEFINITION-NAME ( -- )
   s" a lone colon rejects at the engine reader" T-LABEL
   HB$ s" :" RUN-SOURCE $4A T=
   ERR$ s" missing definition name" CONTAINS? TTRUE ;

: EXACT-EXITS ( -- )
   s" callable ABI exits 86" T-LABEL
   s" ENGINE-ERROR:CALLABLE-ABI" CHILD-RC 86 T=
   s" CATCH stack exits 87" T-LABEL
   s" ENGINE-ERROR:CATCH-STACK" CHILD-RC 87 T=
   s" code certificate exits 88" T-LABEL
   s" ENGINE-ERROR:CODE-CERT" CHILD-RC 88 T= ;

: COMPILE-KEYWORD-SOURCE$ ( ptr u8 n -- ptr u8 n ) {: tok:ptr toku:n :}
   SB-RESET
   s" package KW-NAME private : " SB-APPEND
   tok toku SB-APPEND
   S\"  ( -- ) ; s\" AFTER\" type ;package" SB-APPEND
   SB$ ;

: COMPILE-KEYWORD-REJECT ( ptr u8 n -- ) {: tok:ptr toku:n :}
   tok toku COMPILE-KEYWORD-SOURCE$ {: src:ptr srcu:n :}
   s" compile keyword definition rejects at its name" T-LABEL
   HB$ src srcu RUN-SOURCE 70 T=
   s" compile keyword definition names the constraint" T-LABEL
   ERR$ s" compile keyword cannot be a definition name: " CONTAINS? TTRUE
   s" compile keyword definition stops before later source" T-LABEL
   OUT$ s" AFTER" CONTAINS? 0= TTRUE ;

: COMPILE-KEYWORD-DEFINITION ( -- )
   \ The complete control and loop rows consumed before dictionary lookup: a
   \ same-name definition (EXIT, RECURSE, UNLOOP, {:) could compile rc 0 and run
   \ the syntax instead of the definition.
   s" if" COMPILE-KEYWORD-REJECT       s" then" COMPILE-KEYWORD-REJECT
   s" else" COMPILE-KEYWORD-REJECT     s" begin" COMPILE-KEYWORD-REJECT
   s" until" COMPILE-KEYWORD-REJECT    s" again" COMPILE-KEYWORD-REJECT
   s" while" COMPILE-KEYWORD-REJECT    s" repeat" COMPILE-KEYWORD-REJECT
   s" case" COMPILE-KEYWORD-REJECT     s" of" COMPILE-KEYWORD-REJECT
   s" endof" COMPILE-KEYWORD-REJECT    s" endcase" COMPILE-KEYWORD-REJECT
   s" construct" COMPILE-KEYWORD-REJECT s" match" COMPILE-KEYWORD-REJECT
   s" do" COMPILE-KEYWORD-REJECT       s" loop" COMPILE-KEYWORD-REJECT
   s" i" COMPILE-KEYWORD-REJECT        s" >r" COMPILE-KEYWORD-REJECT
   s" r>" COMPILE-KEYWORD-REJECT       s" r@" COMPILE-KEYWORD-REJECT
   s" exit" COMPILE-KEYWORD-REJECT     s" recurse" COMPILE-KEYWORD-REJECT
   s" ?do" COMPILE-KEYWORD-REJECT      s" +loop" COMPILE-KEYWORD-REJECT
   s" j" COMPILE-KEYWORD-REJECT        s" leave" COMPILE-KEYWORD-REJECT
   s" unloop" COMPILE-KEYWORD-REJECT   s" {:" COMPILE-KEYWORD-REJECT
   \ CASE is the minimal wrong-token reproducer: the diagnostic must include the
   \ guilty spelling itself, not a later body token where syntax finally trips.
   s" compile keyword definition names CASE" T-LABEL
   HB$ S\" package KW-UPPER private : CASE ( -- ) ; s\" AFTER\" type ;package"
      RUN-SOURCE 70 T=
   ERR$ s" compile keyword cannot be a definition name: CASE" CONTAINS? TTRUE
   s" qualified compile keyword definition rejects at its tail" T-LABEL
   HB$ s" : KW-QUAL:CASE ( -- ) ;" RUN-SOURCE 70 T=
   ERR$ s" compile keyword cannot be a definition name: CASE" CONTAINS? TTRUE
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
   PACKAGE-RC 0 T=
   s" checking-off permits a package reload with no checker scope" T-LABEL
   HB$ s" 0 set-check package CHECK-OFF public : W ( -- n ) 1 ; ;package" RUN-SOURCE 0 T=
   s" a definition with no package authority fails closed" T-LABEL
   NO-CONTEXT-RC 70 T=
   \ 70 is also what an undefined word or a rejected body exits with, so fd 2
   \ must name the state the checker refused on.
   s" a definition with no package authority names the refused state" T-LABEL
   ERR$ s" no authenticated package context" CONTAINS? TTRUE
   CAUGHT-SOURCE$ {: csrc:ptr csrcu:n :}
   s" a nested no-context definition under catch is caught as 70" T-LABEL
   HB$ csrc csrcu RUN-SOURCE 0 T=
   ERR$ s" no authenticated package context" CONTAINS? TTRUE
   s" the catch prints 70 and the source after it runs" T-LABEL
   OUT$ S\" caught=70\nAFTER" T$=
   UNCAUGHT-SOURCE$ {: usrc:ptr usrcu:n :}
   s" a nested no-context definition with no catch exits 70" T-LABEL
   HB$ usrc usrcu RUN-SOURCE 70 T=
   ERR$ s" no authenticated package context" CONTAINS? TTRUE
   OUT$ s" AFTER" CONTAINS? 0= TTRUE ;

public

: RUN ( -- )
   T-RESET
   EXACT-EXITS
   MISSING-DEFINITION-NAME
   COMPILE-KEYWORD-DEFINITION
   POST-SEAL-BRIDGE
   T-REPORT
   s" engine-error-package: ok" type cr ;

;package

ENGINE-ERROR-TEST:RUN
