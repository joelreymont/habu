\ Resolve the same word at the checker and native tick boundaries.
require lib/test.f
require lib/test/subject.f

package C2-UNIQUE-POLICY
private
$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: STATUS? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

public
: RUN ( -- )
   T-RESET
   s" interpret tick cannot turn a trusted-only word into an xt" T-LABEL
   s" ' FFI-PTR>CELL drop" 70 STATUS? TTRUE
   s" compiled tick cannot turn a trusted-only word into an xt" T-LABEL
   s" : BAD ( -- ) ['] FFI-PTR>CELL drop ;" 70 STATUS? TTRUE
   s" exporting a trusted-only word cannot erase its policy" T-LABEL
   s" package C2-POLICY-ALIAS public EXPORT FFI-PTR>CELL ;package" 67 STATUS? TTRUE
   s" replacing the checker query cannot grant a trusted-only tick" T-LABEL
   s" undefine CHECKER-TRUSTED-TICK? : CHECKER-TRUSTED-TICK? ( ptr u8 n -- bool ) 2drop false ; ' FFI-PTR>CELL drop" 70 STATUS? TTRUE
   s" replacing the checker query cannot grant a compiled tick" T-LABEL
   s" undefine CHECKER-TRUSTED-TICK? : CHECKER-TRUSTED-TICK? ( ptr u8 n -- bool ) 2drop false ; : BAD ( -- ) ['] FFI-PTR>CELL drop ;" 70 STATUS? TTRUE
   s" a package word with the same spelling keeps its own policy" T-LABEL
   s" package C2-POLICY-SHADOW public : FFI-PTR>CELL ( -- n ) 7 ; : GOOD ( -- ) ['] FFI-PTR>CELL drop ; ;package" 0 STATUS? TTRUE
   T-REPORT
   s" c2-unique-policy: ok" type cr ;
;package

C2-UNIQUE-POLICY:RUN
