\ Resolve the same word at the checker and native tick boundaries.
\ ffi-call-bounded keeps a global trusted-only row (src/habu/prims.f) beside its
\ FFI-private one; a primitive whose only row is owner-private is
\ test/prim-owner-scope.f's subject.
require src/lib/test.f
require src/lib/test/subject.f

package C2-UNIQUE-POLICY
public
: RUN ( -- )
   T-RESET
   s" interpret tick cannot turn a trusted-only word into an xt" T-LABEL
   s" ' ffi-call-bounded drop" 70 SUBJECT:EXITED? TTRUE
   s" compiled tick cannot turn a trusted-only word into an xt" T-LABEL
   s" : BAD ( -- ) ['] ffi-call-bounded drop ;" 70 SUBJECT:EXITED? TTRUE
   s" exporting a trusted-only word cannot erase its policy" T-LABEL
   s" package C2-POLICY-ALIAS public EXPORT ffi-call-bounded ;package" 67 SUBJECT:EXITED? TTRUE
   s" replacing the checker query cannot grant a trusted-only tick" T-LABEL
   s" undefine CHECKER-TRUSTED-TICK? : CHECKER-TRUSTED-TICK? ( ptr u8 n -- bool ) 2drop false ; ' ffi-call-bounded drop" 70 SUBJECT:EXITED? TTRUE
   s" replacing the checker query cannot grant a compiled tick" T-LABEL
   s" undefine CHECKER-TRUSTED-TICK? : CHECKER-TRUSTED-TICK? ( ptr u8 n -- bool ) 2drop false ; : BAD ( -- ) ['] ffi-call-bounded drop ;" 70 SUBJECT:EXITED? TTRUE
   s" a package word with the same spelling keeps its own policy" T-LABEL
   s" package C2-POLICY-SHADOW public : ffi-call-bounded ( -- n ) 7 ; : GOOD ( -- ) ['] ffi-call-bounded drop ; ;package" 0 SUBJECT:EXITED? TTRUE
   T-REPORT
   s" c2-unique-policy: ok" type cr ;
;package

C2-UNIQUE-POLICY:RUN
