\ cast-negative-suite.f - reject contract for the CAST: checked retype declarer.
\ Run BY THE ENGINE over stdin, like test/deftype-suite.f:
\     bin/hb < test/cast-negative-suite.f
\ Registered as a positive gate case in test/candidate-validation.f: it asserts
\ every reject in-process and prints ok, so the process exits 0 with clean stderr.
\
\ Each illegal cast is rejected by its NAMED reject:
\   - E-CAST-ARITY : more than one input term, or more than one output term
\   - E-CAST-CLASS : in/out is not a single retype-eligible machine cell (a
\                    pointer is a class/width reinterpret, not a cell retype)
\   - E-CAST-FAM   : in/out names an undeclared family
\   - E-CAST-OWNER : scalar-cell family output is outside its declaring package
\   - verdict 0    : a net-stack-effect body, or a body that drops the input —
\                    both fail the ( in -- in ) identity certification
\   - verdict 0    : the cast: declarer used inside a checked body (unsafe token)
\ Legality violations throw their named code (7129-7131, 7135); body-certification
\ failures surface as verdict 0. A failure prints F<index>; REPORT exits 1 on any.

require test/checker-assert.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

\ silence expected rejection diagnostics (verdicts are asserted, not printed).
create CNDIAG-BUF 8192 allot
CNDIAG-BUF 8192 DIAG-BUFFER!

package CN
public
NEWTYPE cnfam 0
NEWTYPE cncell 1
;package

\ Arm the one-shot cast window with the declaration name, then certify the
\ declaration as a candidate. A legality violation throws its named code (caught
\ here); a body-certification failure returns verdict 0. Result: an E-CAST-* code,
\ or the verdict. The window is single-shot, cleared by CHECK on every match.
variable CN-A
variable CN-U
variable CN-VERD
: CN-RUN ( -- ) CN-A @ CN-U @ CHECK-QUIET-CANDIDATE! CN-VERD ! ;
: CN-CAST ( ptr u8 n ptr u8 n -- n )   \ ( arm-name decl-source -- result )
   CN-U ! CN-A !
   CAST-PEND!
   0 CN-VERD !
   [: CN-RUN ;] catch
   dup 0 <> if exit then
   drop CN-VERD @ ;

\ arity: more than one input, or more than one output term.
s" CNA1"  s" CNA1 ( n n -- CN:cnfam )"              CN-CAST E-CAST-ARITY T=
s" CNA2"  s" CNA2 ( n -- CN:cnfam CN:cnfam )"       CN-CAST E-CAST-ARITY T=
\ class: a pointer is a reinterpret, not a single-cell retype — either side.
s" CNC1"  s" CNC1 ( n -- ptr a )"        CN-CAST E-CAST-CLASS T=
s" CNC2"  s" CNC2 ( ptr a -- n )"        CN-CAST E-CAST-CLASS T=
\ undeclared family in the signature.
s" CNF1"  s" CNF1 ( n -- neverdecl )"    CN-CAST E-CAST-FAM T=
\ introduction into an arity-0 or parametric cell family belongs to its
\ declaring package. Same-owner introduction works; another package cannot mint
\ either shape, while projections out remain unrestricted.
package CN
s" CNO0"  s" CNO0 ( n -- CN:cnfam )"                 CN-CAST -1 T=
s" CNG0"  s" CNG0 ( n -- CN:cncell<n> )"             CN-CAST -1 T=
;package
package CN-HIR
s" CNO1"  s" CNO1 ( n -- CN:cnfam )"                 CN-CAST E-CAST-OWNER T=
s" CNG1"  s" CNG1 ( n -- CN:cncell<n> )"             CN-CAST E-CAST-OWNER T=
s" CNP1"  s" CNP1 ( CN:cnfam -- n )"                 CN-CAST -1 T=
s" CNP2"  s" CNP2 ( CN:cncell<n> -- n )"             CN-CAST -1 T=
;package
\ CHECKER-PACKAGE is a callable parser mirror, not package authority. Spoofing
\ its supported mutator while the engine remains in the global wordlist rejects.
s" CN" CHECKER-PACKAGE
s" CNSP1" s" CNSP1 ( n -- CN:cnfam )"                 CN-CAST E-CAST-OWNER T=
CHECKER-END-PACKAGE
\ Direct mutation of every name/mode mirror cell rejects for the same reason.
99 CHECKER-PACKAGE-NAME c!
110 CHECKER-PACKAGE-NAME 1 + c!
2 CHECKER-PACKAGE-U !
CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE !
s" CNSP2" s" CNSP2 ( n -- CN:cnfam )"                 CN-CAST E-CAST-OWNER T=
CHECKER-END-PACKAGE
\ net-stack-effect body: the ( in -- in ) identity certification rejects.
package CN
s" CNS1"  s" CNS1 ( n -- CN:cnfam ) dup"             CN-CAST 0 T=
\ a body that drops the input: identity certification rejects.
s" CND1"  s" CND1 ( n -- CN:cnfam ) drop"            CN-CAST 0 T=

\ the cast: declarer used inside a checked body is rejected unsafe (verdict 0);
\ no window is armed — the reject is the bare token, before any name is parsed.
s" CNU1 ( n -- CN:cnfam ) cast:"                      CHECK-QUIET-CANDIDATE! 0 T=
;package

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" cast-negative-suite: failures" 1 die ;
REPORT
