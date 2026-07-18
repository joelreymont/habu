\ value-nominal-suite.f - behavior contract for the NOMINAL: declaration surface
\ (lib/type/value-nominal.f: package-scoped arity-0 type-family nominals + generated
\ converters). Run BY THE ENGINE over stdin, like test/type-nominal-suite.f:
\     bin/hb < test/value-nominal-suite.f
\ Registered as a positive gate case in test/candidate-validation.f.
\
\ This suite LOCKS the value-nominal contract against regression. It proves a
\ `NOMINAL: NAME` type behaves EXACTLY like the built-in CT-roles the older
\ test/type-nominal-suite.f pins, PLUS the package scoping the CT-role table cannot
\ give:
\   - same nominal   vs same nominal   ACCEPT
\   - one nominal    vs other nominal  REJECT (distinctness)
\   - nominal        vs generic int n  REJECT, BOTH directions (no auto-collapse,
\       no n-satisfies-nominal) - MISSING.md's non-negotiable invariant
\   - the ONLY boundary crossing is the generated converter pair (>NAME / NAME>N)
\   - a converter for ONE nominal never launders into another nominal
\   - a demanded nominal INPUT rejects a plain n (input-direction unification)
\   - an undeclared nominal name in a signature is rejected
\   - PACKAGE SCOPING: `NOMINAL: SERIAL` in package CAMERA and the same line in
\       package FRAME are two DISTINCT types (the ergonomic prize the global CT
\       table cannot deliver: a second same-named DEFTYPE dies exit 70)
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

require lib/type/value-nominal.f
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
create VNDIAG-BUF 8192 allot
VNDIAG-BUF 8192 DIAG-BUFFER!

\ ---------------------------------------------------------------------------
\ 1. a declared value nominal, checked inside its own package.
\ ---------------------------------------------------------------------------
package CAMERA
NOMINAL: SERIAL
NOMINAL: FRAME-INDEX

\ a package word that DEMANDS a nominal at an input position, so the input
\ direction of unification is probed, not only the output direction.
: NEED-SERIAL ( serial -- ) SERIAL>N drop ;

\ same nominal vs itself: accept.
s" VN-ID ( serial -- serial )"              CHECK-QUIET-CANDIDATE! -1 T=
\ nominal vs a DIFFERENT nominal: reject (distinctness), both directions.
s" VN-CROSS ( serial -- frame-index )"      CHECK-QUIET-CANDIDATE!  0 T=
s" VN-CROSS2 ( frame-index -- serial )"     CHECK-QUIET-CANDIDATE!  0 T=
\ nominal vs generic int n: reject, BOTH directions (no auto-collapse).
s" VN-N-OUT ( serial -- n )"                CHECK-QUIET-CANDIDATE!  0 T=
s" VN-N-IN ( n -- serial )"                 CHECK-QUIET-CANDIDATE!  0 T=
\ the generated converter pair bridges the boundary explicitly.
s" VN-TO ( n -- serial ) >SERIAL"           CHECK-QUIET-CANDIDATE! -1 T=
s" VN-FROM ( serial -- n ) SERIAL>N"        CHECK-QUIET-CANDIDATE! -1 T=
\ round-trip through both converters is identity on n.
s" VN-RT ( n -- n ) >SERIAL SERIAL>N"       CHECK-QUIET-CANDIDATE! -1 T=
\ a converter for ONE nominal does not launder into ANOTHER nominal.
s" VN-LAUNDER ( n -- frame-index ) >SERIAL" CHECK-QUIET-CANDIDATE!  0 T=
\ demanded nominal INPUT: a plain n at that input rejects; a serial accepts.
s" VN-NEED-OK ( serial -- ) NEED-SERIAL"    CHECK-QUIET-CANDIDATE! -1 T=
s" VN-NEED-N ( n -- ) NEED-SERIAL"          CHECK-QUIET-CANDIDATE!  0 T=
s" VN-NEED-X ( frame-index -- ) NEED-SERIAL" CHECK-QUIET-CANDIDATE! 0 T=
;package

\ ---------------------------------------------------------------------------
\ 2. package scoping: the SAME name in a different package is a DISTINCT type.
\ ---------------------------------------------------------------------------
package FRAME
NOMINAL: SERIAL
\ FRAME's own serial vs itself: accept.
s" VN-F-ID ( serial -- serial )"            CHECK-QUIET-CANDIDATE! -1 T=
\ FRAME's serial vs CAMERA's serial (qualified): distinct, both directions.
s" VN-F-CROSS ( serial -- CAMERA:serial )"  CHECK-QUIET-CANDIDATE!  0 T=
s" VN-F-CROSS2 ( CAMERA:serial -- serial )" CHECK-QUIET-CANDIDATE!  0 T=
;package

\ and the reverse view, from CAMERA: CAMERA's serial vs FRAME's serial: distinct.
package CAMERA
s" VN-C-CROSS ( serial -- FRAME:serial )"   CHECK-QUIET-CANDIDATE!  0 T=
s" VN-C-SAME ( serial -- serial )"          CHECK-QUIET-CANDIDATE! -1 T=
;package

\ ---------------------------------------------------------------------------
\ 3. an undeclared nominal name is rejected as an unknown signature type.
\ ---------------------------------------------------------------------------
s" VN-UNDECL ( never-declared -- never-declared )" CHECK-QUIET-CANDIDATE! 0 T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" value-nominal-suite: failures" 1 die ;
REPORT
