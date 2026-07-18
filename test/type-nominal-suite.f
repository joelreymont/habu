\ type-nominal-suite.f — behavior contract for declarable nominal integer types
\ (the CT-registry role mechanism: DEFTYPE / built-in roles idx/len/... in
\ src/core/roles.f + src/core/checker.f). Run BY THE ENGINE over stdin, exactly
\ like test/type-decl-suite.f:
\     bin/hb < test/type-nominal-suite.f
\ Registered as a positive gate case in test/candidate-validation.f.
\
\ This suite LOCKS the declared-nominal contract against regression. It proves a
\ user-declared nominal (DEFTYPE) behaves EXACTLY like a built-in CT-ROLE:
\   - same role  vs same role   ACCEPT
\   - one role   vs other role   REJECT (distinctness)
\   - role       vs generic int `n`  REJECT, BOTH directions (no auto-collapse,
\       no `n`-satisfies-nominal) — this MIRRORS the observed built-in role
\       behavior and MISSING.md's non-negotiable invariant. (Foundation A1's dot
\       text says "vs generic-int accept"; the observed built-in semantics and
\       MISSING.md both say REJECT. This suite locks REJECT; the tension is
\       recorded for the orchestrator.)
\   - narrow width (u8) does NOT widen into a role (roles stay strict)
\   - the ONLY boundary crossing is the explicit converter pair (>NAME / NAME>N)
\   - an undeclared nominal name is rejected
\   - a declaration is transactional: a role minted inside a rolled-back checker
\     frame fully unwinds (CTN + name pool rewind)
\   - a declared role survives a snapshot-persist of the growable CT store
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

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
create TNDIAG-BUF 8192 allot
TNDIAG-BUF 8192 DIAG-BUFFER!

\ whitebox shims (dot habu-hb-crash-bare pattern): checker-internal words probed
\ at top level go through named trusted shims, exactly like test/type-decl-suite.f.
TRUSTED: TWX-CTN@ ( -- n ) CTN @ ;
TRUSTED: TWX-CT-FIND ( ptr u8 n -- n ) CT-FIND ;
TRUSTED: TWX-CT-ADD-NOMINAL ( ptr u8 n -- ) CT-ADD-NOMINAL ;
TRUSTED: TWX-RBF-PUSH ( -- ) RBF-PUSH ;
TRUSTED: TWX-RBF-POP ( -- ) RBF-POP ;
TRUSTED: TWX-SNAP-PREP ( -- ) CHECKER-SNAPSHOT-PREPARE ;

variable TN-CT0                        \ CTN watermark for the rollback probe
variable TN-PCODE                      \ declared role code for the persist probe

\ TRUST helper effects that DEMAND a role / a plain int at an input position, so
\ the input-direction of unification is probed as well as the output-direction
\ (an `A -- B` candidate with an empty body flows A into the B output slot).
s" TN-NEED-IDX" s" idx --" TRUST
s" TN-NEED-N"   s" n --"   TRUST

\ ---------------------------------------------------------------------------
\ 1. built-in CT-ROLE matrix (idx, len, n): lock the observed verdicts.
\ ---------------------------------------------------------------------------
s" TN-B-IDX-ID ( idx -- idx )"        CHECK-QUIET-CANDIDATE! -1 T=   \ same role: accept
s" TN-B-IDX-LEN ( idx -- len )"       CHECK-QUIET-CANDIDATE!  0 T=   \ other role: reject
s" TN-B-IDX-N ( idx -- n )"           CHECK-QUIET-CANDIDATE!  0 T=   \ role -> n: reject
s" TN-B-N-IDX ( n -- idx )"           CHECK-QUIET-CANDIDATE!  0 T=   \ n -> role: reject
s" TN-B-N-ID ( n -- n )"              CHECK-QUIET-CANDIDATE! -1 T=   \ plain int: accept
s" TN-B-IDX-NEED ( idx -- ) TN-NEED-IDX"  CHECK-QUIET-CANDIDATE! -1 T=   \ input role: accept
s" TN-B-LEN-NEED ( len -- ) TN-NEED-IDX"  CHECK-QUIET-CANDIDATE!  0 T=   \ input other role: reject
s" TN-B-N-NEED ( n -- ) TN-NEED-IDX"      CHECK-QUIET-CANDIDATE!  0 T=   \ input n into role: reject
s" TN-B-IDX-NEEDN ( idx -- ) TN-NEED-N"   CHECK-QUIET-CANDIDATE!  0 T=   \ input role into n: reject
\ narrowing/widening never applies to a role: u8 does NOT widen into idx.
s" TN-B-U8-IDX ( u8 -- ) TN-NEED-IDX" CHECK-QUIET-CANDIDATE!  0 T=
\ the explicit converter pair is the only boundary crossing.
s" TN-B-TO ( n -- idx ) >IDX"         CHECK-QUIET-CANDIDATE! -1 T=
s" TN-B-FROM ( idx -- n ) IDX>N"      CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ 2. DEFTYPE: two user-declared nominals mint distinct role codes past CC-MAX.
\ ---------------------------------------------------------------------------
TWX-CTN@ CC-MAX < 0 T=                 \ next code is at/past CC-MAX (nothing below it declarable)
DEFTYPE FRAME-IDX
DEFTYPE CAMERA-SERIAL
s" FRAME-IDX" TWX-CT-FIND CC-MAX < 0 T=       \ minted past CC-MAX
s" CAMERA-SERIAL" TWX-CT-FIND CC-MAX < 0 T=
s" FRAME-IDX" TWX-CT-FIND  s" CAMERA-SERIAL" TWX-CT-FIND  = 0 T=   \ distinct codes

\ same declared role vs itself: accept.
s" TN-D-ID ( FRAME-IDX -- FRAME-IDX )"              CHECK-QUIET-CANDIDATE! -1 T=
\ declared role vs a DIFFERENT declared role: reject (distinctness invariant).
s" TN-D-CROSS ( FRAME-IDX -- CAMERA-SERIAL )"       CHECK-QUIET-CANDIDATE!  0 T=
s" TN-D-CROSS2 ( CAMERA-SERIAL -- FRAME-IDX )"      CHECK-QUIET-CANDIDATE!  0 T=
\ declared role vs a BUILT-IN role: reject.
s" TN-D-BUILTIN ( FRAME-IDX -- idx )"               CHECK-QUIET-CANDIDATE!  0 T=
\ declared role vs generic int n: reject, BOTH directions (mirror built-in role).
s" TN-D-N-OUT ( FRAME-IDX -- n )"                   CHECK-QUIET-CANDIDATE!  0 T=
s" TN-D-N-IN ( n -- FRAME-IDX )"                    CHECK-QUIET-CANDIDATE!  0 T=
\ the auto-generated converter pair bridges the boundary explicitly.
s" TN-D-TO ( n -- FRAME-IDX ) >FRAME-IDX"           CHECK-QUIET-CANDIDATE! -1 T=
s" TN-D-FROM ( FRAME-IDX -- n ) FRAME-IDX>N"        CHECK-QUIET-CANDIDATE! -1 T=
\ round-trip through both converters is identity on n.
s" TN-D-RT ( n -- n ) >FRAME-IDX FRAME-IDX>N"       CHECK-QUIET-CANDIDATE! -1 T=
\ a converter for ONE role does not launder into ANOTHER role.
s" TN-D-XLAUNDER ( n -- CAMERA-SERIAL ) >FRAME-IDX" CHECK-QUIET-CANDIDATE!  0 T=

\ ---------------------------------------------------------------------------
\ 3. an undeclared nominal name is rejected as an unknown signature type.
\ ---------------------------------------------------------------------------
s" TN-U-UNDECL ( NEVER-DECLARED -- NEVER-DECLARED )" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ 4. transactionality: a role minted inside a rolled-back checker frame fully
\    unwinds. RBF-PUSH/POP save+restore CTN and the CT string pool (checker.f
\    RBF.CTN/RBF.CTU), so a speculative CT-ADD-NOMINAL never leaks.
\ ---------------------------------------------------------------------------
TWX-CTN@ TN-CT0 !
s" EPHEMERAL-ROLE" TWX-CT-FIND 0 T=          \ not present before
TWX-RBF-PUSH
s" EPHEMERAL-ROLE" TWX-CT-ADD-NOMINAL
TWX-CTN@ TN-CT0 @ 1 + T=                     \ grew by one inside the frame
s" EPHEMERAL-ROLE" TWX-CT-FIND 0 = 0 T=      \ resolves inside the frame
TWX-RBF-POP
TWX-CTN@ TN-CT0 @ T=                          \ CTN rewound on pop
s" EPHEMERAL-ROLE" TWX-CT-FIND 0 T=          \ role gone after rollback
\ interpreter is healthy after the balanced frame: a normal verdict still holds.
s" TN-T-HEALTH ( FRAME-IDX -- FRAME-IDX )"   CHECK-QUIET-CANDIDATE! -1 T=

\ ---------------------------------------------------------------------------
\ 5. persistence: a declared role survives a snapshot-persist of the growable CT
\    store (CT-SNAPSHOT-PERSIST bakes the grown store and rebases name pointers).
\ ---------------------------------------------------------------------------
DEFTYPE PERSIST-ROLE
s" PERSIST-ROLE" TWX-CT-FIND TN-PCODE !
TN-PCODE @ 0 = 0 T=                          \ registered before persist
TWX-SNAP-PREP
s" PERSIST-ROLE" TWX-CT-FIND TN-PCODE @ T=   \ same code survives the persist
s" TN-P-ID ( PERSIST-ROLE -- PERSIST-ROLE )" CHECK-QUIET-CANDIDATE! -1 T=
s" TN-P-N ( PERSIST-ROLE -- n )"             CHECK-QUIET-CANDIDATE!  0 T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-nominal-suite: failures" 1 die ;
REPORT
