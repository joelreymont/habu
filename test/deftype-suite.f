\ deftype-suite.f - the DEFTYPE declaration surface (src/lib/type/deftype.f:
\ package-scoped arity-0 type-family nominals and generated converters).
\ WHITEBOX-SUITE row: it reopens VNOM, which the arm64 engine seals with T0, so
\ the gate runs it on the unsealed engine test/whitebox-engine.f builds.
\ The converter pair (>NAME / NAME>N) is the only boundary crossing; the same
\ name in two packages is two types. T-REPORT exits 1 on any failure.

require src/lib/type/deftype.f
require src/lib/test.f

T-RESET

\ silence expected rejection diagnostics (verdicts are asserted, not printed).
create VNDIAG-BUF 8192 allot
VNDIAG-BUF 8192 DIAG-BUFFER!

\ Whitebox shim: the checker-internal snapshot bake through a named checked
\ shim, as test/type-family-suite.f does.
: TWX-SNAP-PREP ( -- ) CHECKER-CAPTURE-PREPARE ;

\ 1. a declared value nominal, checked inside its own package.
package CAMERA
DEFTYPE SERIAL
DEFTYPE FRAME-INDEX

\ a package word that DEMANDS a nominal at an input position, so the input
\ direction of unification is probed, not only the output direction.
: NEED-SERIAL ( serial -- ) SERIAL>N drop ;

s" VN-ID ( serial -- serial )"              CHECK-QUIET-CANDIDATE! -1 T=
s" VN-CROSS ( serial -- frame-index )"      CHECK-QUIET-CANDIDATE!  0 T=
s" VN-CROSS2 ( frame-index -- serial )"     CHECK-QUIET-CANDIDATE!  0 T=
s" VN-N-OUT ( serial -- n )"                CHECK-QUIET-CANDIDATE!  0 T=
s" VN-N-IN ( n -- serial )"                 CHECK-QUIET-CANDIDATE!  0 T=
s" VN-TO ( n -- serial ) >SERIAL"           CHECK-QUIET-CANDIDATE! -1 T=
s" VN-FROM ( serial -- n ) SERIAL>N"        CHECK-QUIET-CANDIDATE! -1 T=
s" VN-RT ( n -- n ) >SERIAL SERIAL>N"       CHECK-QUIET-CANDIDATE! -1 T=
s" VN-LAUNDER ( n -- frame-index ) >SERIAL" CHECK-QUIET-CANDIDATE!  0 T=
s" VN-NEED-OK ( serial -- ) NEED-SERIAL"    CHECK-QUIET-CANDIDATE! -1 T=
s" VN-NEED-N ( n -- ) NEED-SERIAL"          CHECK-QUIET-CANDIDATE!  0 T=
s" VN-NEED-X ( frame-index -- ) NEED-SERIAL" CHECK-QUIET-CANDIDATE! 0 T=
;package

\ 2. package scoping: the SAME name in a different package is a DISTINCT type.
package FRAME
DEFTYPE SERIAL
s" VN-F-ID ( serial -- serial )"            CHECK-QUIET-CANDIDATE! -1 T=
s" VN-F-CROSS ( serial -- CAMERA:serial )"  CHECK-QUIET-CANDIDATE!  0 T=
s" VN-F-CROSS2 ( CAMERA:serial -- serial )" CHECK-QUIET-CANDIDATE!  0 T=
;package

\ The reverse view, from CAMERA.
package CAMERA
s" VN-C-CROSS ( serial -- FRAME:serial )"   CHECK-QUIET-CANDIDATE!  0 T=
s" VN-C-SAME ( serial -- serial )"          CHECK-QUIET-CANDIDATE! -1 T=
;package

\ 3. an undeclared nominal name is rejected as an unknown signature type.
s" VN-UNDECL ( never-declared -- never-declared )" CHECK-QUIET-CANDIDATE! 0 T=

\ 4. persistence: a declared nominal survives a snapshot-persist of the growable
\    type-family store. CHECKER-CAPTURE-PREPARE bakes the grown store and rebases
\    its pointers, so the nominal's checked contract must read back identically.
package PERSISTPKG
DEFTYPE PSERIAL
s" VN-PP-PRE ( pserial -- pserial )" CHECK-QUIET-CANDIDATE! -1 T=
TWX-SNAP-PREP
s" VN-PP-ID ( pserial -- pserial )"  CHECK-QUIET-CANDIDATE! -1 T=
s" VN-PP-N ( pserial -- n )"         CHECK-QUIET-CANDIDATE!  0 T=
;package

\ 5. the name rule: a tail an effect reads as something other than the family is
\    a reserved name in every scope. A bare `ptr` is the pointer constructor, so
\    a package's own `ptr` would leave its converter `( n -- ptr )` without a
\    pointee. Shadowing another package's family stays legal: `side` is
\    IR-SCHEMA's public enum, and the package's own `side` is a distinct type.
: MINT-PTR ( -- ) s" PTR" VNOM:MINT ;
' MINT-PTR catch TYPE-DECL:E-TDECL-NAME T=
package CTORPKG
' MINT-PTR catch TYPE-DECL:E-TDECL-NAME T=
;package

package SIDEPKG
DEFTYPE SIDE
s" VN-SIDE-MK ( n -- side ) >SIDE"          CHECK-QUIET-CANDIDATE! -1 T=
s" VN-SIDE-X ( side -- IR-SCHEMA:side )"    CHECK-QUIET-CANDIDATE!  0 T=
;package

\ 6. the name length rule is the family name cap (TFAM:TF-NAME-MAX, 255 bytes):
\    a 40-byte name and a 255-byte one declare with working converters, and a
\    256-byte one is refused E-VNOM-CAP before anything is registered.
package LONGPKG
DEFTYPE FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP
s" VN-LONG-RT ( n -- n ) >FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP>N" CHECK-QUIET-CANDIDATE! -1 T=
s" VN-LONG-N ( forty-byte-surface-name-past-the-old-cap -- n )" CHECK-QUIET-CANDIDATE! 0 T=
DEFTYPE MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
s" VN-MAX-RT ( n -- n ) >MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM>N" CHECK-QUIET-CANDIDATE! -1 T=
;package

package VNOM
create LONG-NAME TFAM:TF-NAME-MAX 1+ allot
: LONG-NAME! ( -- )  TFAM:TF-NAME-MAX 1+ 0 ?do 78 LONG-NAME i + c! loop ;
LONG-NAME!
: MINT-LONG ( -- ) LONG-NAME TFAM:TF-NAME-MAX 1+ MINT ;
' MINT-LONG catch E-VNOM-CAP T=
;package

T-REPORT
