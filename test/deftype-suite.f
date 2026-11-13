\ deftype-suite.f - the DEFTYPE declaration surface (src/lib/type/deftype.f:
\ package-scoped arity-0 type-family nominals and generated converters).
\ The converter pair (>NAME / NAME>N) is the only boundary crossing; the same
\ name in two packages is two types. T-REPORT exits 1 on any failure.

require src/lib/codegen.f
require src/lib/type/deftype.f
require src/lib/test.f

T-RESET

\ silence expected rejection diagnostics (verdicts are asserted, not printed).
create VNDIAG-BUF 8192 allot
VNDIAG-BUF 8192 DIAG-BUFFER!

\ 1. a declared value nominal, checked inside its own package; test/engine-suite.f
\    checks the converters and the refused crossings in signatures.
package CAMERA
DEFTYPE SERIAL
DEFTYPE FRAME-INDEX

\ a package word that demands a nominal at an input position probes the input
\ direction of unification.
: NEED-SERIAL ( serial -- ) SERIAL>N drop ;

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

\ 4. the name rule: a tail an effect reads as something other than the family is
\    a reserved name in every scope. A bare `ptr` is the pointer constructor, so
\    a package's own `ptr` would leave its converter `( n -- ptr )` without a
\    pointee.
: MINT-PTR ( -- ) s" PTR" VNOM:MINT ;
' MINT-PTR catch TYPE-DECL:E-TDECL-NAME T=
package CTORPKG
' MINT-PTR catch TYPE-DECL:E-TDECL-NAME T=
;package

\ 5. the name length rule is the family name cap (TFAM:TF-NAME-MAX, 255 bytes):
\    a 40-byte name and a 255-byte one declare with working converters, and a
\    256-byte one is refused E-VNOM-CAP before anything is registered.
package LONGPKG
DEFTYPE FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP
s" VN-LONG-RT ( n -- n ) >FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP>N" CHECK-QUIET-CANDIDATE! -1 T=
s" VN-LONG-N ( forty-byte-surface-name-past-the-old-cap -- n )" CHECK-QUIET-CANDIDATE! 0 T=
DEFTYPE MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
s" VN-MAX-RT ( n -- n ) >MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM>N" CHECK-QUIET-CANDIDATE! -1 T=
;package

\ The 256-byte name goes through the public route, a DEFTYPE text evaluated in
\ its own package; -6002 is deftype.f's private E-VNOM-CAP.
-6002 constant VN-CAP-CODE
TFAM:TF-NAME-MAX 32 + CODEGEN:BUFFER VN-CAP-SRC
: VN-CAP-SRC! ( -- )
   VN-CAP-SRC CODEGEN:RESET
   s" package VNCAP DEFTYPE " VN-CAP-SRC CODEGEN:APPEND-STRING
   TFAM:TF-NAME-MAX 1+ 0 ?do 78 VN-CAP-SRC CODEGEN:APPEND-BYTE loop
   s"  ;package" VN-CAP-SRC CODEGEN:APPEND-STRING ;
VN-CAP-SRC!
VN-CAP-SRC CODEGEN:CONTENTS TEST-EVAL:RC VN-CAP-CODE T=

T-REPORT
