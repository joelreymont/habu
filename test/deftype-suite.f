\ deftype-suite.f - the DEFTYPE declaration surface (src/lib/type/deftype.f:
\ package-scoped arity-0 type-family nominals and generated converters).
\ The converter pair (>NAME / NAME>N) is the only boundary crossing; the same
\ name in two packages is two types. T-REPORT exits 1 on any failure.

require src/lib/codegen.f
require src/lib/type/deftype.f
require src/lib/test.f

T-RESET

\ silence expected rejection diagnostics (verdicts are asserted, not printed).
REJECTION:CAPTURE

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
\    a reserved name in every scope. An effect reads the token `ptr` ahead of
\    any family, so a package's own `ptr` could never be named in one.
: MINT-PTR ( -- ) s" PTR" VNOM:MINT ;
\ The record names the surface PTR once, in its `word` note.
: PTR-REJECTED ( -- )
   REJECTION:CAPTURE
   ['] MINT-PTR CHECKER-REJECT-RC TTHROWSQ
   REJECTION:BUFFER$ s" error[E-BAD-DECLARATION]: bad newtype declaration `ptr`" CONTAINS? TTRUE
   REJECTION:BUFFER$ s" ^ reserved name" CONTAINS? TTRUE
   REJECTION:BUFFER$ S\" = word: PTR\n" CONTAINS? TTRUE
   REJECTION:BUFFER$ s" = word: ptr" CONTAINS? TFALSE ;
PTR-REJECTED
package CTORPKG
PTR-REJECTED
;package

\ 5. the name length rule is the family name cap (TFAM:TF-NAME-MAX, 255 bytes):
\    a 40-byte name and a 255-byte one declare with working converters, and a
\    256-byte one is rejected with a record before anything is registered.
package LONGPKG
DEFTYPE FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP
s" VN-LONG-RT ( n -- n ) >FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP>N" CHECK-QUIET-CANDIDATE! -1 T=
s" VN-LONG-N ( forty-byte-surface-name-past-the-old-cap -- n )" CHECK-QUIET-CANDIDATE! 0 T=
DEFTYPE MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
s" VN-MAX-RT ( n -- n ) >MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM>N" CHECK-QUIET-CANDIDATE! -1 T=
;package

\ The 256-byte name goes through the public route, a DEFTYPE text evaluated in
\ its own package.
CODEGEN:BUFFER VN-CAP-SRC
: VN-CAP-SRC! ( -- )
   VN-CAP-SRC CODEGEN:RESET
   s" package VNCAP DEFTYPE " VN-CAP-SRC CODEGEN:APPEND-STRING
   TFAM:TF-NAME-MAX 1+ 0 ?do 78 VN-CAP-SRC CODEGEN:APPEND-BYTE loop
   10 VN-CAP-SRC CODEGEN:APPEND-BYTE
   s" ;package" VN-CAP-SRC CODEGEN:APPEND-STRING ;
VN-CAP-SRC!
REJECTION:CAPTURE
VN-CAP-SRC CODEGEN:CONTENTS TEST-EVAL:RC CHECKER-REJECT-RC T=
REJECTION:BUFFER$ s" name longer than 255 bytes" CONTAINS? TTRUE

\ 6. a base on the name's line: the type is as wide as the base and apart from
\    it, crossed by >NAME and NAME>BASE, the base in upper case.
DEFTYPE REV str
s" abc" >str >REV REV>STR str> nip 3 T=
\ a rev is not a str: 70, the checker's refusal ("expected: str actual: rev").
s" : DTX ( rev -- str ) ;" TEST-EVAL:RC 70 T=
\ the base is one name, since NAME>BASE is made from it; the refusal names the
\ type.
VNDIAG-BUF 8192 REJECTION:BUFFER!
s" DEFTYPE DTBAD option<n>" TEST-EVAL:RC TYPE-DECL:E-TDECL-SYNTAX T=
REJECTION:BUFFER$ s" DTBAD" CONTAINS? TTRUE
s" DEFTYPE DTBAD2 pkg:thing" TEST-EVAL:RC TYPE-DECL:E-TDECL-SYNTAX T=
\ a comment on the name's line is no base: the type stays one cell over n.
DEFTYPE DTNOTE   \ a comment
s" VN-NOTE-RT ( n -- n ) >DTNOTE DTNOTE>N" CHECK-QUIET-CANDIDATE! -1 T=
\ the verifier replays a base as the load registers it.
: DT-VERIFY ( -- )
   s\" DEFTYPE REV2 str\n: DTV ( str -- rev2 ) >REV2 ;" VERIFY:SOURCE-BUF ;
' DT-VERIFY catch 0 T=
\ the verifier's converter text holds NAME>BASE with a name at the cap.
TFAM:TF-NAME-MAX 32 + CODEGEN:BUFFER VN-VMAX-SRC
: VN-VMAX-SRC! ( -- )
   VN-VMAX-SRC CODEGEN:RESET
   s" DEFTYPE " VN-VMAX-SRC CODEGEN:APPEND-STRING
   TFAM:TF-NAME-MAX 0 ?do 81 VN-VMAX-SRC CODEGEN:APPEND-BYTE loop
   s"  str" VN-VMAX-SRC CODEGEN:APPEND-STRING ;
VN-VMAX-SRC!
: DT-VERIFY-MAX ( -- ) VN-VMAX-SRC CODEGEN:CONTENTS VERIFY:SOURCE-BUF ;
' DT-VERIFY-MAX catch 0 T=

\ 7. a multi-error load records a base the wrap refuses and declares no
\    converters for it; an earlier DEFTYPE's converter still verifies.
256 CODEGEN:BUFFER DT-MULTI-SRC
: DT-LINE ( ^u8 n -- )
   DT-MULTI-SRC CODEGEN:APPEND-STRING
   10 DT-MULTI-SRC CODEGEN:APPEND-BYTE ;
: DT-MULTI-SRC! ( -- )
   DT-MULTI-SRC CODEGEN:RESET
   s" DEFTYPE DTGOOD str" DT-LINE
   s" DEFTYPE DTWRAP option" DT-LINE
   s" : DTUSE ( n -- dtwrap ) >DTWRAP ;" DT-LINE
   s" : DTGUSE ( str -- dtgood ) >DTGOOD ;" DT-LINE ;
DT-MULTI-SRC!
: DT-MULTI ( -- )
   DT-MULTI-SRC CODEGEN:CONTENTS VERIFY:SOURCE-BUF ;
VNDIAG-BUF 8192 REJECTION:BUFFER!
MULTI-ERR-BEGIN ' DT-MULTI catch MULTI-ERR-END
2 T=  0 T=
REJECTION:BUFFER$ s" undefined word '>DTWRAP'"
CONTAINS? TTRUE
REJECTION:BUFFER$ s" dtguse" CONTAINS? TFALSE

T-REPORT
