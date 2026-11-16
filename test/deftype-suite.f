\ deftype-suite.f - the DEFTYPE declaration surface (src/lib/type/deftype.f:
\ package-scoped arity-0 type-family nominals and generated converters).
\ The converter pair (>NAME / NAME>N) is the only boundary crossing; the same
\ name in two packages is two types. T-REPORT exits 1 on any failure.

require src/lib/codegen.f
require src/lib/type/deftype.f
require src/lib/test.f
require test/decl-diag-capture.f

T-RESET

\ silence expected rejection diagnostics (verdicts are asserted, not printed).
REJECTION:CAPTURE

\ 1. a declared value nominal, checked inside its own package; test/engine-suite.f
\    checks the converters and the rejected crossings in signatures.
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
   REJECTION:BUFFER$ s" error[E-BAD-DECLARATION]: bad deftype declaration `ptr`" CONTAINS? TTRUE
   REJECTION:BUFFER$ s" ^ reserved name" CONTAINS? TTRUE
   REJECTION:BUFFER$ S\" = word: PTR\n" CONTAINS? TTRUE
   REJECTION:BUFFER$ s" = word: ptr" CONTAINS? TFALSE ;
PTR-REJECTED
package CTORPKG
PTR-REJECTED
;package

\ 5. a family name has no length limit: a 40-byte name and a 1000-byte one
\    declare with working converters.
package LONGPKG
DEFTYPE FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP
s" VN-LONG-RT ( n -- n ) >FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP FORTY-BYTE-SURFACE-NAME-PAST-THE-OLD-CAP>N" CHECK-QUIET-CANDIDATE! -1 T=
s" VN-LONG-N ( forty-byte-surface-name-past-the-old-cap -- n )" CHECK-QUIET-CANDIDATE! 0 T=
;package

\ The 1000-byte name goes through the public route, a DEFTYPE text evaluated in
\ its own package, and its converters round-trip a number.
1000 constant VN-LONG-U
CODEGEN:BUFFER VN-LONG-SRC
: VN-LONG-NAME ( -- ) VN-LONG-U 0 ?do 78 VN-LONG-SRC CODEGEN:APPEND-BYTE loop ;
: VN-LONG+ ( ^u8 n -- ) VN-LONG-SRC CODEGEN:APPEND-STRING ;
: VN-LONG-SRC! ( -- )
   VN-LONG-SRC CODEGEN:RESET
   s" package VNLONG DEFTYPE " VN-LONG+  VN-LONG-NAME  10 VN-LONG-SRC CODEGEN:APPEND-BYTE
   s" public : VN-HUGE-RT ( n -- n ) >" VN-LONG+  VN-LONG-NAME  s"  " VN-LONG+  VN-LONG-NAME
   s" >N ; ;package" VN-LONG+ ;
VN-LONG-SRC!
VN-LONG-SRC CODEGEN:CONTENTS TEST-EVAL:RC 0 T=
s" 5 VNLONG:VN-HUGE-RT" TEST-EVAL:N 5 T=

\ 6. a base on the name's line: the type is as wide as the base and apart from
\    it, crossed by >NAME and NAME>BASE, the base in upper case.
DEFTYPE REV str
s" abc" >str >REV REV>STR str> nip 3 T=
\ a rev is not a str: 70, the checker's rejection ("expected: str", "found: rev").
s" : DTX ( rev -- str ) ;" TEST-EVAL:RC 70 T=
\ The record of a rejected DEFTYPE: its title, the label at its token, no
\ `word` note and the help, each a whole line.
: DT-REC ( ^u8 n ^u8 n ^u8 n ^u8 n ^u8 n -- )
   {: sa:^ su:n ta:^ tu:n ba:^ bu:n la:^ lu:n ha:^ hu:n :}
   REJECTION:CAPTURE
   sa su TEST-EVAL:RC CHECKER-REJECT-RC T=
   REJECTION:BUFFER$ ta tu CONTAINS? TTRUE
   ba bu la lu DECL-DIAG:CAUSE? TTRUE
   REJECTION:BUFFER$ s" = word:" CONTAINS? TFALSE
   REJECTION:BUFFER$ ha hu CONTAINS? TTRUE ;
\ the base is one name, since NAME>BASE is made from it. A name and a base the
\ NEWTYPE would accept, the base a pointer, an application or qualified, get
\ that NEWTYPE; any other keeps the record that rejects the name or the base.
s" DEFTYPE DTBAD option<n>"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `DTBAD`\n"
s" option<n>" s" a deftype base is one name"
S\" = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dtbad option<n>` instead\n" DT-REC
s" DEFTYPE DTBAD2 pkg:thing"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `DTBAD2`\n"
s" pkg:thing" s" unknown type"
S\" = help: name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`\n" DT-REC
s" DEFTYPE dtp ^u8"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dtp`\n"
s" ^u8" s" a deftype base is one name"
S\" = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dtp ^u8` instead\n" DT-REC
s" DEFTYPE dx ^nosuch"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n"
s" nosuch" s" unknown type"
S\" = help: name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`\n" DT-REC
\ A record of the name stands at the name as written, not at the base.
s" DEFTYPE ptr ^u8"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `ptr`\n"
s" ptr" s" reserved name"
S\" = help: pick another name: a built-in type such as `n` cannot name a family\n" DT-REC
s" DEFTYPE ptr str"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `ptr`\n"
s" ptr" s" reserved name"
S\" = help: pick another name: a built-in type such as `n` cannot name a family\n" DT-REC
DEFTYPE DTDUP str
s" DEFTYPE DTDUP str"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `DTDUP`\n"
s" DTDUP" s" duplicate family"
S\" = help: rename the family: this package already declares a family with this name\n" DT-REC
\ inside a longer base the help keeps the base whole: no `NEWTYPE dx option<n>`.
s" DEFTYPE dx ^option"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n"
s" option" s" type is parametric and needs type arguments"
S\" = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dx ^option<n>` instead\n" DT-REC
\ a malformed pointer inside a longer base gets the NEWTYPE over `^T`; a
\ NEWTYPE's gets the pointer's spelling.
REJECTION:CAPTURE
: DT-PTR-PTR ( -- ) s" DEFTYPE dx ^^" VERIFY:SOURCE-BUF ;
' DT-PTR-PTR catch CHECKER-REJECT-RC T=
REJECTION:BUFFER$ S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n --> <input>:1:13\n  |\n1 | DEFTYPE dx ^^\n  |             ^ pointer without a pointee\n  = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dx ^T` instead, `^` joined to the type it points at, as in `NEWTYPE dx ^u8`\n\n" CONTAINS? TTRUE
REJECTION:CAPTURE
: DT-NT-PTR ( -- ) s" NEWTYPE nx ^" VERIFY:SOURCE-BUF ;
' DT-NT-PTR catch CHECKER-REJECT-RC T=
REJECTION:BUFFER$ S\" error[E-BAD-DECLARATION]: bad newtype declaration `nx`\n --> <input>:1:12\n  |\n1 | NEWTYPE nx ^\n  |            ^ pointer without a pointee\n  = help: spell a pointer `^T`, `^` joined to the type it points at, as in `NEWTYPE link ^u8`\n\n" CONTAINS? TTRUE
\ a width inside a longer base gets the NEWTYPE that loads, `n` for the width.
s" DEFTYPE dx ^width"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n"
s" width" s" a width is not stored: keep its bytes (width>n)"
S\" = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dx ^n` instead\n" DT-REC
\ inside an application the record stands at the argument it judges.
s" DEFTYPE dx option<option>"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n"
s" option" s" type is parametric and needs type arguments"
S\" = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dx option<option<n>>` instead\n" DT-REC
s" DEFTYPE dx option<width>"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n"
s" width" s" a width is not stored: keep its bytes (width>n)"
S\" = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dx option<n>` instead\n" DT-REC
s" DEFTYPE dx option<nosuch>"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n"
s" nosuch" s" unknown type"
S\" = help: name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`\n" DT-REC
s" NEWTYPE nx option<nx>"
S\" error[E-BAD-DECLARATION]: bad newtype declaration `nx`\n"
s" nx" s" type cannot recursively name its owner"
S\" = help: name the type being declared only beneath `^`, as in `NEWTYPE link ^link`\n" DT-REC
\ No wrap holds a view, beneath a pointer or not: the help declares a STRUCTURE.
s" NEWTYPE nx ^read-view<n,n,n>"
S\" error[E-BAD-DECLARATION]: bad newtype declaration `nx`\n"
s" read-view" s" a pointer cannot reach `read-view`, which takes a scope or region"
S\" = help: declare a STRUCTURE whose field holds the view, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`\n" DT-REC
s" DEFTYPE dx ^read-view<n,n,n>"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n"
s" read-view" s" a pointer cannot reach `read-view`, which takes a scope or region"
S\" = help: declare a STRUCTURE whose field holds the view, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`\n" DT-REC
package DTQP
public
NEWTYPE dtqt
;package
s" DEFTYPE dx DTQP:dtqt"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dx`\n"
s" DTQP:dtqt" s" a deftype base is one name"
S\" = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dx DTQP:dtqt` instead\n" DT-REC
\ a NEWTYPE has no header and no clause, and its helps offer neither.
REJECTION:CAPTURE
: DT-NT-PARAM ( -- ) s" NEWTYPE dtqqq a" VERIFY:SOURCE-BUF ;
' DT-NT-PARAM catch CHECKER-REJECT-RC T=
REJECTION:BUFFER$ S\" error[E-BAD-DECLARATION]: bad newtype declaration `dtqqq`\n --> <input>:1:15\n  |\n1 | NEWTYPE dtqqq a\n  |               ^ unknown type\n  = help: name a type declared before this declaration: `n`, `^u8`, a `STRUCTURE` or an `ENUM`\n\n" CONTAINS? TTRUE
REJECTION:CAPTURE
: DT-NT-OPEN ( -- ) s" NEWTYPE dtqqq option<n" VERIFY:SOURCE-BUF ;
' DT-NT-OPEN catch CHECKER-REJECT-RC T=
REJECTION:BUFFER$ S\" error[E-BAD-DECLARATION]: bad newtype declaration `dtqqq`\n --> <input>:1:15\n  |\n1 | NEWTYPE dtqqq option<n\n  |               ^^^^^^^^ unclosed type application\n  = help: close each `<` of the type with `>`\n\n" CONTAINS? TTRUE
REJECTION:CAPTURE
: DT-NT-RV ( -- ) s" NEWTYPE dtrv read-view<n,n,n>" VERIFY:SOURCE-BUF ;
' DT-NT-RV catch CHECKER-REJECT-RC T=
REJECTION:BUFFER$ S\" error[E-BAD-DECLARATION]: bad newtype declaration `dtrv`\n --> <input>:1:24\n  |\n1 | NEWTYPE dtrv read-view<n,n,n>\n  |                        ^ type argument must be a scope parameter\n  = help: declare a STRUCTURE whose header lists the parameter, as in `STRUCTURE box<p,q>` with `FIELD v read-view<p,q,u8>`\n\n" CONTAINS? TTRUE
\ a base no type declares: the record names DEFTYPE and its help offers one
\ bare name, never a pointer.
REJECTION:CAPTURE
s" DEFTYPE path frob" TEST-EVAL:RC CHECKER-REJECT-RC T=
REJECTION:BUFFER$ s" error[E-BAD-DECLARATION]: bad deftype declaration `path`" CONTAINS? TTRUE
s" frob" s" unknown type" DECL-DIAG:CAUSE? TTRUE
REJECTION:BUFFER$ s" = help: name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`" CONTAINS? TTRUE
REJECTION:BUFFER$ s" `^u8`" CONTAINS? TFALSE
\ a comment on the name's line is no base: the type stays one cell over n.
DEFTYPE DTNOTE   \ a comment
s" VN-NOTE-RT ( n -- n ) >DTNOTE DTNOTE>N" CHECK-QUIET-CANDIDATE! -1 T=
\ the verifier replays a base as the load registers it.
: DT-VERIFY ( -- )
   s\" DEFTYPE REV2 str\n: DTV ( str -- rev2 ) >REV2 ;" VERIFY:SOURCE-BUF ;
' DT-VERIFY catch 0 T=
\ the verifier's converter text holds NAME>BASE with a 1000-byte name.
CODEGEN:BUFFER VN-VMAX-SRC
: VN-VMAX-SRC! ( -- )
   VN-VMAX-SRC CODEGEN:RESET
   s" DEFTYPE " VN-VMAX-SRC CODEGEN:APPEND-STRING
   VN-LONG-U 0 ?do 81 VN-VMAX-SRC CODEGEN:APPEND-BYTE loop
   s"  str" VN-VMAX-SRC CODEGEN:APPEND-STRING ;
VN-VMAX-SRC!
: DT-VERIFY-MAX ( -- ) VN-VMAX-SRC CODEGEN:CONTENTS VERIFY:SOURCE-BUF ;
' DT-VERIFY-MAX catch 0 T=

\ 7. a parametric base: a family whose parameters all take `n` gets the NEWTYPE
\    that declares it; any other gets another base, never `ptr<`. A multi-error
\    load records it and declares no converters for it; an earlier DEFTYPE's
\    converter still verifies.
CODEGEN:BUFFER DT-MULTI-SRC
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
REJECTION:CAPTURE
CHECK:MULTI-ERR-BEGIN ' DT-MULTI catch CHECK:MULTI-ERR-END
2 T=  0 T=
REJECTION:BUFFER$ S\" error[E-BAD-DECLARATION]: bad deftype declaration `DTWRAP`\n --> <input>:2:16\n  |\n2 | DEFTYPE DTWRAP option\n  |                ^^^^^^ type is parametric and needs type arguments\n  = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dtwrap option<n>` instead\n\n" CONTAINS? TTRUE
REJECTION:BUFFER$ s" error[E-UNDEFINED]: cannot find word `>DTWRAP`" CONTAINS? TTRUE
REJECTION:BUFFER$ s" = help: define `>DTWRAP` before this point, or check its spelling" CONTAINS? TTRUE
REJECTION:BUFFER$ s" dtguse" CONTAINS? TFALSE
\ The same record when the load runs DEFTYPE.
s" DEFTYPE DTWRAP option"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `DTWRAP`\n"
s" option" s" type is parametric and needs type arguments"
S\" = help: a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE dtwrap option<n>` instead\n" DT-REC
\ read-view's parameters are scopes and a type, which `n` does not fill.
s" DEFTYPE dtv read-view"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dtv`\n"
s" read-view" s" type is parametric and needs type arguments"
S\" = help: name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`\n" DT-REC
\ `ptr` names no type without `<`, and no `ptr<` is offered.
s" DEFTYPE dtk ptr"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `dtk`\n"
s" ptr" s" unknown type"
S\" = help: name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`\n" DT-REC
REJECTION:BUFFER$ s" ptr<" CONTAINS? TFALSE
\ a base resolves as a field type does, case and all: `Option` names no family.
s" DEFTYPE rev3 Option"
S\" error[E-BAD-DECLARATION]: bad deftype declaration `rev3`\n"
s" Option" s" unknown type"
S\" = help: name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`\n" DT-REC

T-REPORT
