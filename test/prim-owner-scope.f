\ prim-owner-scope.f - a package-owned primitive row admits the owner and nobody else.
\
\ Run: bin/hb --load test/prim-owner-scope.f
\
\ The subject is the general private row closer CLOSE-PRIVATE (src/core/checker.f,
\ beside PPRIM;). It interns a PPRIM: axiom into the OWNER package's private
\ wordlist, so CHECKER-FIND-ACTIVE-SYM's private leg is the only scope that
\ resolves it. Hoisting it out of package CHECKER-DECL-FRAME is what lets any
\ package own a primitive; the child fixture spells it at top level, which is
\ exactly what was impossible before.
\
\ WHY A CHILD AND NOT AN IN-PROCESS CHECK. Primitive rows are declarable only
\ while a fresh checker prefix is loading: past src/core/internal-mark.f's seal
\ PRIM: / PPRIM: / CLOSE-PRIVATE are DNAME-INT and fail closed. So the fixture
\ runs under test/native-window-owner-child.f, the native-build handoff window,
\ which re-includes src/core/checker.f FROM SOURCE - which is also what makes this
\ suite measure the working tree's checker rather than the engine's baked copy.
\
\ The transcript is compared whole rather than line by line: a case that stops
\ emitting is as much a failure as a case that answers wrongly, and only an exact
\ comparison catches the first.

require lib/test.f
require lib/string.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package PRIM-OWNER-SCOPE-SUITE

$4000 constant IO-CAP
$0A constant LF-C
180000 constant TIMEOUT-MS

create OUT IO-CAP allot
create ERR IO-CAP allot

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" prim-owner-scope" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/prim-owner-scope-child.f" >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV! ;

: CASE+ ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n va:ptr vu:n :}
   s" prim-owner: " SB-APPEND
   la lu SB-APPEND
   s" : " SB-APPEND
   va vu SB-APPEND
   LF-C SB-APPEND-C ;

\ The fresh axiom has no engine word and no second row, so these four are the
\ closer's own effect: the owner package resolves the name and no other scope does.
: AXIOM-LINES ( -- )
   s" axiom top level"      s" unresolvable" CASE+
   s" axiom inside owner"   s" admitted"     CASE+
   s" axiom other package"  s" unresolvable" CASE+
   s" axiom reopened owner" s" admitted"     CASE+ ;

\ addrmap-set keeps its global PRIM-TRUSTED-ONLY! row AND gains an owner-private
\ one, so the outside answer stays the named E-CAP-TRUSTED reject while the owner
\ gains a checked caller. `rejected` is the compile-reject rc 70.
: TIER0-LINES ( -- )
   s" t0 checked inside owner"   s" compiled" CASE+
   s" t0 checked top level"      s" rejected" CASE+
   s" t0 checked other package"  s" rejected" CASE+
   s" t0 checked reopened owner" s" compiled" CASE+
   s" t0 trusted top level"      s" compiled" CASE+ ;

\ The tier-1 trusted caller outside the owner is the case the global row exists
\ for: the optimizing compiler reads a callee's widths out of the prim table, and
\ an owner-private-only row would answer E-HIR-UNMODELED here.
: TIER1-LINES ( -- )
   s" t1 checked inside owner" s" compiled" CASE+
   s" t1 checked top level"    s" rejected" CASE+
   s" t1 trusted top level"    s" compiled" CASE+ ;

\ Package FFI's three owned rows: the foreign call and the two retypes package
\ FFI needs to marshal one. The owner compiles a checked body; every other scope
\ gets the capability reject, which is what keeps the raw FFI surface sealed
\ while lib/ffi-abi.f itself carries no TRUSTED: line.
: FFI-LINES ( -- )
   s" ffi call inside owner"     s" compiled" CASE+
   s" ptr>cell inside owner"     s" compiled" CASE+
   s" cell>ptr inside owner"     s" compiled" CASE+
   s" ffi call top level"        s" rejected" CASE+
   s" ptr>cell top level"        s" rejected" CASE+
   s" cell>ptr top level"        s" rejected" CASE+
   s" ffi call other package"    s" rejected" CASE+
   s" cell>ptr other package"    s" rejected" CASE+ ;

: EXPECT$ ( -- ptr u8 n )
   SB-RESET
   AXIOM-LINES
   TIER0-LINES
   TIER1-LINES
   FFI-LINES
   s" prim-owner: ok" SB-APPEND LF-C SB-APPEND-C
   s" window: 0" SB-APPEND LF-C SB-APPEND-C
   SB$ ;

: CHECK ( -- )
   ARGS
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   s" owner-scope transcript" T-LABEL
   OUT outu LEN>N EXPECT$ T$=
   s" the outside reject is the named capability reject" T-LABEL
   ERR erru LEN>N s" E-CAP-TRUSTED" CONTAINS? TTRUE
   ERR erru LEN>N s" 'addrmap-set' is a trust-boundary primitive" CONTAINS? TTRUE
   s" the FFI rows reject by the same named capability boundary" T-LABEL
   ERR erru LEN>N s" 'ffi-call-bounded' is a trust-boundary primitive" CONTAINS? TTRUE
   ERR erru LEN>N s" 'FFI-CELL>PTR' is a trust-boundary primitive" CONTAINS? TTRUE ;

: RUN ( -- )
   T-RESET
   [: PREPARE CHECK ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
