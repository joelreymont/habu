\ using-test.f - `using NAME ... ;using` consumer-side package import
\ (dot habu-using-import-pkg-a07dd7ba).
\
\ `using NAME` makes package NAME's PUBLIC wordlist visible to bare lookup until
\ the matching `;using`, the enclosing `;package`, or the end of the load file.
\ Privates stay invisible, no definition lands in NAME, qualified NAME:WORD is
\ unchanged, and a bare tail resolving in two used packages is a hard error.
\ Every case runs a source string through INCLUDE-EVALUATE under catch, so an
\ interpret- or compile-time reject surfaces as the engine's named throw code
\ (0 = accepted). The checked-body cases prove the checker resolves used publics
\ identically to the runtime: an unresolved bare tail would fail certification.

require lib/prelude.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ . ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" using-test: expected " type want . s" got " type got . cr
      #FAIL @ 1 + #FAIL !
   then ;

\ evaluate a source string, returning its throw code (0 = accepted).
variable UCE-A   variable UCE-U
: UCE-GO ( -- )  UCE-A @ UCE-U @ INCLUDE-EVALUATE ;
: UCE-CATCH ( ptr u8 n -- n )  UCE-U ! UCE-A !  [: UCE-GO ;] catch ;

\ Engine reject codes (src/core/engine-error.f USING-*), delivered as catchable
\ throws inside INCLUDE-EVALUATE.
89 constant E-NO-NAME
90 constant E-BAD-NAME
91 constant E-UNKNOWN
92 constant E-OVERFLOW
93 constant E-UNBALANCED
94 constant E-AMBIGUOUS
70 constant E-REJECT            \ E-UNDEFINED / checker rejection
7141 constant E-SHADOW          \ E-USING-SHADOW-GLOBAL: a global shadows a used public of the same name

\ --- shared fixture packages (defined once; cases reference them) ---
package UA public : AW ( -- n ) 11 ; ;package
package UB public : BW ( -- n ) 22 ; ;package
package UC public : AW ( -- n ) 33 ; ;package   \ same tail AW as UA -> ambiguity source
package UP  : SECP ( -- n ) 99 ; public : PUBW ( -- n ) SECP ; ;package   \ SECP is private

\ shadow fixtures (dot habu-err-on-global-e62f806c): a used package whose publics
\ collide with globals of the same name. GW's global effect DIFFERS from the
\ public's; MW's global effect COINCIDES with the public's (the dangerous
\ silent-wrong-bind case: no mismatch would ever surface). NC has no global, SW's
\ global is defined later inside the using scope (the order-swapped variant).
package USG public
   : GW ( -- n ) 1 ;
   : MW ( -- n ) 2 ;
   : SW ( -- n ) 3 ;
   : NC ( -- n ) 4 ;
;package
: GW ( n n -- n ) + ;              \ global GW (2 -- 1): differs from USG:GW (0 -- 1)
: MW ( -- n ) 111 ;                \ global MW (0 -- 1): coincides with USG:MW (0 -- 1)

\ === positives (accepted) ===
\ top-level using: bare public resolves
s" using UA AW drop ;using" UCE-CATCH 0 T=
\ two concurrent usings both resolve; ;using pops the most recent
s" using UA using UB AW drop BW drop ;using ;using" UCE-CATCH 0 T=
\ qualified name is always available regardless of using
s" UA:AW drop" UCE-CATCH 0 T=
\ the same package used twice is not ambiguous (one WID)
s" using UA using UA AW drop ;using ;using" UCE-CATCH 0 T=
\ checked body resolves a used public (proves the checker resolution)
s" using UA : UT-CB1 ( -- n ) AW 1 + ; UT-CB1 drop ;using" UCE-CATCH 0 T=
\ using inside a package block; the used public resolves in the public body
s" package UH using UA public : UT-H1 ( -- n ) AW ; ;package UH:UT-H1 drop" UCE-CATCH 0 T=
\ inner scope wins silently: the open package's own tail shadows a used tail
s" package UI using UA : AW ( -- n ) 7 ; public : UT-I1 ( -- n ) AW ; ;package UI:UT-I1 drop" UCE-CATCH 0 T=
\ a call resolved under a using stays compiled after ;using (compile-time resolution)
s" using UA : UT-CB2 ( -- n ) AW ; ;using UT-CB2 drop" UCE-CATCH 0 T=

\ === negatives (rejected, fail closed) ===
\ used package's PRIVATE word does not resolve bare
s" using UP SECP drop ;using" UCE-CATCH E-REJECT T=
\ a private word is not reachable in a checked body under using either
s" using UP : UT-BADP ( -- n ) SECP ; ;using" UCE-CATCH E-REJECT T=
\ a bare public is not visible without a using
s" AW drop" UCE-CATCH E-REJECT T=
\ ambiguous: AW resolves in two used packages (interpret)
s" using UA using UC AW drop ;using ;using" UCE-CATCH E-AMBIGUOUS T=
\ The checker reports its own ambiguity code for a compiled body.
s" using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using" UCE-CATCH E-USING-AMBIGUOUS T=
\ unknown package
s" using NOPE-PKG" UCE-CATCH E-UNKNOWN T=
\ a colon-bearing name is not a package name
s" using UA:AW" UCE-CATCH E-BAD-NAME T=
\ ;using with no using open is unbalanced
s" ;using" UCE-CATCH E-UNBALANCED T=
\ a define under a using lands in the current scope, never in the used package
s" using UA : UT-NEW ( -- n ) 5 ; ;using UA:UT-NEW drop" UCE-CATCH E-REJECT T=
\ overflow: more than USE-MAX (16) concurrent usings
s" using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA" UCE-CATCH E-OVERFLOW T=

\ === scope end: using does not leak ===
\ ;using ends the scope: AW is unresolved after it
s" using UA AW drop ;using AW drop" UCE-CATCH E-REJECT T=
\ ;package clears a using opened inside the package
s" package UJ using UA public : UT-J1 ( -- n ) AW ; ;package AW drop" UCE-CATCH E-REJECT T=

\ === global-vs-used-public shadow (dot habu-err-on-global-e62f806c) ===
\ A bare tail inside a using scope that resolves to a global AND a used public of
\ the same name is ambiguous and fails closed at the reference site.
\ global defined by an earlier load, then using-import of the same name -> reject
s" using USG : USG-R1 ( -- n ) GW ; ;using" UCE-CATCH E-SHADOW T=
\ order-swapped: using first, global defined later in the same scope, then bareword
s" using USG : SW ( -- n ) 9 ; : USG-R2 ( -- n ) SW ; ;using" UCE-CATCH E-SHADOW T=
\ effects coincide: today binds the global silently and CERTIFIES; must still reject
s" using USG : USG-R3 ( -- n ) MW ; ;using" UCE-CATCH E-SHADOW T=
\ qualified access is the escape: PKG:WORD reaches the used public and certifies
s" using USG : USG-R4 ( -- n ) USG:GW ; ;using" UCE-CATCH 0 T=
\ a used import with NO global collision keeps resolving (purely-additive contract)
s" using USG : USG-R5 ( -- n ) NC ; ;using" UCE-CATCH 0 T=
\ no using in scope: a bare global is never a shadow error (resolves to the global)
s" MW drop" UCE-CATCH 0 T=

\ An already compiled import keeps its binding when a global is defined later.
package USH public
   : SOLO ( -- n ) 7 ;
   : LATER ( n -- n ) 2 * ;
   : SOONER ( -- n ) 13 ;
;package
s" using USH : USH-R3 ( -- n ) SOLO ; ;using" UCE-CATCH 0 T=
USH-R3 7 T=
s" using USH : LATER ( -- n ) 3 ; : USH-R4 ( -- n ) LATER ; ;using" UCE-CATCH E-SHADOW T=
s" using USH : USH-R5 ( -- n ) SOONER ; ;using" UCE-CATCH 0 T=
USH-R5 13 T=
s" : SOONER ( -- n ) 99 ;" UCE-CATCH 0 T=
USH-R5 13 T=

\ An open package's own definition wins over an imported tail. The different
\ effect makes a wrong binding observable through rejection and execution.
package USP public : USPW ( n -- n ) 1 + ; ;package
s" package USQ using USP : USPW ( -- n ) 5 ; ;using ;package" UCE-CATCH 0 T=
s" package USQ using USP public : USQ-R1 ( -- n ) 41 USPW ; ;using ;package" UCE-CATCH E-REJECT T=
s" package USQ using USP public : USQ-R2 ( -- n ) 41 USP:USPW ; ;using ;package" UCE-CATCH 0 T=
USQ:USQ-R2 42 T=

\ Package-private and public definitions both win over a same-named global.
\ These are ordinary compiled declarations with their real checked effects.
s" : OPK-G ( -- n ) 7 ;" UCE-CATCH 0 T=
s" package OPKA : OPK-G ( -- n ) 41 ; ;package" UCE-CATCH 0 T=
s" package OPKA public : OPKA-R ( -- n ) OPK-G ; ;package" UCE-CATCH 0 T=
OPKA:OPKA-R 41 T=
\ Another package still reaches the global and ordinary primitives.
s" package OPKB public : OPKB-R ( -- n ) OPK-G ; ;package" UCE-CATCH 0 T=
OPKB:OPKB-R 7 T=
s" package OPKB public : OPKB-P ( n -- n ) dup + ; ;package" UCE-CATCH 0 T=
7 OPKB:OPKB-P 14 T=

s" : OPK-H ( -- n ) 7 ;" UCE-CATCH 0 T=
s" package OPKC public : OPK-H ( -- n ) 41 ; ;package" UCE-CATCH 0 T=
s" package OPKC public : OPKC-R ( -- n ) OPK-H ; ;package" UCE-CATCH 0 T=
OPKC:OPKC-R 41 T=

\ Binding to the package's different effect must reject the mismatched caller.
s" : OPK-K ( -- n ) 7 ;" UCE-CATCH 0 T=
s" package OPKD : OPK-K ( n -- n ) drop 41 ; ;package" UCE-CATCH 0 T=
s" package OPKD public : OPKD-R ( -- n ) OPK-K ; ;package" UCE-CATCH E-REJECT T=

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" using-test: failures" 1 die ;
REPORT
