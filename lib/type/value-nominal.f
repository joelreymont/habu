\ value-nominal.f - the ergonomic declaration surface for value-nominal integer
\ types: application code names a distinct checked integer type in one readable
\ line and the checker keeps it apart from a plain int and from every other
\ nominal.
\
\     package CAMERA
\     NOMINAL: SERIAL          \ a camera serial is its own type, not a bare n
\     NOMINAL: FRAME-INDEX     \ so is a frame index, distinct from a serial
\     NOMINAL: EXPOSURE-US     \ and an exposure time in microseconds
\     ;package
\
\ Each `NOMINAL: NAME` mints a fresh checker type and derives its explicit
\ converter pair `>NAME ( n -- NAME )` and `NAME>N ( NAME -- n )`. The converters
\ are the ONLY way across the boundary: the checker never lets a plain `n` stand
\ in for a nominal, and never collapses a nominal back to `n` on its own. That is
\ the whole point - the conversion is visible in the source (MISSING.md Foundation
\ A1's non-negotiable invariant, locked by test/value-nominal-suite.f).
\
\ SUBSTRATE (decision record: docs/value-nominal-substrate.md). A value nominal is
\ a package-scoped arity-0 type-family cell (a TFAM nominal scalar), exactly the
\ substrate maki/extent.f uses for a flat extent. This was chosen over extending
\ the CT-role table (DEFTYPE, src/core/roles.f) because type families are ALREADY
\ package-scoped: `NOMINAL: SERIAL` in package CAMERA and `NOMINAL: SERIAL` in
\ package FRAME are two distinct types with no engine change, whereas the CT-role
\ table is global and a second same-named DEFTYPE dies (exit 70). Choosing TFAM
\ obviates the CON-OF/CT-FIND package-scoping restructure (dot
\ habu-foundation-a1b-pkg-6692f4e3) for value nominals. Both substrates give the
\ same strictness (probed: distinct from n both directions, distinct nominals,
\ explicit-converter-only); TFAM adds package scoping for free.
\
\ MANGLING (surface NAME -> family tail). A TFAM tail is lowercase
\ (E-TFAM-CASE); the surface name is UPPER-CASE by project convention. `NOMINAL:`
\ folds the surface name to lowercase to form the tail (SERIAL -> serial,
\ FRAME-INDEX -> frame-index); internal hyphens survive (TF-CANON? allows them).
\ The upper-case surface spelling is what the generated converter WORD names use
\ (`>SERIAL`, `SERIAL>N`); the lowercase tail is what a signature type names
\ (`( serial -- n )`). CHECKER-DEFFAMILY fails closed on a tail that collides with
\ a built-in family, a CT-role, or an already-declared nominal in the same package
\ (E-TFAM-DUP / reserved-name throw), so no silent shadowing or rename.
\
\ TRUSTED BOUNDARY. The single static trusted site is NG-EVAL, the audited
\ `evaluate` that compiles each generated converter (the roles.f DTC-EVAL /
\ maki/extent.f XG-EVAL pattern): `evaluate` cannot be checker-typed, and the
\ per-nominal `n <-> family-scalar` retype is the sanctioned nominal-cast the
\ checker cannot yet express (the roles.f role-cast gap, tracked project-wide by
\ the retire-TRUSTED epic habu-epic-type-habu-a34713f0). Each generated body is a
\ no-op identity cast, correct by construction and built from string literals, so
\ no static trust site is added per declaration and every generated pair is
\ certified through the check hook by NG-EVAL. Tested by test/value-nominal-suite.f.

package VNOM

private

-6001 constant E-VNOM-NAME     \ NOMINAL: given an empty name
-6002 constant E-VNOM-CAP      \ generated-source or mangle buffer capacity exceeded

\ ---- generated-source codegen buffer (build the "TRUSTED: ... ;" converter text
\ each declaration evaluates), mirroring maki/extent.f XG-*. ------------------
$400 constant NG-CAP
create NG-BUF NG-CAP allot
variable NG-U
: NG-RESET ( -- )  0 NG-U ! ;
: NG-C ( n -- ) {: c:n :}                         \ append one byte
   NG-U @ 1 + NG-CAP > if E-VNOM-CAP throw then
   c NG-BUF NG-U @ + c!  NG-U @ 1 + NG-U ! ;
: NG+ ( ptr u8 n -- ) {: a:ptr u:n :}             \ append a string
   0 begin dup u < while  dup a + c@ NG-C  1 +  repeat drop ;
: NG$ ( -- ptr u8 n )  NG-BUF NG-U @ ;

\ the one metaprogramming boundary: `evaluate` cannot be checker-typed, so this
\ audited TRUSTED wrapper compiles the constructed converter text with the check
\ hook active. Every generated body is a proven no-op identity cast, so this one
\ boundary covers every declaration-derived converter pair (dot
\ habu-epic-type-habu-a34713f0).
TRUSTED: NG-EVAL ( -- )  NG$ evaluate ;

\ ---- surface NAME (UPPER-CASE) -> lowercase family tail -----------------------
32 constant NM-CAP
create NM-BUF NM-CAP allot
variable NM-U
: NM-LC ( n -- n ) {: c:n :}                      \ ASCII fold A..Z -> a..z
   c 65 >= c 90 <= and if c 32 + else c then ;
: NM-C ( n -- ) {: c:n :}
   NM-U @ NM-CAP >= if E-VNOM-CAP throw then
   c NM-BUF NM-U @ + c!  NM-U @ 1 + NM-U ! ;
: MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 NM-U !
   u 0 ?do  a i + c@ NM-LC NM-C  loop
   NM-BUF NM-U @ ;

\ ---- converter emitters: `TRUSTED: >NAME ( n -- tail ) ;` (inject) and
\ `TRUSTED: NAME>N ( tail -- n ) ;` (project), both no-op identity casts. --------
: EMIT-IN ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   NG-RESET
   s" TRUSTED: >" NG+  sa su NG+  s"  ( n -- " NG+  ta tu NG+  s"  ) ; " NG+
   NG-EVAL ;
: EMIT-OUT ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   NG-RESET
   s" TRUSTED: " NG+  sa su NG+  s" >N ( " NG+  ta tu NG+  s"  -- n ) ; " NG+
   NG-EVAL ;

public

\ MINT: register the arity-0 family + its converter pair in the ACTIVE package
\ (the package that called NOMINAL:, or the global scope at top level). The
\ surface name doubles as the converter word spelling; its lowercase fold is the
\ family tail. Fails closed on an empty name (E-VNOM-NAME) or a reserved / already
\ declared tail (CHECKER-DEFFAMILY: reserved-name / E-TFAM-DUP).
: MINT ( ptr u8 n -- ) {: sa:ptr su:n :}
   su 0= if E-VNOM-NAME throw then
   sa su MANGLE {: ta:ptr tu:n :}
   ta tu s" 0" CHECKER-DEFFAMILY
   sa su ta tu EMIT-IN
   sa su ta tu EMIT-OUT ;

;package

\ NOMINAL: - the value-nominal declaration keyword. Core language surface: a
\ single global declarer word (sibling to DEFTYPE / ENUM / SUMTYPE / TYPEFAMILY)
\ so it reads bare from any application package while the family lands in that
\ package's scope. Top-level-interpret-only, like the other type-declaration
\ openers: it parses the next name off the input stream and mutates the type
\ registry (through the checked VNOM:MINT boundary) - side effects its ( -- ) row
\ does not model. All machinery lives in package VNOM; this is the only new global
\ word.
: NOMINAL: ( -- )
   parse-name VNOM:MINT ;
