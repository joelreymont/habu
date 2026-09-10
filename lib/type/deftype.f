\ deftype.f - the ergonomic declaration surface for value-nominal integer
\ types: application code names a distinct checked integer type in one readable
\ line and the checker keeps it apart from a plain int and from every other
\ nominal.
\
\     package CAMERA
\     DEFTYPE SERIAL           \ a camera serial is its own type, not a bare n
\     DEFTYPE FRAME-INDEX      \ so is a frame index, distinct from a serial
\     DEFTYPE EXPOSURE-US      \ and an exposure time in microseconds
\     ;package
\
\ Each `DEFTYPE NAME` mints a fresh checker type and derives its explicit
\ converter pair `>NAME ( n -- NAME )` and `NAME>N ( NAME -- n )`. The converters
\ are the ONLY way across the boundary: the checker never lets a plain `n` stand
\ in for a nominal, and never collapses a nominal back to `n` on its own. That is
\ the whole point - the conversion is visible in the source (MISSING.md Foundation
\ A1's non-negotiable invariant, locked by test/deftype-suite.f).
\
\ SUBSTRATE (decision record: docs/value-nominal-substrate.md). A value nominal is
\ a package-scoped arity-0 type-family cell (a TFAM nominal scalar), exactly the
\ substrate maki/extent.f uses for a flat extent. This was chosen over extending
\ the CT-role table (roles.f's since-retired DEFTYPE) because type families are
\ ALREADY package-scoped: `DEFTYPE SERIAL` in package CAMERA and `DEFTYPE SERIAL`
\ in package FRAME are two distinct types with no engine change, whereas the CT-role
\ table was global and a second same-named declaration collided. Choosing TFAM
\ obviates the CON-OF/CT-FIND package-scoping restructure (dot
\ habu-foundation-a1b-pkg-6692f4e3) for value nominals. Both substrates give the
\ same strictness (probed: distinct from n both directions, distinct nominals,
\ explicit-converter-only); TFAM adds package scoping for free.
\
\ MANGLING (surface NAME -> family tail). A TFAM tail is lowercase
\ (E-TFAM-CASE); the surface name is UPPER-CASE by project convention. `DEFTYPE`
\ folds the surface name to lowercase to form the tail (SERIAL -> serial,
\ FRAME-INDEX -> frame-index); internal hyphens survive (TF-CANON? allows them).
\ The upper-case surface spelling is what the generated converter WORD names use
\ (`>SERIAL`, `SERIAL>N`); the lowercase tail is what a signature type names
\ (`( serial -- n )`). CHECKER-DEFFAMILY fails closed on a tail that collides with
\ a built-in family, a CT-role, or an already-declared nominal in the same package
\ (E-TFAM-DUP / reserved-name throw), so no silent shadowing or rename.
\
\ Generated converters use CAST:, which checks representation, ownership and
\ linearity. NG-EVAL is the source-evaluation boundary; it does not exempt a
\ generated declaration from those checks.

require lib/string.f                 \ ASCII-LOWER: fold the surface name to the family tail
require lib/codegen.f                \ CODEGEN:BUFFER-E: the shared generated-source byte buffer

package VNOM

private

-6001 constant E-VNOM-NAME     \ DEFTYPE given an empty name
-6002 constant E-VNOM-CAP      \ generated-source or mangle buffer capacity exceeded

\ ---- generated-source codegen buffer (build the "CAST: ..." converter text
\ each declaration evaluates). The append mechanics live in package CODEGEN
\ (lib/codegen.f); these thin words bind them to this file's NG-BUFFER instance,
\ minted with the E-VNOM-CAP throw code its callers already expect. ------------
$400 constant NG-CAP
NG-CAP E-VNOM-CAP E-VNOM-CAP CODEGEN:BUFFER-E NG-BUFFER
: NG-RESET ( -- )  NG-BUFFER CODEGEN:RESET ;
: NG+ ( ptr u8 n -- )  NG-BUFFER CODEGEN:APPEND-STRING ;   \ append a string
: NG$ ( -- ptr u8 n )  NG-BUFFER CODEGEN:CONTENTS ;

\ Evaluate declaration text with the checker active.
TRUSTED: NG-EVAL ( -- )  NG$ evaluate ;

\ ---- surface NAME (UPPER-CASE) -> lowercase family tail -----------------------
\ A second CODEGEN buffer, kept separate from NG-BUFFER because MINT reads the
\ mangled tail out of here while EMIT-IN/EMIT-OUT build the converter text in
\ NG-BUFFER. The tail fold is lib/string.f ASCII-LOWER.
32 constant NM-CAP
NM-CAP E-VNOM-CAP E-VNOM-CAP CODEGEN:BUFFER-E NM-BUFFER
: MANGLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   NM-BUFFER CODEGEN:RESET
   u 0 ?do  a i + c@ ASCII-LOWER NM-BUFFER CODEGEN:APPEND-BYTE  loop
   NM-BUFFER CODEGEN:CONTENTS ;

\ Converter declarations have no body; CAST: validates the retype.
: EMIT-IN ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   NG-RESET
   s" CAST: >" NG+  sa su NG+  s"  ( n -- " NG+  ta tu NG+  s"  ) " NG+
   NG-EVAL ;
: EMIT-OUT ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   NG-RESET
   s" CAST: " NG+  sa su NG+  s" >N ( " NG+  ta tu NG+  s"  -- n ) " NG+
   NG-EVAL ;

public

\ MINT: register the arity-0 family + its converter pair in the ACTIVE package
\ (the package that called DEFTYPE, or the global scope at top level). The
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

\ DEFTYPE - the value-nominal declaration keyword. Core language surface: a
\ single global declarer word (sibling to ENUM / SUMTYPE / NEWTYPE)
\ so it reads bare from any application package while the family lands in that
\ package's scope. Top-level-interpret-only, like the other type-declaration
\ openers: it parses the next name off the input stream and mutates the type
\ registry (through the checked VNOM:MINT boundary) - side effects its ( -- ) row
\ does not model. All machinery lives in package VNOM; this is the only new global
\ word.
: DEFTYPE ( -- )
   parse-name VNOM:MINT ;
