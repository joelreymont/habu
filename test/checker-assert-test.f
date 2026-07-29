\ checker-assert-test.f - acceptance for the shared declaration-shape reflection set in
\ test/checker-assert.f (package REFLECT; dot habu-lift-family-reflection-b18cda06).
\
\ REFLECT is what every migrated suite uses to pin the shape the type registry actually
\ recorded, so a fault here would make ten suites report green about families they never
\ read. This suite drives REFLECT against fixtures declared for the purpose:
\   A-*  : a public family resolves uniquely and every reader returns its real value.
\   D-*  : DISCRIMINATION - two public families share one tail under different
\          constructor packages, and each identity reads back its OWN family. A
\          tail-only lookup (the pre-R7 shape) would answer whichever loaded first.
\   X-*  : AMBIGUITY - two PRIVATE families share one tail. A private family publishes
\          no constructor package, so both answer to the same identity and FAMS reports
\          2: the uniqueness assertion fails loudly instead of pinning a coin flip.
\   S-*  : SENTINEL REFUSAL - a wrong constructor package, an unknown tail, and an
\          out-of-range case index each make every reader answer -1 (or `<missing>`),
\          never a neighbouring registry row.
\   R-*  : the record readers, which hang off NO-VARIANT rather than a case.
\
\ Full ENUM and STRUCTURE fixtures exercise named payload layouts through the
\ unified declaration path.

require lib/prelude.f
require lib/test.f
require test/checker-assert.f

\ ---- fixtures ------------------------------------------------------------------------
\ Two PUBLIC families share the tail `probe`. They differ in arity, case count, case names
\ and payload names, so a lookup that resolved the wrong one cannot accidentally agree.
package REFLTEST-A
public
ENUM probe 0
   VARIANT alpha FIELD first n ;VARIANT
   VARIANT beta ;VARIANT
;ENUM
;package

package REFLTEST-B
public
ENUM probe 1
   VARIANT gamma FIELD only a ;VARIANT
   VARIANT delta ;VARIANT
   VARIANT epsilon ;VARIANT
;ENUM
;package

\ Two PRIVATE families share the tail `shadow`. Neither publishes a constructor package,
\ so neither can be told from the other by identity - the case FAMS exists to expose.
package REFLTEST-P
private
ENUM shadow 0
   VARIANT one FIELD a n ;VARIANT
   VARIANT two ;VARIANT
;ENUM
;package

package REFLTEST-Q
private
ENUM shadow 0
   VARIANT one FIELD a n ;VARIANT
   VARIANT two ;VARIANT
;ENUM
;package

\ A record fixture: its fields hang off NO-VARIANT, which is the other reader path.
package REFLTEST-R
public
STRUCTURE row 0
   FIELD height n
   FIELD width n
;STRUCTURE
;package

package REFLECT-TEST

\ the identities under test, named once
: A$ ( -- ptr u8 n ptr u8 n )     s" probe" s" REFLTEST--A-PROBE" ;
: B$ ( -- ptr u8 n ptr u8 n )     s" probe" s" REFLTEST--B-PROBE" ;
: ROW$ ( -- ptr u8 n ptr u8 n )   s" row" s" REFLTEST--R-ROW" ;
: SHADOW$ ( -- ptr u8 n ptr u8 n )  s" shadow" s" " ;      \ private: no constructor package
: WRONG$ ( -- ptr u8 n ptr u8 n )   s" probe" s" REFLTEST--Z-PROBE" ;   \ no such package
: GONE$ ( -- ptr u8 n ptr u8 n )    s" no-such-family" s" REFLTEST--A-PROBE" ;

T-RESET

\ ---- A: a public family resolves uniquely and reads back its own shape ---------------
A$ REFLECT:FAMS 1 T=
A$ REFLECT:KIND TK-SUM T=
A$ REFLECT:ARITY 0 T=
A$ REFLECT:WIDTH 2 T=
A$ REFLECT:VIS 1 T=
A$ REFLECT:VARS 2 T=
A$ 0 REFLECT:ARM$ s" alpha" T$=
A$ 1 REFLECT:ARM$ s" beta" T$=
A$ 0 REFLECT:ARM-CTOR$ s" REFLTEST--A-PROBE" T$=
A$ 0 REFLECT:ARM-FLDS 1 T=
A$ 1 REFLECT:ARM-FLDS 0 T=
A$ 0 s" first" REFLECT:ARM-SLOT 0 T=

\ ---- D: the same tail under another constructor package is another family ------------
B$ REFLECT:FAMS 1 T=
B$ REFLECT:ARITY 1 T=              \ A is arity 0; reading B's arity proves the split
B$ REFLECT:VARS 3 T=               \ A has 2 cases, B has 3
B$ 0 REFLECT:ARM$ s" gamma" T$=    \ A's case 0 is `alpha`
B$ 2 REFLECT:ARM$ s" epsilon" T$=  \ A has no case 2 at all
B$ 0 s" only" REFLECT:ARM-SLOT 0 T=
B$ 0 s" first" REFLECT:ARM-SLOT -1 T=   \ A's field name is absent from B

\ ---- X: two private families collide, and FAMS says so -------------------------------
\ This is the uniqueness assertion earning its keep: a suite pinning `shadow` by this
\ identity gets 2 and must stop, instead of silently reading whichever row came first.
SHADOW$ REFLECT:FAMS 2 T=

\ ---- S: every reader refuses the sentinel --------------------------------------------
WRONG$ REFLECT:FAMS 0 T=
WRONG$ REFLECT:KIND -1 T=
WRONG$ REFLECT:ARITY -1 T=
WRONG$ REFLECT:WIDTH -1 T=
WRONG$ REFLECT:VIS -1 T=
WRONG$ REFLECT:VARS -1 T=
WRONG$ 0 REFLECT:ARM$ s" <missing>" T$=
WRONG$ 0 REFLECT:ARM-CTOR$ s" <missing>" T$=
WRONG$ 0 REFLECT:ARM-FLDS -1 T=
WRONG$ 0 s" first" REFLECT:ARM-SLOT -1 T=
WRONG$ REFLECT:FLDS -1 T=
WRONG$ s" first" REFLECT:SLOT -1 T=
GONE$ REFLECT:FAMS 0 T=
GONE$ REFLECT:KIND -1 T=
GONE$ 0 REFLECT:ARM$ s" <missing>" T$=

\ an out-of-range case index is refused the same way, from both ends
A$ 2 REFLECT:ARM$ s" <missing>" T$=
A$ -1 REFLECT:ARM$ s" <missing>" T$=
A$ 2 REFLECT:ARM-FLDS -1 T=
A$ 2 s" first" REFLECT:ARM-SLOT -1 T=

\ ---- R: the record readers -----------------------------------------------------------
ROW$ REFLECT:FAMS 1 T=
ROW$ REFLECT:KIND TK-PRODUCT T=
ROW$ REFLECT:FLDS 2 T=
ROW$ s" height" REFLECT:SLOT 0 T=
ROW$ s" width" REFLECT:SLOT 1 T=
ROW$ s" depth" REFLECT:SLOT -1 T=       \ a field the record does not declare
ROW$ 0 REFLECT:ARM-FLDS 0 T=            \ a record owns no per-case rows
ROW$ s" height" REFLECT:CELLS 1 T=      \ cell width of a declared field ...
ROW$ s" depth" REFLECT:CELLS -1 T=      \ ... and the sentinel for one that is absent
A$ 0 s" first" REFLECT:ARM-CELLS 1 T=   \ the case path reads widths too
A$ 0 s" absent" REFLECT:ARM-CELLS -1 T=
A$ 2 s" first" REFLECT:ARM-CELLS -1 T=  \ out-of-range case, not the record rows
WRONG$ s" first" REFLECT:CELLS -1 T=

T-REPORT

;package
