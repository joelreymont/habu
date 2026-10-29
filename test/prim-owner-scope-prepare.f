\ The fixture's rows, then the production seal, loaded in
\ test/native-window-owner-child.f's window ahead of
\ test/prim-owner-scope-child.f. src/core/internal-mark.f decides once, over
\ every record the window holds, which records a checked caller can reach; past
\ it PRIM:, PPRIM: and CLOSE-PRIVATE are DNAME-INT, so the rows come first and
\ the seal runs last.

\ PRIM-OWNER-AXIOM is a fresh name with no engine word: the closer's own effect,
\ with no record involved.

PPRIM: PRIM-OWNER-SCOPE PRIM-OWNER-AXIOM PE-N PE-IN CLOSE-PRIVATE

\ The package's own CORE-FOLD-C, which the engine binds there first, owns this
\ row: no row types util.f's global CORE-FOLD-C and the seal must still mark it
\ DNAME-INT.
package PRIM-OWNER-SHADOW
: CORE-FOLD-C ( n -- n ) ;
;package
PPRIM: PRIM-OWNER-SHADOW CORE-FOLD-C PE-N PE-IN PE-N PE-OUT CLOSE-PRIVATE

\ FIELD-PROJ! is REG-PROTECT: the seal marks its record DNAME-INT and no checked
\ body may call it after, its owner's included. Its owner's caller,
\ src/core/sumtype.f FIELD-ARM, compiles before the seal, so TYPE-DECL's row
\ is measured here at both tiers, a checked call and a checked tick. A refusal
\ stops the window before the child's first case.
package TYPE-DECL
: PREPARE-T0 ( -- )
   HB-TARGET-LINUX-X86-64? if exit then
   s" 0 set-tier" evaluate-closed
   s" : POS-FP0-IN ( ptr u8 n n n -- ) FIELD-PROJ! ;" evaluate-closed
   s" : POS-TFP0-IN ( -- ) ['] FIELD-PROJ! drop ;" evaluate-closed ;
PREPARE-T0
1 set-tier
: POS-FP1-IN ( ptr u8 n n n -- ) FIELD-PROJ! ;
: RESTORE-T0 ( -- )
   HB-TARGET-LINUX-X86-64? 0= if s" 0 set-tier" evaluate-closed then ;
RESTORE-T0
;package

require src/core/prefix-boundary.f
include src/core/internal-mark.f
