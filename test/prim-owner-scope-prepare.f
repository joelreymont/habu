\ prim-owner-scope-prepare.f - the fixture's rows and its pre-seal owner case,
\ then the production seal.
\
\ Loaded in test/native-window-owner-child.f's window once the include words are
\ live, ahead of test/prim-owner-scope-child.f. The seal is what the private-only
\ cases measure: src/core/internal-mark.f decides which records a checked caller
\ can reach, and it decides it once, over every record the window holds. Rows are
\ declarable only before that pass (past it PRIM: / PPRIM: / CLOSE-PRIVATE are
\ DNAME-INT), so the fixture's rows come first and the real pass runs last.
\
\ 1. PRIM-OWNER-AXIOM is a fresh name with no engine word: the closer's own
\    effect, with no record involved.
\ 2. Package PRIM-OWNER-SHADOW defines its own CORE-FOLD-C and owns a private
\    row for it. That row types the package's word, which the engine binds there
\    first, so it says nothing about util.f's global CORE-FOLD-C: no row types
\    that record and the seal must still mark it DNAME-INT.
\ 3. FIELD-PROJ! is REG-PROTECT, so the seal marks its record DNAME-INT and no
\    checked body may call it after that, its owner's included. Its owner's
\    caller, src/core/sumtype.f TDPLAN-FP-ARM, compiles before the seal, so
\    TYPE-DECL's row is measured here, in the reopened owner at both tiers: a
\    checked call and a checked tick. A refusal stops the window before the
\    child's first case.

PPRIM: PRIM-OWNER-SCOPE PRIM-OWNER-AXIOM PE-N PE-IN CLOSE-PRIVATE

package PRIM-OWNER-SHADOW
: CORE-FOLD-C ( n -- n ) ;
;package
PPRIM: PRIM-OWNER-SHADOW CORE-FOLD-C PE-N PE-IN PE-N PE-OUT CLOSE-PRIVATE

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
