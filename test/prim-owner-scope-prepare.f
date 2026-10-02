\ prim-owner-scope-prepare.f - the fixture's rows, then the production seal.
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

PPRIM: PRIM-OWNER-SCOPE PRIM-OWNER-AXIOM PE-N PE-IN CLOSE-PRIVATE

package PRIM-OWNER-SHADOW
: CORE-FOLD-C ( n -- n ) ;
;package
PPRIM: PRIM-OWNER-SHADOW CORE-FOLD-C PE-N PE-IN PE-N PE-OUT CLOSE-PRIVATE

require src/core/prefix-boundary.f
include src/core/internal-mark.f
