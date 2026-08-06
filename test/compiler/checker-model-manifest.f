\ checker-model-manifest.f - focused test for the shared checker model contract.
\
\ It runs the Habu half of the checker parity binding: the frozen tables in
\ `test/compiler/checker-model-schema.f` asked of the shipped checker. That is
\ the token-by-token walk over the concrete type registry `CT-INIT` and over the
\ control-flow dispatch `CF-TOK?`, the term tags and their codes, the source run
\ that writes each control frame kind, and every shared program vector driven
\ through the real `CHECK-QUIET-CANDIDATE!`.
\
\ It deliberately asks nothing of the proof assistant. The other half - making
\ Rocq prove the same rows about `Habu.Common.Effects` and
\ `Habu.Common.Control`, and holding both models' assumption sets empty - is
\ `test/compiler/checker-model-proof.f`, which needs the Rocq toolchain on PATH
\ and therefore runs in the standalone gate rather than in every resident test
\ run.

require lib/test.f
require test/compiler/checker-model-cases.f

package CHECKER-MODEL-MANIFEST-TEST
public

: RUN ( -- )
   T-RESET
   CHECKER-MODEL-CASES:HABU-SIDE
   T-REPORT ;

;package

\ The vectors are driven INSIDE the package that declares their sum families.
\ `construct` resolves its family in the active package only
\ (`TFAM-CONSTRUCT-FAM`, src/core/type-family.f), so a construct vector can only
\ be asked where the family is owned: asked from any other package the very same
\ text is refused for a reason that has nothing to do with the rule under test.
\ No other row here depends on the package, and every one of them answers the
\ same either way. test/compiler/checker-model-proof.f drives the same rows from
\ the same place for the same reason.
package CHECKER-MODEL-CASES
CHECKER-MODEL-MANIFEST-TEST:RUN
;package
