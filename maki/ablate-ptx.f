\ maki/ablate-ptx.f - fail-closed post-emit PTX text mutation (seeded golden faults).
\
\ Shared by the device ablation tests (maki/ablate-golden-device-test.f,
\ maki/precision-device-test.f): copy a captured PTX module, replacing the FIRST
\ occurrence of a target substring - which MUST exist (the emitters are never
\ edited), so a drifted emitter surfaces as E-ABL-NOSUB, never as a silent no-op
\ mutation that would let a "caught fault" assertion pass vacuously. The mutated
\ text is what the test hands to ptxas. maki -> habu only; ablate-ptx owns
\ -5250..-5251 (moved off -5210..-5211, which maki/lower-move.f already owned;
\ tools/error-code-lint.f now enforces uniqueness).

require lib/errors.f
require lib/string.f

-5250 constant E-ABL-NOSUB        \ ablate: PTX mutation target substring not found (fail closed)
-5251 constant E-ABL-CAP          \ ablate: mutated PTX exceeds the scratch buffer

package MAKI
private

$10000 constant ABL-PTX-CAP       \ mutated-module scratch (blocked-tile PTX is ~28 KB)
create ABL-PTX ABL-PTX-CAP allot  variable ABL-PTX-U

public

\ ---- PTX text mutator: copy src, replace the FIRST tgt with rep (fail closed) ----
: ABL-MUTATE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n ra:ptr ru:n :}
   su ru + tu - ABL-PTX-CAP > if E-ABL-CAP throw then
   sa su ta tu FIND-SUB {: pos:n :}
   pos 0 < if E-ABL-NOSUB throw then
   sa ABL-PTX pos BYTE-COPY                        \ prefix [0,pos)
   ra ABL-PTX pos + ru BYTE-COPY                   \ replacement
   sa pos + tu +  ABL-PTX pos + ru +  su pos - tu -  BYTE-COPY   \ suffix (past the target)
   pos ru + su pos - tu - + ABL-PTX-U ! ;
: ABL-PTX$ ( -- ptr u8 n )  ABL-PTX ABL-PTX-U @ ;

end-package
