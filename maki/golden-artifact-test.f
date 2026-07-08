\ maki/golden-artifact-test.f - checked tests for external GOLDEN reference artifacts.
\ save->load->check round-trips; a corrupted / out-of-tolerance stored output FAILs; a
\ within-tolerance perturbation still PASSes; a missing artifact is not-run; a save on a
\ non-host-executable model fails closed; and GOLDEN-INTO prefers an artifact over the
\ self-consistency v1. The store lives under a throwaway CAD store root (STORE-RESET).

require lib/test.f
require lib/string.f
require lib/float.f
require maki/report.f
require maki/cad.f
require maki/golden-artifact.f
require maki/golden.f

package MAKI

variable GT-VA  variable GT-VU
: GT-SAVE ( ptr u8 n -- )  GT-VU ! GT-VA ! ;
: GT-IN ( ptr u8 n -- )  GT-VA @ GT-VU @ 2swap CONTAINS? TTRUE ;

: TRY-SAVE-CAST ( -- )  GA-SAVE ;

T-RESET
STORE-RESET

\ ---- host-executability membership -----------------------------------------
MODEL: GA-SUP ( x:2x3 w:3x4 b:1x4 -- y ) LINEAR ;
GA-SUPPORTED? TTRUE
MODEL: GA-CST ( x:2x2 -- y ) CAST ;
GA-SUPPORTED? TFALSE

\ ---- save -> exists -> check round-trip PASSes ------------------------------
MODEL: GA-RT ( x:2x3 w:3x4 b:1x4 -- y ) LINEAR ;
GA-EXISTS? TFALSE
GA-SAVE
GA-EXISTS? TTRUE
GA-CHECK V-PASS T=
GA-RE$ GT-SAVE  s" external artifact GA-RT matched" GT-IN

\ ---- load binds the inputs + loads the expected output; verdict is real ------
GA-LOAD TTRUE
GA-VERDICT V-PASS T=

\ ---- a corrupted stored output value FAILs (beyond tolerance) ----------------
GA-LOAD TTRUE
1000.0 0 GA-EXP!
GA-VERDICT V-FAIL T=
GA-RE$ GT-SAVE  s" mismatch beyond tolerance" GT-IN

\ ---- tolerance respected: a within-atol perturbation still PASSes ------------
GA-LOAD TTRUE
0 GA-EXP@  0.0000001 f+  0 GA-EXP!        \ +1e-7 << atol 1e-6 + rtol*|b|
GA-VERDICT V-PASS T=

\ ---- ...but an out-of-tolerance perturbation FAILs --------------------------
GA-LOAD TTRUE
0 GA-EXP@  0.001 f+  0 GA-EXP!            \ +1e-3 >> the tolerance band
GA-VERDICT V-FAIL T=

\ ---- a missing artifact is not-run (load returns false, check is V-NOTRUN) ---
MODEL: GA-NONE ( x:2x2 -- y ) GELU ;
GA-EXISTS? TFALSE
GA-LOAD TFALSE
GA-CHECK V-NOTRUN T=
GA-RE$ GT-SAVE  s" no external reference artifact" GT-IN

\ ---- a non-host-executable model is not-run, and GA-SAVE fails closed --------
MODEL: GA-CST2 ( x:2x2 -- y ) CAST ;
GA-CHECK V-NOTRUN T=
GA-RE$ GT-SAVE  s" not host-executable" GT-IN
' TRY-SAVE-CAST E-GA-UNSUP TTHROWS

\ ---- GOLDEN-INTO prefers the external artifact (real comparison) -------------
MODEL: GA-RT ( x:2x3 w:3x4 b:1x4 -- y ) LINEAR ;
GA-EXISTS? TTRUE
GOLDEN
dup G-GOLDEN REPORT:GATE-TAG@ V-PASS T=
dup G-GOLDEN REPORT:GATE-REASON@ GT-SAVE  s" external artifact GA-RT matched" GT-IN
dup REPORT:RENDER GT-SAVE  s" golden: external reference artifact comparison" GT-IN
drop

\ ---- ...and falls back to self-consistency when no artifact exists -----------
MODEL: GA-SELF ( x:2x3 w:3x4 b:1x4 -- y ) LINEAR ;
GA-EXISTS? TFALSE
GOLDEN
dup G-GOLDEN REPORT:GATE-TAG@ V-PASS T=
dup G-GOLDEN REPORT:GATE-REASON@ GT-SAVE  s" host self-consistent" GT-IN
drop

STORE-RESET
T-REPORT

end-package
