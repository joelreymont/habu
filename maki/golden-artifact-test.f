\ maki/golden-artifact-test.f - checked tests for external GOLDEN reference artifacts.
\ save->load->check round-trips; a corrupted / out-of-tolerance stored output FAILs; a
\ within-tolerance perturbation still PASSes; a missing artifact is not-run; a save on a
\ non-host-executable model fails closed; GOLDEN-INTO prefers an artifact over the
\ self-consistency v1; and GA-BIND-SYNTH fills gather INDEX slots with a varied in-range
\ row permutation whose executed output discriminates real row selection from both a
\ row-0-only fill and an index-ignoring positional copy. The store lives under a
\ throwaway CAD store root (STORE-RESET).

require lib/test.f
require lib/string.f
require lib/float.f
require maki/report.f
require maki/cad.f
require maki/golden-artifact.f
require maki/golden.f

package MAKI

: GT-IN-PTR ( n -- ptr a )  MIR-SLOT-ID GA-IN-PTR ;

variable GT-VA  variable GT-VU
: GT-SAVE ( ptr u8 n -- )  GT-VU ! GT-VA ! ;
: GT-IN ( ptr u8 n -- )  GT-VA @ GT-VU @ 2swap CONTAINS? TTRUE ;

: TRY-SAVE-CAST ( -- )  GA-SAVE ;
: TRY-GA-PI ( -- )  s" nope" GA-PARSE-INT-VAL drop ;    \ non-numeric artifact value field

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
' TRY-GA-PI      E-GA-PARSE  TTHROWS

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

\ ---- gather index slots fill with a varied in-range row permutation ----------
\ GA-BIND-SYNTH fills a GATHER index slot with src_rows-1 - (e mod src_rows): for
\ x:4x2 idx:3x1 that is {3,2,1} - deterministic, in-range, non-constant and
\ non-identity - so the golden exercises a real index->row mapping, not row 0 only.
MODEL: GA-GAT ( x:4x2 idx:3x1 -- y ) GATHER ;
GA-BIND-SYNTH
1 GT-IN-PTR 0 T-GET 0.5 f+ f>s 3 T=
1 GT-IN-PTR 1 T-GET 0.5 f+ f>s 2 T=
1 GT-IN-PTR 2 T-GET 0.5 f+ f>s 1 T=

\ ---- ...and the executed gather output under that fill DISCRIMINATES ---------
\ y row0 = x row idx[0]=3 (flat 6 -> 6*0.17+0.4 = 1.42). The old all-0.0 fill and
\ an index-IGNORING positional-copy kernel would both put x[0,0] = 0.40 there, so
\ this output separates real row selection from both failure shapes.
MIR-N@ EX-RUN-N
0 MIR-NODE-ID EX-OUT@ 0 T-GET 100.0 f* 0.5 f+ f>s 142 T=      \ y[0,0] = x[3,0], NOT...
0 GT-IN-PTR 0 T-GET 100.0 f* 0.5 f+ f>s  40 T=    \ ...the row-0 value 0.40
0 MIR-NODE-ID EX-OUT@ 4 T-GET 100.0 f* 0.5 f+ f>s  74 T=      \ y[2,0] = x[1,0] (flat 2: 0.74)

\ ---- gather artifact round-trip: save -> check PASSes under the varied fill ---
GA-SAVE
GA-CHECK V-PASS T=

STORE-RESET
T-REPORT

;package
