\ maki/move-facts-test.f - checked tests for movement fact encoding + verdicts.
\ Transform<->op-kind mapping, attrs pack/unpack round-trip, the 6.3 dissolution
\ classifiers, report gating + reason text, and every fail-closed range path.

require lib/test.f
require lib/string.f
require maki/move-facts.f

package MAKI

\ ---- fail-closed probes ----------------------------------------------------
: TRY-MVF-TF     ( -- )  MV-TF-N OP-OF-MV drop ;
: TRY-MVF-VD     ( -- )  MVV-N MV-VD-NAME 2drop ;
: TRY-MVF-PARAM  ( -- )  MV-RESHAPE MVV-FREE  MV-PMASK 1+  0  MV-PACK drop ;
: TRY-MVF-NOTMOVE ( -- ) OP-GELU MV-OF-OP drop ;

T-RESET

\ ---- op-kind <-> compact transform tag round-trip --------------------------
OP-RESHAPE   MV-OF-OP MV-RESHAPE   T=
OP-GATHER    MV-OF-OP MV-GATHER    T=
MV-SLICE     OP-OF-MV OP-SLICE     T=
OP-CONCAT    MV-OF-OP OP-OF-MV OP-CONCAT T=

\ ---- attrs pack / unpack round-trip ----------------------------------------
MV-SLICE MVV-MATERIALIZE 5 9 MV-PACK
dup MV-TF@ MV-SLICE        T=
dup MV-VD@ MVV-MATERIALIZE T=
dup MV-PA@ 5               T=
dup MV-PB@ 9               T=
drop
MV-RESHAPE MVV-FREE MV-PMASK 0 MV-PACK MV-PA@ MV-PMASK T=   \ 20-bit param survives

\ ---- verdict text ----------------------------------------------------------
MVV-FREE        MV-VD-NAME s" free"        T$=
MVV-STAGED      MV-VD-NAME s" staged"      T$=
MVV-MATERIALIZE MV-VD-NAME s" materialize" T$=
MVV-GATHERED    MV-VD-NAME s" gathered"    T$=

\ ---- dissolution classifiers (6.3) -----------------------------------------
LAY-ROW MV-RESHAPE-VERDICT MVV-FREE        T=   \ contiguous reshape dissolves
LAY-COL MV-RESHAPE-VERDICT MVV-MATERIALIZE T=   \ non-contiguous -> materialize
MV-TRANSPOSE-VERDICT       MVV-STAGED      T=
LAY-ROW 0 8 MV-SLICE-VERDICT MVV-FREE        T=  \ offset 0 lane-aligned
LAY-ROW 1 3 MV-SLICE-VERDICT MVV-MATERIALIZE T=  \ offset 3 not lane-aligned
LAY-COL 0 8 MV-SLICE-VERDICT MVV-MATERIALIZE T=  \ column-major rows strided
MV-CONCAT-VERDICT MVV-MATERIALIZE T=
MV-GATHER-VERDICT MVV-GATHERED    T=

\ ---- report gating + reason text -------------------------------------------
MVV-MATERIALIZE MV-VD-REPORTS? TTRUE
MVV-GATHERED    MV-VD-REPORTS? TTRUE
MVV-FREE        MV-VD-REPORTS? TFALSE
MVV-STAGED      MV-VD-REPORTS? TFALSE
OP-CONCAT MV-REASON$ s" concat" CONTAINS? TTRUE
OP-GATHER MV-REASON$ s" gathered" CONTAINS? TTRUE

\ ---- fail closed -----------------------------------------------------------
' TRY-MVF-TF      E-MV-TF      TTHROWS
' TRY-MVF-VD      E-MV-VD      TTHROWS
' TRY-MVF-PARAM   E-MV-PARAM   TTHROWS
' TRY-MVF-NOTMOVE E-MV-NOTMOVE TTHROWS

T-REPORT

end-package
