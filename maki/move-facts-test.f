\ maki/move-facts-test.f - checked tests for movement fact encoding + verdicts.
\ Transform<->op-kind mapping, attrs pack/unpack round-trip, the 6.3 dissolution
\ classifiers, report gating + reason text, and every fail-closed range path.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/move-facts.f

package MAKI

\ ---- fail-closed probes ----------------------------------------------------
: TRY-MVF-TF     ( -- )  MV-TF-N OP-OF-MV drop ;
: TRY-MVF-VD     ( -- )  MVV-N MV-VD-NAME 2drop ;
: TRY-MVF-PARAM  ( -- )  MV-RESHAPE MVV-FREE  MV-PMASK 1+  0  MV-PACK drop ;
: TRY-MVF-NOTMOVE ( -- ) MAKI-OPKIND:GELU MV-OF-OP drop ;

T-RESET

\ ---- op-kind <-> compact transform tag round-trip --------------------------
MAKI-OPKIND:RESHAPE MV-OF-OP MV-RESHAPE   T=
MAKI-OPKIND:GATHER  MV-OF-OP MV-GATHER    T=
MV-SLICE     OP-OF-MV OPKIND>N OP-SLICE     T=
MAKI-OPKIND:CONCAT MV-OF-OP OP-OF-MV OPKIND>N OP-CONCAT T=

\ ---- attrs pack / unpack round-trip ----------------------------------------
MV-SLICE MVV-MATERIALIZE 5 9 MV-PACK
dup MV-TF@ MV-SLICE        T=
dup MV-VD@ MVV-MATERIALIZE T=
dup MV-PA@ 5               T=
dup MV-PB@ 9               T=
drop
MV-RESHAPE MVV-FREE MV-PMASK 0 MV-PACK MV-PA@ MV-PMASK T=   \ 20-bit param survives

\ ---- typed extent packers (the confined raw boundary; R3) -------------------
MV-RESHAPE MVV-FREE 8 4 SHAPE MV-PACK-SHAPE
dup MV-TF@ MV-RESHAPE T=
dup MV-VD@ MVV-FREE   T=
dup MV-PA@ 8          T=   \ param A = target rows
dup MV-PB@ 4          T=   \ param B = target cols
drop
MV-SLICE MVV-MATERIALIZE 5 1 SHAPE drop 9 1 SHAPE drop MV-PACK-ROWS
dup MV-PA@ 5          T=   \ param A = r0
dup MV-PB@ 9          T=   \ param B = r1
drop

\ swapped shape roles into a typed packer reject BEFORE runtime
s" MVF-PACK-SHAPE-OK ( n n CAD-KIND:rows CAD-KIND:cols -- n ) MV-PACK-SHAPE" CHECK-QUIET-CANDIDATE! -1 T=
s" MVF-NEG-PACK-SWAP ( n n CAD-KIND:cols CAD-KIND:rows -- n ) MV-PACK-SHAPE" CHECK-QUIET-CANDIDATE! 0 T=
s" MVF-NEG-PACK-RAW ( n n n n -- n ) MV-PACK-SHAPE" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- verdict text ----------------------------------------------------------
MVV-FREE        MV-VD-NAME s" free"        T$=
MVV-STAGED      MV-VD-NAME s" staged"      T$=
MVV-MATERIALIZE MV-VD-NAME s" materialize" T$=
MVV-GATHERED    MV-VD-NAME s" gathered"    T$=

\ ---- dissolution classifiers (6.3) -----------------------------------------
MAKI-LAYOUT:ROW MV-RESHAPE-VERDICT MVV-FREE        T=   \ contiguous reshape dissolves
MAKI-LAYOUT:COL MV-RESHAPE-VERDICT MVV-MATERIALIZE T=   \ non-contiguous -> materialize
MV-TRANSPOSE-VERDICT       MVV-STAGED      T=
MAKI-LAYOUT:ROW 0 8 MV-SLICE-VERDICT MVV-FREE        T=  \ offset 0 lane-aligned
MAKI-LAYOUT:ROW 1 3 MV-SLICE-VERDICT MVV-MATERIALIZE T=  \ offset 3 not lane-aligned
MAKI-LAYOUT:COL 0 8 MV-SLICE-VERDICT MVV-MATERIALIZE T=  \ column-major rows strided
MV-CONCAT-VERDICT MVV-MATERIALIZE T=
MV-GATHER-VERDICT MVV-GATHERED    T=

\ typed slice verdict mirrors the raw classifier; swapped roles reject
MAKI-LAYOUT:ROW 0 8 SHAPE MV-SLICE-VD MVV-FREE        T=
MAKI-LAYOUT:ROW 1 3 SHAPE MV-SLICE-VD MVV-MATERIALIZE T=
MAKI-LAYOUT:COL 0 8 SHAPE MV-SLICE-VD MVV-MATERIALIZE T=
s" MVF-NEG-VD-SWAP ( layout CAD-KIND:cols CAD-KIND:rows -- n ) MV-SLICE-VD" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- report gating + reason text -------------------------------------------
MVV-MATERIALIZE MV-VD-REPORTS? TTRUE
MVV-GATHERED    MV-VD-REPORTS? TTRUE
MVV-FREE        MV-VD-REPORTS? TFALSE
MVV-STAGED      MV-VD-REPORTS? TFALSE
MAKI-OPKIND:CONCAT MV-REASON$ s" concat" CONTAINS? TTRUE
MAKI-OPKIND:GATHER MV-REASON$ s" gathered" CONTAINS? TTRUE

\ ---- fail closed -----------------------------------------------------------
' TRY-MVF-TF      E-MV-TF      TTHROWS
' TRY-MVF-VD      E-MV-VD      TTHROWS
' TRY-MVF-PARAM   E-MV-PARAM   TTHROWS
' TRY-MVF-NOTMOVE E-MV-NOTMOVE TTHROWS

T-REPORT

;package
