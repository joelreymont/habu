\ cp-async-legal-test.f - regressions for the cp.async pipeline target-legality
\ gate (maki/cp-async-legal.f). Proves the "unsupported target rejects statically"
\ + "pipeline-depth legality" acceptance of dot habu-v2-checked-async-8d460576.
\
\ The gate is a static depth-vs-target legality check (NOT an emit-time per-slot
\ typestate - depth>1 emit-time typestate is permanently refuted, see the module
\ header). All probe descriptors are BUILT, never registered, so the suite never
\ touches the append-only, capped target registry (the target-test.f CAP-FILL
\ ordering coupling); the one interned-id case reuses the INIT-registered SM87.

require lib/test.f
require maki/cp-async-legal.f

package CPLEGAL-TEST

16384 constant BUFB   \ one cp.async buffer, the lib/ptx cg-mma.f MMA-BUFB default (16 KiB)

\ ephemeral sm_87-shaped descriptors, one varied field each (no registration).
: FULL-DESC ( -- TARGET:descriptor )
   TARGET:ISA-PTX 87 32 1024 49152 TARGET:CAP-ALL TARGET:DESCRIPTOR ;
: NOASYNC-DESC ( -- TARGET:descriptor )
   TARGET:ISA-PTX 87 32 1024 49152 TARGET:CAP-ALL TARGET:CAP-ASYNC invert and TARGET:DESCRIPTOR ;
: NOBAR-DESC ( -- TARGET:descriptor )
   TARGET:ISA-PTX 87 32 1024 49152 TARGET:CAP-ALL TARGET:CAP-BARRIER invert and TARGET:DESCRIPTOR ;
: SMALL-DESC ( -- TARGET:descriptor )
   TARGET:ISA-PTX 87 32 1024 16384 TARGET:CAP-ALL TARGET:DESCRIPTOR ;

: MAIN ( -- )
   \ POSITIVE: an async+barrier target with room certifies single- and double-buffer.
   s" single-buffer on a capable target is legal" T-LABEL
   BUFB 1 FULL-DESC CPLEGAL:LEGAL-DESC? TTRUE
   s" double-buffer on a capable target is legal" T-LABEL
   BUFB 2 FULL-DESC CPLEGAL:LEGAL-DESC? TTRUE

   \ NEGATIVE: no async copy engine -> the cp.async issue has no home, any depth rejects.
   s" no CAP-ASYNC target is illegal" T-LABEL
   BUFB 1 NOASYNC-DESC CPLEGAL:LEGAL-DESC? TFALSE
   [: BUFB 1 NOASYNC-DESC CPLEGAL:REQUIRE-DESC ;] E-CP-ASYNC-TGT TTHROWSQ

   \ NEGATIVE: no block barrier -> the WAIT fence (M5 committed->ready) has no home.
   s" no CAP-BARRIER target is illegal" T-LABEL
   BUFB 1 NOBAR-DESC CPLEGAL:LEGAL-DESC? TFALSE
   [: BUFB 1 NOBAR-DESC CPLEGAL:REQUIRE-DESC ;] E-CP-ASYNC-TGT TTHROWSQ

   \ NEGATIVE (depth-vs-target): a depth whose staged buffers overflow the budget rejects.
   s" over-budget depth on a capable target is illegal" T-LABEL
   BUFB 4 FULL-DESC CPLEGAL:LEGAL-DESC? TFALSE               \ 4*16K = 64K > 48K
   [: BUFB 4 FULL-DESC CPLEGAL:REQUIRE-DESC ;] E-CP-ASYNC-TGT TTHROWSQ

   \ NEGATIVE (small-smem target): double-buffer overflows a 16 KiB budget; single fits exactly.
   s" double-buffer overflows a small-smem target" T-LABEL
   BUFB 2 SMALL-DESC CPLEGAL:LEGAL-DESC? TFALSE             \ 2*16K = 32K > 16K
   s" single-buffer fits a small-smem target exactly" T-LABEL
   BUFB 1 SMALL-DESC CPLEGAL:LEGAL-DESC? TTRUE              \ 16K <= 16K

   \ PRODUCTION id path: the interned sm_87 id passes double-buffer, rejects over-budget.
   s" REQUIRE certifies double-buffer on the interned sm_87 id" T-LABEL
   [: BUFB 2 TARGET:SM87 CPLEGAL:REQUIRE ;] 0 TTHROWSQ
   s" REQUIRE rejects an over-budget depth on the interned sm_87 id" T-LABEL
   [: BUFB 4 TARGET:SM87 CPLEGAL:REQUIRE ;] E-CP-ASYNC-TGT TTHROWSQ

   T-REPORT ;

MAIN

;package
