\ maki/lower-ew-test.f - host-side checked tests for the elementwise region lowering.
\
\ Off-device: builds captured models, lowers a region to PTX text (in-process capture,
\ src/arch/ptx/emit.f PTX-CAPTURE-ON), and asserts the flat-kernel shape - exactly one
\ .version header, exactly one REGION_<rid> entry, the per-input/out/n params, and the
\ expected op instructions for a GELU->RELU chain and a two-input ADD->RELU chain. Then
\ the fail-closed paths: non-elementwise region, unsupported op, the >4-input cap, and a
\ broadcast operand. No device, no ptxas - PTX validity is proven on the Orin by
\ maki/lower-device-test.f. Load via maki/test.f.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/lower-ew.f

package MAKI

\ ---- captured-text assertions (LEWT- prefix: shares the dictionary with siblings) --
variable LEWT-VA  variable LEWT-VU
: LEWT-SAVE ( ptr u8 n -- )  LEWT-VU ! LEWT-VA ! ;
: LEWT$ ( -- ptr u8 n )  LEWT-VA @ LEWT-VU @ ;
: LEWT-IN ( ptr u8 n -- )  LEWT$ 2swap CONTAINS? TTRUE ;   \ LEWT$ contains the needle

\ exactly-one: the needle occurs once (find it, then prove no second occurrence)
: LEWT-ONCE? ( ptr u8 n ptr u8 n -- bool ) {: ha:ptr hu:n na:ptr nu:n :}
   ha hu na nu FIND-SUB {: i1:n :}
   i1 0 < if false exit then
   ha i1 + nu +  hu i1 - nu -  na nu FIND-SUB 0 < ;
: LEWT-ONCE ( ptr u8 n -- )  LEWT$ 2swap LEWT-ONCE? TTRUE ;

: LEWT-CAP0 ( -- )  PTX-CAPTURE-ON  0 LEW-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ LEWT-SAVE ;

\ ---- fail-closed probes (each acts on the current model) --------------------
: LEWT-TRY-NOTEW  ( -- )  0 LEW-EMIT ;      \ non-elementwise region -> reject before emit
: LEWT-TRY-OP     ( -- )  0 LEW-ANALYZE ;   \ unsupported elementwise op
: LEWT-TRY-INPUTS ( -- )  0 LEW-ANALYZE ;   \ more than the v1 input cap
: LEWT-TRY-BCAST  ( -- )  0 LEW-ANALYZE ;   \ broadcast operand (not full N)

T-RESET

\ ---- GELU RELU: one flat kernel, gelu (ex2.approx) then relu (max.f32) -------
MODEL: GR ( x:4x8 -- y ) GELU RELU ;
FP-BUILD
LEWT-CAP0
s" .version 8.3"              LEWT-ONCE
s" .visible .entry REGION_0"  LEWT-ONCE
s" .param .u64 p_in0"         LEWT-IN
s" .param .u64 p_out"         LEWT-IN
s" .param .u32 p_n"           LEWT-IN
s" ld.param.u32 %r1, [p_n];"  LEWT-IN
s" ex2.approx.f32"            LEWT-IN
s" max.f32"                   LEWT-IN
s" DONE:"                     LEWT-IN
s" ret;"                      LEWT-IN

\ ---- two-input ADD RELU: two params, a binary add.rn.f32 --------------------
MODEL: AR ( a:2x4 b:2x4 -- y ) ADD RELU ;
FP-BUILD
LEWT-CAP0
s" .visible .entry REGION_0"  LEWT-ONCE
s" .param .u64 p_in0"         LEWT-IN
s" .param .u64 p_in1"         LEWT-IN
s" add.rn.f32"                LEWT-IN
s" max.f32"                   LEWT-IN

\ ---- fail closed: a row-reduction region is not elementwise -----------------
MODEL: LN ( x:4x8 -- y ) LAYERNORM ;
FP-BUILD
' LEWT-TRY-NOTEW E-LEW-NOTEW TTHROWS

\ ---- fail closed: CAST is elementwise but has no v1 device emitter ----------
MODEL: CS ( x:4x8 -- y ) CAST ;
FP-BUILD
' LEWT-TRY-OP E-LEW-OP TTHROWS

\ ---- fail closed: a 5-input add chain exceeds the v1 input cap (4) ----------
MODEL: A5 ( a:2x4 b:2x4 c:2x4 d:2x4 e:2x4 -- y ) ADD ADD ADD ADD ;
FP-BUILD
' LEWT-TRY-INPUTS E-LEW-INPUTS TTHROWS

\ ---- fail closed: a 1-row bias operand is a broadcast, unsupported in v1 -----
MODEL: BC ( x:4x8 b:1x8 -- y ) ADD ;
FP-BUILD
' LEWT-TRY-BCAST E-LEW-BCAST TTHROWS

T-REPORT

end-package
