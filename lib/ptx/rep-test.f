\ rep-test.f - positive coverage for the phantom-preserving register-emitter
\ combinators (lib/ptx/rep.f, package PTXREP).
\
\ Two positive facts:
\   1. Certification — a kernel-token op that PRESERVES its operand phantom
\      certifies as CHECKED code through REP1/REP2/REPMIX2 (the RT-PHANTOM-*
\      definitions load; a reject would fail this file).
\   2. Runtime — the emitter runs on the operands' register cells and the result
\      is returned unchanged, so a converted wrapper lowers identically.

require lib/ptx/test-prelude.f

256 %BLOCK

\ (1) certification: the phantom flows THROUGH a checked emitter and the SAME
\ family (tile / uniform / acc) is returned, or (SINK*) is CONSUMED with no phantom
\ minted. These certify at load.
: RT-PHANTOM-UNARY ( tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-RELU ;] PTXREP:REP1 ;
: RT-PHANTOM-BINARY ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-ADD ;] PTXREP:REP2 ;
: RT-PHANTOM-MIX ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   [: EMIT-SCALE ;] PTXREP:REPMIX2 ;
\ REPMIX3 preserves the FIRST operand (ACC-FMA: acc ∘ tile ∘ tile -> acc);
\ REPMIX3B preserves the SECOND (FMA.: uniform ∘ tile ∘ tile -> tile).
: RT-PHANTOM-MIX3 ( acc<t,b,m> tile<t,b,m> tile<t,b,m> -- acc<t,b,m> )
   [: EMIT-ACC-FMA ;] PTXREP:REPMIX3 ;
: RT-PHANTOM-MIX3B ( uniform<t> tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-FMA ;] PTXREP:REPMIX3B ;
\ SINK3 / SINK4 consume the operand phantoms and return nothing (the store class).
: RT-PHANTOM-SINK3 ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- )
   [: EMIT-STORE ;] PTXREP:SINK3 ;
: RT-PHANTOM-SINK4 ( tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> uniqidxctx<b,i,e,m> -- )
   [: EMIT-INDEX-STORE ;] PTXREP:SINK4 ;

\ (2) runtime: the combinators are register-level applications of the quoted
\ emitter (proven here with arithmetic quotations over register cells).
variable RT-SINK

: RT-MAIN ( -- )
   T-RESET
   s" REP1 applies a unary emitter to the operand register" T-LABEL
   5 [: 1+ ;] PTXREP:REP1  6 T=
   s" REP2 applies a binary emitter to both operand registers" T-LABEL
   3 4 [: + ;] PTXREP:REP2  7 T=
   s" REPMIX2 applies a binary emitter, first operand order preserved" T-LABEL
   10 3 [: - ;] PTXREP:REPMIX2  7 T=
   s" REPMIX3 applies a ternary emitter over all three operand registers" T-LABEL
   100 5 7 [: + + ;] PTXREP:REPMIX3  112 T=
   s" REPMIX3B applies a ternary emitter over all three operand registers" T-LABEL
   100 5 7 [: + + ;] PTXREP:REPMIX3B  112 T=
   s" SINK3 runs a ternary sink emitter over the three operand registers" T-LABEL
   0 RT-SINK !
   1 2 3 [: + + RT-SINK ! ;] PTXREP:SINK3  RT-SINK @ 6 T=
   s" SINK4 runs a 4-ary sink emitter over the four operand registers" T-LABEL
   0 RT-SINK !
   1 2 3 4 [: + + + RT-SINK ! ;] PTXREP:SINK4  RT-SINK @ 10 T=
   T-REPORT ;

RT-MAIN
