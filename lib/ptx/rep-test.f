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
\ family (tile / uniform) is returned. These certify at load.
: RT-PHANTOM-UNARY ( tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-RELU ;] PTXREP:REP1 ;
: RT-PHANTOM-BINARY ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-ADD ;] PTXREP:REP2 ;
: RT-PHANTOM-MIX ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   [: EMIT-SCALE ;] PTXREP:REPMIX2 ;

\ (2) runtime: the combinators are register-level applications of the quoted
\ emitter (proven here with arithmetic quotations over register cells).
: RT-MAIN ( -- )
   T-RESET
   s" REP1 applies a unary emitter to the operand register" T-LABEL
   5 [: 1+ ;] PTXREP:REP1  6 T=
   s" REP2 applies a binary emitter to both operand registers" T-LABEL
   3 4 [: + ;] PTXREP:REP2  7 T=
   s" REPMIX2 applies a binary emitter, first operand order preserved" T-LABEL
   10 3 [: - ;] PTXREP:REPMIX2  7 T=
   T-REPORT ;

RT-MAIN
