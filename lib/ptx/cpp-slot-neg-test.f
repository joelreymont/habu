\ cpp-slot-neg-test.f - committed negative regressions for the cp.async
\ pipeline-slot typestate (lib/ptx/cpp-slot.f).
\
\ The four DYNAMIC pipeline-protocol negatives the dot habu-checker-cp-async-6ba788a5
\ owns - the ones the tilepipe surface could not express while the protocol was
\ fused inside MM-PIPE-KLOOP-WITH - now reject fail-closed by state-family +
\ parity unification, plus the barrier-composition negative:
\
\   N1 read-before-wait  READ demands a cpp-ready slot; a cpp-pending (issued, not
\                        yet committed/waited) slot does not unify.
\   N2 missing-commit    WAIT demands a cpp-committed slot; a cpp-pending slot
\                        (never committed) does not unify.
\   N3 double-wait       WAIT demands a cpp-committed slot; after one WAIT the slot
\                        is cpp-ready, so a second WAIT does not unify.
\   N4 parity mismatch   READ-STAGE ties the ready slot's parity to the staged
\                        tile's; a ready<parity-a> against an mmstage of parity-b
\                        is a cross-buffer read - rejected.
\   N5 divergent barrier WAIT is a bar.sync fence (committed->ready is CTL-BARRIER);
\                        a WAIT reached inside an open control frame is not
\                        block-uniform and rejects, composing with the M5 model.
\
\ The SAME-BODY parity discipline; the runtime loop-carried parity ALTERNATION
\ across $KLOOP iterations stays the trusted MM-PIPE-KLOOP-WITH boundary (a
\ runtime-dataflow property no emit-time stack-effect checker expresses).

require lib/ptx/neg-test-lib.f
require lib/ptx/cpp-slot.f

: CSN-MAIN ( -- )
   T-RESET
   256 %BLOCK

   s" BAD-RBW ( cpp-pending<p> -- ) CPPSLOT:READ"
   s" cpp-ready" s" cpp-slot read-before-wait negative" PTXN-REJECTS
   s" NEG: READ of a pending (unwaited) slot rejected (needs cpp-ready)" type cr

   s" BAD-MC ( cpp-pending<p> -- ) CPPSLOT:WAIT CPPSLOT:READ"
   s" cpp-committed" s" cpp-slot missing-commit negative" PTXN-REJECTS
   s" NEG: WAIT of a pending (uncommitted) slot rejected (needs cpp-committed)" type cr

   s" BAD-DW ( cpp-committed<p> -- ) CPPSLOT:WAIT CPPSLOT:WAIT CPPSLOT:READ"
   s" cpp-committed" s" cpp-slot double-wait negative" PTXN-REJECTS
   s" NEG: second WAIT of an already-ready slot rejected (needs cpp-committed)" type cr

   s" BAD-PAR ( mmstage<f32,block-256,geom-as64x32-bs32x64,mask-live,parity-b> cpp-ready<parity-a> -- mmstage<f32,block-256,geom-as64x32-bs32x64,mask-live,parity-b> ) CPPSLOT:READ-STAGE"
   s" parity-a" s" cpp-slot parity-mismatch negative" PTXN-REJECTS
   s" NEG: READ-STAGE of a slot whose parity differs from the tile rejected (same-buffer rule)" type cr

   s" BAD-DIVBAR ( cpp-committed<p> f -- ) IF CPPSLOT:WAIT CPPSLOT:READ THEN"
   s" divergent barrier" s" cpp-slot divergent-barrier negative" PTXN-REJECTS
   s" NEG: WAIT (bar.sync fence) under divergent control rejected (M5 compose)" type cr

   T-REPORT ;

CSN-MAIN
