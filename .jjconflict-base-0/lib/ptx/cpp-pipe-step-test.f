\ cpp-pipe-step-test.f - exact-byte regression for the decomposed cp.async pipeline
\ STEP emitters (CPP-* in lib/ptx/cg-matmul-emit.f).
\
\ The fused cp.async K-loop (MM-PIPE-KLOOP-WITH / cg-mma.f MMA-PIPE-KLOOP-WITH /
\ MMA-PIPE-KLOOP-SINGLE) is composed from small named protocol steps so the future
\ cp.async typestate capability (habu-checker-cp-async-6ba788a5) can attach a per-step
\ obligation. This file PINS each step's emitted bytes: capture the step under PTX-CAPTURE
\ and assert the exact PTX fragment, so a drift in any single step fails here (the
\ byte-capture pattern of lib/ptx/tile-pipe-test.f, applied at the step granularity). The
\ full-loop byte-identity across every pinned config is proven by the whole-kernel goldens
\ (lib/ptx/tile-pipe-test.f EMIT-PIPED==EMIT-MATMUL, and the off-device MMM config dump).
\ Load after lib/ptx/cg-matmul.f.

require lib/ptx/test-prelude.f
require lib/ptx/cg-matmul.f

package CPPSTEP
private

\ capture one emitting step (or step sequence) and assert its exact PTX bytes
: STEP= ( ptr u8 n -- )                        \ ( expected ) ; compares the pending capture
   PTX-CAPTURE$ 2swap T-STR= TTRUE ;

: MAIN ( -- )
   T-RESET
   256 %BLOCK

   s" CPP-KT-INIT" T-LABEL
   PTX-CAPTURE-ON CPP-KT-INIT PTX-CAPTURE-OFF
   s\" mov.u32 %r14,0;\n" STEP=

   s" CPP-PARITY-INIT" T-LABEL
   PTX-CAPTURE-ON CPP-PARITY-INIT PTX-CAPTURE-OFF
   s\" mov.u32 %r15,0;\n" STEP=

   s" CPP-COMMIT" T-LABEL
   PTX-CAPTURE-ON CPP-COMMIT PTX-CAPTURE-OFF
   s\" cp.async.commit_group;\n" STEP=

   s" CPP-WAIT 1 (keep prefetch in flight)" T-LABEL
   PTX-CAPTURE-ON 1 CPP-WAIT PTX-CAPTURE-OFF
   s\" cp.async.wait_group 1;\n" STEP=

   s" CPP-WAIT 0 (drain)" T-LABEL
   PTX-CAPTURE-ON 0 CPP-WAIT PTX-CAPTURE-OFF
   s\" cp.async.wait_group 0;\n" STEP=

   s" CPP-SYNC" T-LABEL
   PTX-CAPTURE-ON CPP-SYNC PTX-CAPTURE-OFF
   s\" bar.sync 0;\n" STEP=

   s" CPP-FLIP" T-LABEL
   PTX-CAPTURE-ON CPP-FLIP PTX-CAPTURE-OFF
   s\" xor.b32 %r15,%r15,1;\n" STEP=

   s" CPP-SINGLE-WINDOW" T-LABEL
   PTX-CAPTURE-ON CPP-SINGLE-WINDOW PTX-CAPTURE-OFF
   s\" mov.u32 %r16,%r11;\n" STEP=

   s" CPP-CUR-WINDOW (read-window, bufb=MM-BUFB)" T-LABEL
   PTX-CAPTURE-ON MM-BUFB CPP-CUR-WINDOW PTX-CAPTURE-OFF
   s\" mul.lo.u32 %r16,%r15,16384;\nadd.u32 %r16,%r11,%r16;\n" STEP=

   s" CPP-NEXT-WINDOW (prefetch dst, bufb=MM-BUFB)" T-LABEL
   PTX-CAPTURE-ON MM-BUFB CPP-NEXT-WINDOW PTX-CAPTURE-OFF
   s\" xor.b32 %r18,%r15,1;\nmul.lo.u32 %r18,%r18,16384;\nadd.u32 %r18,%r11,%r18;\n" STEP=

   s" CPP-KT-NEXT (bk=MM-BK)" T-LABEL
   PTX-CAPTURE-ON MM-BK CPP-KT-NEXT PTX-CAPTURE-OFF
   s\" add.u32 %r17,%r14,32;\n" STEP=

   s" CPP-KT-ADVANCE (bk=MM-BK)" T-LABEL
   PTX-CAPTURE-ON MM-BK CPP-KT-ADVANCE PTX-CAPTURE-OFF
   s\" add.u32 %r14,%r14,32;\n" STEP=

   s" CPP-KGUARD" T-LABEL
   PTX-CAPTURE-ON CPP-KGUARD PTX-CAPTURE-OFF
   s\" $KLOOP:\nsetp.ge.u32 %p1,%r14,%r3;\n@%p1 bra $KEND;\n" STEP=

   s" CPP-KTAIL" T-LABEL
   PTX-CAPTURE-ON CPP-KTAIL PTX-CAPTURE-OFF
   s\" bra $KLOOP;\n$KEND:\n" STEP=

   s" CPP-PF-TEST" T-LABEL
   PTX-CAPTURE-ON CPP-PF-TEST PTX-CAPTURE-OFF
   s\" setp.lt.u32 %p2,%r17,%r3;\n@!%p2 bra $NOPF;\n" STEP=

   s" CPP-PF-ELSE" T-LABEL
   PTX-CAPTURE-ON CPP-PF-ELSE PTX-CAPTURE-OFF
   s\" bra $PFDONE;\n$NOPF:\n" STEP=

   s" CPP-PF-END" T-LABEL
   PTX-CAPTURE-ON CPP-PF-END PTX-CAPTURE-OFF
   s\" $PFDONE:\n" STEP=

   \ parameterized read-window: a non-MM buffer size threads through unchanged (the MMA
   \ non-default configs pass MMA-BUFB / MMA-BK @ into the same steps)
   s" CPP-CUR-WINDOW parameterized (bufb=20480)" T-LABEL
   PTX-CAPTURE-ON 20480 CPP-CUR-WINDOW PTX-CAPTURE-OFF
   s\" mul.lo.u32 %r16,%r15,20480;\nadd.u32 %r16,%r11,%r16;\n" STEP=

   \ composition: the steps assemble into the real K-loop scaffold inside EMIT-MATMUL
   PTX-CAPTURE-ON EMIT-MATMUL PTX-CAPTURE-OFF PTX-CAPTURE$ {: pa:ptr pu:n :}
   s" loop assembles: $KLOOP present" T-LABEL   pa pu s" $KLOOP:" CONTAINS? TTRUE
   s" loop assembles: commit present" T-LABEL   pa pu s" cp.async.commit_group;" CONTAINS? TTRUE
   s" loop assembles: wait 1 present" T-LABEL    pa pu s" cp.async.wait_group 1;" CONTAINS? TTRUE
   s" loop assembles: parity flip present" T-LABEL  pa pu s" xor.b32 %r15,%r15,1;" CONTAINS? TTRUE

   T-REPORT ;

MAIN

;package
