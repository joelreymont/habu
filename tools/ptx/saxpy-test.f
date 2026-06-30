\ saxpy-test.f - focused tests for PTX text and checked-codegen output.
\
\ Load after lib/errors.f, lib/string.f, lib/test.f, lib/fs.f, lib/process.f,
\ lib/process-argv.f, lib/process-env.f, and src/arch/ptx/emit.f.

require lib/ptx/test-prelude.f
require lib/ptx/process-test-prelude.f

16384 constant PTXT-CAP
10000 constant PTXT-TIMEOUT-MS

create PTXT-OUT PTXT-CAP allot
create PTXT-ERR PTXT-CAP allot

variable PTXT-OUT-U

: PTXT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: PTXT-HAS ( ptr u8 n -- ) {: a:ptr u :}
   PTXT-OUT PTXT-OUT-U @ a u CONTAINS? TTRUE ;

: PTXT-NOT-HAS ( ptr u8 n -- ) {: a:ptr u :}
   PTXT-OUT PTXT-OUT-U @ a u CONTAINS? 0= TTRUE ;

: PTXT-RUN-SAXPY ( -- n n n )
   PROC-ARGV-ENV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+
   s" tools/ptx/saxpy.f"  >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN PTXT-OUT PTXT-CAP >LEN PTXT-ERR PTXT-CAP >LEN
   PTXT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   PTXT-CAPTURE>N ;

: PTXT-RUN-OPS-CG ( -- n n n )
   PROC-ARGV-ENV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"  >LEN PROC-ARGV+
   s" lib/fmt.f"  >LEN PROC-ARGV+
   s" lib/test.f"  >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg-vec.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"  >LEN PROC-ARGV+
   s" lib/ptx/tile.f"  >LEN PROC-ARGV+
   s" lib/ptx/tile-v4.f"  >LEN PROC-ARGV+
   s" tools/ptx/ops-cg.f"  >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN PTXT-OUT PTXT-CAP >LEN PTXT-ERR PTXT-CAP >LEN
   PTXT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   PTXT-CAPTURE>N ;

: PTXT-RUN-ONCE-CG ( -- n n n )
   PROC-ARGV-ENV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"  >LEN PROC-ARGV+
   s" lib/fmt.f"  >LEN PROC-ARGV+
   s" lib/test.f"  >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"  >LEN PROC-ARGV+
   s" lib/ptx/tile.f"  >LEN PROC-ARGV+
   s" tools/ptx/once-cg.f"  >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN PTXT-OUT PTXT-CAP >LEN PTXT-ERR PTXT-CAP >LEN
   PTXT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   PTXT-CAPTURE>N ;

: PTXT-RUN-SOFTMAX-CG ( -- n n n )
   PROC-ARGV-ENV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"  >LEN PROC-ARGV+
   s" lib/fmt.f"  >LEN PROC-ARGV+
   s" lib/test.f"  >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg-collective.f"  >LEN PROC-ARGV+
   s" lib/ptx/collective.f"  >LEN PROC-ARGV+
   s" tools/ptx/softmax-cg.f"  >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN PTXT-OUT PTXT-CAP >LEN PTXT-ERR PTXT-CAP >LEN
   PTXT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   PTXT-CAPTURE>N ;

: PTXT-RUN-SUM-CG ( -- n n n )
   PROC-ARGV-ENV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"  >LEN PROC-ARGV+
   s" lib/fmt.f"  >LEN PROC-ARGV+
   s" lib/test.f"  >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg-collective.f"  >LEN PROC-ARGV+
   s" lib/ptx/collective.f"  >LEN PROC-ARGV+
   s" tools/ptx/sum-cg.f"  >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN PTXT-OUT PTXT-CAP >LEN PTXT-ERR PTXT-CAP >LEN
   PTXT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   PTXT-CAPTURE>N ;

: PTXT-RUN-SUM1024-CG ( -- n n n )
   PROC-ARGV-ENV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"  >LEN PROC-ARGV+
   s" lib/fmt.f"  >LEN PROC-ARGV+
   s" lib/test.f"  >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"  >LEN PROC-ARGV+
   s" lib/ptx/cg-collective.f"  >LEN PROC-ARGV+
   s" lib/ptx/collective.f"  >LEN PROC-ARGV+
   s" tools/ptx/sum1024-cg.f"  >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN PTXT-OUT PTXT-CAP >LEN PTXT-ERR PTXT-CAP >LEN
   PTXT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   PTXT-CAPTURE>N ;

: PTXT-SAXPY-OUTPUT ( -- )
   PTXT-RUN-SAXPY 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .version 8.3" PTXT-HAS
   s" .target sm_87" PTXT-HAS
   s" .visible .entry SAXPY" PTXT-HAS
   s" ld.param.u64 %rd1, [p_x];" PTXT-HAS
   s" mad.lo.u32 %r5, %r2, %r3, %r4;" PTXT-HAS
   s" setp.ge.u32 %p1, %r5, %r1;" PTXT-HAS
   s" mul.rn.f32 %f4, %f1, %f2;" PTXT-HAS
   s" add.rn.f32 %f4, %f4, %f3;" PTXT-HAS
   s" st.global.f32 [%rd5], %f4;" PTXT-HAS
   s" DONE:" PTXT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-OPS-CG-OUTPUT ( -- )
   PTXT-RUN-OPS-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" sub.rn.f32 %f4, %f2, %f3;" PTXT-HAS
   s" div.rn.f32 %f6, %f4, %f5;" PTXT-HAS
   s" fma.rn.f32" PTXT-HAS
   s" sub.rn.f32 %f10, %f2, %f6;" PTXT-HAS
   s" red.global.add.f32" PTXT-HAS
   s" div.rn.f32 %f18, %f10, %f14;" PTXT-HAS
   s" ld.global.v4.f32 {%f2, %f3, %f4, %f5}, [%rd4];" PTXT-HAS
   s" @%p3 ld.global.f32 %f2, [%rd4];" PTXT-HAS
   s" @%p6 ld.global.f32 %f5, [%rd7];" PTXT-HAS
   s" st.global.v4.f32 [%rd19], {%f18, %f19, %f20, %f21};" PTXT-HAS
   s" @%p18 st.global.f32 [%rd19], %f18;" PTXT-HAS
   s" @%p21 st.global.f32 [%rd22], %f21;" PTXT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-ONCE-CG-OUTPUT ( -- )
   PTXT-RUN-ONCE-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry ONCE_SPAN" PTXT-HAS
   s" ld.global.f32" PTXT-HAS
   s" st.global.f32" PTXT-HAS
   s" red.global.add.f32" PTXT-NOT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SOFTMAX-CG-OUTPUT ( -- )
   PTXT-RUN-SOFTMAX-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .shared .align 4 .b8 SMEM[1024];" PTXT-HAS
   s" mov.f32 %f2, 0fFF800000;" PTXT-HAS
   s" @%p2 mov.f32 %f2, %f1;" PTXT-HAS
   s" mov.f32 %f8, 0f00000000;" PTXT-HAS
   s" @%p5 mov.f32 %f8, %f7;" PTXT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SUM-CG-OUTPUT ( -- )
   PTXT-RUN-SUM-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry SUM_ROWS" PTXT-HAS
   s" .shared .align 4 .b8 SMEM[1024];" PTXT-HAS
   s" mov.f32 %f2, 0f00000000;" PTXT-HAS
   s" @%p2 mov.f32 %f2, %f1;" PTXT-HAS
   s" setp.ge.u32 %p4, %r10, 256;" PTXT-HAS
   s" add.f32 %f3, %f3, %f4;" PTXT-HAS
   s" .visible .entry SCATTER_ROWS" PTXT-HAS
   s" red.global.add.f32" PTXT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SUM1024-CG-OUTPUT ( -- )
   PTXT-RUN-SUM1024-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry SUM_ROWS_1024" PTXT-HAS
   s" .shared .align 4 .b8 SMEM[4096];" PTXT-HAS
   s" mov.f32 %f2, 0f00000000;" PTXT-HAS
   s" @%p2 mov.f32 %f2, %f1;" PTXT-HAS
   s" setp.ge.u32 %p4, %r10, 1024;" PTXT-HAS
   s" add.f32 %f3, %f3, %f4;" PTXT-HAS
   s" ERROR" PTXT-NOT-HAS ;

T-RESET
PTXT-SAXPY-OUTPUT
PTXT-OPS-CG-OUTPUT
PTXT-ONCE-CG-OUTPUT
PTXT-SOFTMAX-CG-OUTPUT
PTXT-SUM-CG-OUTPUT
PTXT-SUM1024-CG-OUTPUT
T-REPORT
s" saxpy-test: ok" type cr
