\ saxpy-test.f - focused tests for the PTX SAXPY encoder.
\
\ Load after lib/errors.f, lib/string.f, lib/test.f, lib/fs.f, lib/process.f,
\ lib/process-argv.f, lib/process-env.f, and src/arch/ptx/emit.f.

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

T-RESET
PTXT-SAXPY-OUTPUT
T-REPORT
s" saxpy-test: ok" type cr
