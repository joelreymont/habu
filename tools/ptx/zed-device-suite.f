\ zed-device-suite.f - drive the PTX device goldens on the Orin via the ssh
\ harness (closes the zed proof of habu-fix-ptx-collective).
\
\ The maki-type-families engine predates the Orin's baked bin/hb (its type-family
\ checker vocabulary is missing there) and the box's gforth is too old to
\ bootstrap, so the branch engine cannot be rebuilt on the Orin. Instead each
\ kernel's PTX is emitted HERE by the branch engine (proving the branch codegen),
\ shipped to a private scratch dir, assembled by the remote ptxas, and launched by
\ the committed device launchers on the Orin, which compare against the CPU
\ reference. Golden mismatch or a bad launch is fail-closed (nonzero rc asserted).
\ SUM_ROWS is emitted single-kernel via tools/ptx/sum-device-cg.f; softmax
\ forward/backward source is byte-identical across the branch and the Orin tree.
\
\ Run: HABU_ZED=1 bin/hb --load lib/test.f lib/fs.f lib/fs-mutate.f tools/zed-run-lib.f tools/ptx/zed-device-suite.f
\ When HABU_ZED is unset/0 it SKIPS explicitly (no device).

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/zed-run-lib.f

package ZED

$493E0 constant DEV-TIMEOUT-MS   \ 300000 ms: emit + ship + ptxas + launch

\ ---- local emit: spawn the branch bin/hb, capture PTX, write a local file -----

: RUN-EMIT ( ptr u8 n -- ) {: p:ptr pu:n :}   \ PROC-CMD args preloaded; p = local path
   s" bin/hb" >LEN DEV-TIMEOUT-MS >MS PROC-CMD-RUN-RC
   RC>N 0 <> if E-ZED-EMIT throw then
   p pu PROC-CMD-OUT$ WRITE-ALL ;

: EMIT-CG-COMMON ( -- )   \ shared collective-codegen emit prefix
   PROC-CMD-RESET
   s" --load" >LEN PROC-CMD-ARG+
   s" lib/errors.f" >LEN PROC-CMD-ARG+  s" lib/string.f" >LEN PROC-CMD-ARG+
   s" lib/float.f" >LEN PROC-CMD-ARG+   s" lib/fmt.f" >LEN PROC-CMD-ARG+
   s" src/arch/ptx/emit.f" >LEN PROC-CMD-ARG+  s" lib/ptx/cg.f" >LEN PROC-CMD-ARG+
   s" lib/ptx/header.f" >LEN PROC-CMD-ARG+  s" lib/ptx/cg-collective.f" >LEN PROC-CMD-ARG+
   s" lib/ptx/launch.f" >LEN PROC-CMD-ARG+  s" lib/ptx/collective.f" >LEN PROC-CMD-ARG+ ;

: EMIT-SUM ( -- )
   EMIT-CG-COMMON
   s" tools/ptx/sum-device-cg.f" >LEN PROC-CMD-ARG+
   s" /tmp/zed-sum.ptx" RUN-EMIT ;

: EMIT-SOFTMAX ( -- )
   EMIT-CG-COMMON
   s" tools/ptx/softmax-cg.f" >LEN PROC-CMD-ARG+
   s" /tmp/zed-softmax.ptx" RUN-EMIT ;

: EMIT-SOFTMAX-BWD ( -- )
   PROC-CMD-RESET
   s" --load" >LEN PROC-CMD-ARG+
   s" lib/errors.f" >LEN PROC-CMD-ARG+  s" lib/string.f" >LEN PROC-CMD-ARG+
   s" lib/float.f" >LEN PROC-CMD-ARG+   s" lib/fmt.f" >LEN PROC-CMD-ARG+
   s" lib/test.f" >LEN PROC-CMD-ARG+    s" src/arch/ptx/emit.f" >LEN PROC-CMD-ARG+
   s" lib/ptx/cg.f" >LEN PROC-CMD-ARG+  s" lib/ptx/header.f" >LEN PROC-CMD-ARG+
   s" lib/ptx/cg-collective.f" >LEN PROC-CMD-ARG+  s" lib/ptx/collective.f" >LEN PROC-CMD-ARG+
   s" lib/ptx/ad-dag.f" >LEN PROC-CMD-ARG+  s" tools/ptx/softmax-bwd-cg.f" >LEN PROC-CMD-ARG+
   s" /tmp/zed-softmax-bwd.ptx" RUN-EMIT ;

\ ---- remote assemble + launch -------------------------------------------------

: LAUNCH-PREFIX ( -- )   \ committed launcher load prefix, cwd = the Orin checkout
   CMD-RESET
   s" cd ~/Work/habu && ./bin/hb --load" CMD-TOK
   s" lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f" CMD-TOK
   s" src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/header.f lib/ptx/launch.f lib/ffi.f" CMD-TOK ;

: PASS. ( ptr u8 n -- )   \ print a PASS line with the launcher's captured report
   s" device golden PASS on Orin: " type type
   s"  -> " type OUT$ type ;

: SUM-GOLDEN ( -- )
   EMIT-SUM
   s" /tmp/zed-sum.ptx" PUT-FILE
   s" /usr/local/cuda/bin/ptxas -arch=sm_87 zed-sum.ptx -o /tmp/sum.cubin" RUN-IN RUN-OK
   LAUNCH-PREFIX  s" tools/ptx/sum-launch.f" CMD-TOK
   CMD$ RUN RC>N 0 T=
   s" SUM_ROWS direct row sum" PASS. ;

: SOFTMAX-GOLDEN ( -- )
   EMIT-SOFTMAX
   s" /tmp/zed-softmax.ptx" PUT-FILE
   s" /usr/local/cuda/bin/ptxas -arch=sm_87 zed-softmax.ptx -o /tmp/softmax.cubin" RUN-IN RUN-OK
   LAUNCH-PREFIX  s" tools/ptx/softmax-launch.f" CMD-TOK
   CMD$ RUN RC>N 0 T=
   s" SOFTMAX_ROWS forward" PASS. ;

: GRADCHECK-GOLDEN ( -- )
   EMIT-SOFTMAX-BWD
   s" /tmp/zed-softmax-bwd.ptx" PUT-FILE
   s" /usr/local/cuda/bin/ptxas -arch=sm_87 zed-softmax-bwd.ptx -o /tmp/softmax-bwd.cubin" RUN-IN RUN-OK
   LAUNCH-PREFIX  s" maki/array.f" CMD-TOK  s" tools/ptx/softmax-gradcheck.f" CMD-TOK
   CMD$ RUN RC>N 0 T=
   s" SOFTMAX_BWD gradcheck (k=4 < block)" PASS. ;

: CLEANUP ( -- )
   s" rm -f /tmp/sum.cubin /tmp/softmax.cubin /tmp/softmax-bwd.cubin" RUN RUN-OK
   SCRATCH-RM ;

: MAIN ( -- )
   AVAILABLE? 0= if s" ptx device goldens need HABU_ZED" SKIP exit then
   DEV-TIMEOUT-MS TIMEOUT!
   PING
   s" /usr/local/cuda/bin/ptxas" NEED-TOOL
   SCRATCH-MK
   T-RESET
   SUM-GOLDEN
   SOFTMAX-GOLDEN
   GRADCHECK-GOLDEN
   CLEANUP
   T-REPORT ;

MAIN

;package
