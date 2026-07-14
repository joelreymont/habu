\ zed-device-suite.f - drive the PTX device goldens on the Orin via the ssh
\ harness (closes the zed proof of habu-fix-ptx-collective).
\
\ The committed launchers (tools/ptx/sum-launch.f, softmax-launch.f,
\ softmax-gradcheck.f) are now SELF-EMITTING: each spawns bin/hb on the Orin to
\ emit its checked producer to a PRIVATE per-run PTXTC PTX, ptxas-assembles it to a
\ private cubin, loads it, launches, and compares against the CPU golden - all
\ fail-closed (a missing producer or nonzero emit/ptxas rc throws E-PTX-EMIT with
\ the child stderr surfaced; a dropped copy-back throws E-PTX-READBACK). So this
\ harness no longer emits PTX locally, ships /tmp/zed-*.ptx, or ptxas-builds shared
\ /tmp/*.cubin (those artifacts were ignored by the self-emitting launchers). It
\ just runs each launcher on the Orin over ssh and asserts a zero exit; the
\ launcher owns emit+assemble+launch+golden+cleanup.
\
\ Run: HABU_ZED=1 bin/hb --load lib/test.f lib/fs.f lib/fs-mutate.f tools/zed-run-lib.f tools/ptx/zed-device-suite.f
\ When HABU_ZED is unset/0 it SKIPS explicitly (no device).

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/zed-run-lib.f

package ZED

$493E0 constant DEV-TIMEOUT-MS   \ 300000 ms: self-emit + ptxas + launch on the Orin

: LAUNCH-PREFIX ( -- )   \ committed launcher load prefix, cwd = the Orin checkout
   CMD-RESET
   s" cd ~/Work/habu && ./bin/hb --load" CMD-TOK
   s" lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f" CMD-TOK
   s" src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/header.f lib/ptx/launch.f lib/ffi.f" CMD-TOK ;

: PASS. ( ptr u8 n -- )   \ print a PASS line with the launcher's captured report
   s" device golden PASS on Orin: " type type
   s"  -> " type OUT$ type ;

: SUM-GOLDEN ( -- )
   LAUNCH-PREFIX  s" tools/ptx/sum-launch.f" CMD-TOK
   CMD$ RUN RC>N 0 T=
   s" SUM_ROWS direct row sum (self-emit)" PASS. ;

: SOFTMAX-GOLDEN ( -- )
   LAUNCH-PREFIX  s" tools/ptx/softmax-launch.f" CMD-TOK
   CMD$ RUN RC>N 0 T=
   s" SOFTMAX_ROWS forward (self-emit)" PASS. ;

: GRADCHECK-GOLDEN ( -- )
   LAUNCH-PREFIX  s" maki/array.f" CMD-TOK  s" tools/ptx/softmax-gradcheck.f" CMD-TOK
   CMD$ RUN RC>N 0 T=
   s" SOFTMAX_BWD gradcheck (k=4 < block, self-emit)" PASS. ;

: MAIN ( -- )
   AVAILABLE? 0= if s" ptx device goldens need HABU_ZED" SKIP exit then
   DEV-TIMEOUT-MS TIMEOUT!
   PING
   s" /usr/local/cuda/bin/ptxas" NEED-TOOL
   T-RESET
   SUM-GOLDEN
   SOFTMAX-GOLDEN
   GRADCHECK-GOLDEN
   T-REPORT ;

MAIN

;package
