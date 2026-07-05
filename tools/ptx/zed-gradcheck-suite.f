\ zed-gradcheck-suite.f - drive the per-VJP device gradcheck gate on the Orin.
\
\ Mac-side orchestrator over the ssh harness (tools/zed-run-lib.f): emits the
\ nine AD_FWD/AD_BWD kernels of tools/ptx/ad-entry-lib.f with the branch engine
\ into a PRIVATE local tmp root, ships each PTX to a private remote scratch,
\ assembles with the remote ptxas, ships tools/ptx/ad-gradcheck-launch.f, and
\ runs it from the device checkout: 10 per-entry positive fixtures (rnd, tie,
\ saturated) must agree numeric-vs-analytic and the 2 wrong-VJP fixtures must
\ be REJECTED, all on hardware. Device failure classes are asserted red first:
\ a malformed PTX makes the remote ptxas fail (E-ZED-RC) and a missing cubin
\ dies nonzero inside the launcher (CUDA:RC0 fail-closed).
\
\ Run: HABU_ZED=1 bin/hb --load lib/test.f lib/fs.f lib/fs-mutate.f
\   tools/zed-run-lib.f tools/ptx/zed-gradcheck-suite.f
\ When HABU_ZED is unset/0 it SKIPS explicitly (no device).

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/zed-run-lib.f

package ZED

$493E0 constant AGS-TIMEOUT-MS   \ 300000 ms: emit + ship + ptxas + launch

create AGS-ROOT-BUF FS-PATH-CAP allot
create AGS-DRV-BUF FS-PATH-CAP allot
create AGS-PTX-BUF FS-PATH-CAP allot

variable AGS-ROOT-U
variable AGS-DRV-U
variable AGS-PTX-U

: AGS-ROOT$ ( -- ptr u8 n )
   AGS-ROOT-BUF AGS-ROOT-U @ ;

: AGS-DRV$ ( -- ptr u8 n )
   AGS-DRV-BUF AGS-DRV-U @ ;

: AGS-PTX$ ( -- ptr u8 n )
   AGS-PTX-BUF AGS-PTX-U @ ;

: AGS-PREPARE ( -- )   \ private local tmp root for drivers + emitted PTX
   CLEANUP-RESET
   s" habu-adgc" TMPDIR-MKDIR {: a:ptr u:n :}
   u FS-PATH-CAP > if E-ZED-ARG throw then
   a AGS-ROOT-BUF u BYTE-COPY
   u AGS-ROOT-U !
   AGS-ROOT$ CLEANUP-TREE+
   AGS-ROOT$ s" drv.f" AGS-DRV-BUF JOIN-PATH AGS-DRV-U ! ;

: AGS-DRIVER! ( ptr u8 n -- ) {: key:ptr keyu:n :}   \ one-line emit driver: ade-<key>
   SB-RESET
   s" ade-" SB-APPEND  key keyu SB-APPEND  $0A SB-APPEND-C
   AGS-DRV$ SB$ WRITE-ALL ;

: AGS-PTX-PATH! ( ptr u8 n -- ) {: key:ptr keyu:n :}   \ root/<key>.ptx
   SB-RESET
   key keyu SB-APPEND  s" .ptx" SB-APPEND
   AGS-ROOT$ SB$ AGS-PTX-BUF JOIN-PATH AGS-PTX-U ! ;

: AGS-EMIT ( -- )   \ spawn the branch engine on the current driver, write PTX
   PROC-CMD-RESET
   s" --load" >LEN PROC-CMD-ARG+
   s" tools/ptx/ad-entry-lib.f" >LEN PROC-CMD-ARG+
   AGS-DRV$ >LEN PROC-CMD-ARG+
   s" bin/hb" >LEN AGS-TIMEOUT-MS >MS PROC-CMD-RUN-RC
   RC>N 0 <> if E-ZED-EMIT throw then
   AGS-PTX$ PROC-CMD-OUT$ WRITE-ALL ;

: AGS-ASSEMBLE ( ptr u8 n -- ) {: key:ptr keyu:n :}   \ remote ptxas <key>.ptx -> <key>.cubin
   SB-RESET
   s" /usr/local/cuda/bin/ptxas -arch=sm_87 " SB-APPEND
   key keyu SB-APPEND  s" .ptx -o " SB-APPEND
   key keyu SB-APPEND  s" .cubin" SB-APPEND
   SB$ RUN-IN RUN-OK ;

: AGS-KERNEL ( ptr u8 n -- ) {: key:ptr keyu:n :}   \ emit + ship + assemble one kernel
   key keyu AGS-DRIVER!
   key keyu AGS-PTX-PATH!
   AGS-EMIT
   AGS-PTX$ PUT-FILE
   key keyu AGS-ASSEMBLE ;

: AGS-EMIT-FILE ( ptr u8 n -- ) {: file:ptr fileu:n :}   \ standalone self-emitting driver
   PROC-CMD-RESET
   s" --load" >LEN PROC-CMD-ARG+
   file fileu >LEN PROC-CMD-ARG+
   s" bin/hb" >LEN AGS-TIMEOUT-MS >MS PROC-CMD-RUN-RC
   RC>N 0 <> if E-ZED-EMIT throw then
   AGS-PTX$ PROC-CMD-OUT$ WRITE-ALL ;

: AGS-KERNEL-FILE ( ptr u8 n ptr u8 n -- )   \ emit via a standalone cg file
   {: key:ptr keyu:n file:ptr fileu:n :}
   key keyu AGS-PTX-PATH!
   file fileu AGS-EMIT-FILE
   AGS-PTX$ PUT-FILE
   key keyu AGS-ASSEMBLE ;

: AGS-KERNELS ( -- )
   s" exp-fwd" AGS-KERNEL      s" exp-bwd" AGS-KERNEL
   s" xmsub-fwd" AGS-KERNEL    s" xmsub-bwd" AGS-KERNEL
   s" xdivsum-fwd" AGS-KERNEL  s" xdivsum-bwd" AGS-KERNEL
   s" softmax-fwd" AGS-KERNEL  s" softmax-bwd" AGS-KERNEL
   s" xdivsum-bwd-wrong" AGS-KERNEL ;

: AGS-VJP-KERNELS ( -- )   \ the vjp.f table entries' device fixtures
   s" add2-fwd" AGS-KERNEL     s" add2-bwd" AGS-KERNEL
   s" sub2-fwd" AGS-KERNEL     s" sub2-bwd" AGS-KERNEL
   s" mul2-fwd" AGS-KERNEL     s" mul2-bwd" AGS-KERNEL
   s" div2-fwd" AGS-KERNEL     s" div2-bwd" AGS-KERNEL
   s" overk-fwd" AGS-KERNEL    s" overk-bwd" AGS-KERNEL
   s" overk-bwd-wrong" AGS-KERNEL
   s" dropk-fwd" AGS-KERNEL    s" dropk-bwd" AGS-KERNEL
   s" dropk-bwd-wrong" AGS-KERNEL
   s" scale-fwd" AGS-KERNEL    s" scale-bwd" AGS-KERNEL
   s" fma-fwd" AGS-KERNEL      s" fma-bwd" AGS-KERNEL
   s" xsubsum-fwd" AGS-KERNEL  s" xsubsum-bwd" AGS-KERNEL
   s" expgen-fwd" AGS-KERNEL   s" expgen-bwd" AGS-KERNEL
   s" softmax-rows-bwd" s" tools/ptx/softmax-rows-bwd-cg.f" AGS-KERNEL-FILE ;

\ ---- device failure classes (red first) ---------------------------------------

: AGS-BAD-ASSEMBLE ( -- )   \ malformed PTX through the same assemble path
   AGS-ROOT$ s" garbage.ptx" AGS-PTX-BUF JOIN-PATH AGS-PTX-U !
   AGS-PTX$ s" this is not ptx" WRITE-ALL
   AGS-PTX$ PUT-FILE
   s" garbage" AGS-ASSEMBLE ;

: AGS-PTXAS-RED ( -- )
   [: AGS-BAD-ASSEMBLE ;] E-ZED-RC TTHROWSQ
   s" device failure class: malformed PTX -> remote ptxas fail-closed (E-ZED-RC)" type cr ;

: AGS-LAUNCH-CMD ( ptr u8 n -- ) {: dir:ptr diru:n :}   \ build the remote launcher run
   CMD-RESET
   s" cd ~/Work/habu && ./bin/hb --load" CMD-TOK
   s" lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f" CMD-TOK
   s" src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/header.f lib/ptx/launch.f" CMD-TOK
   s" lib/ffi.f maki/cuda-types.f maki/cuda-driver.f" CMD-TOK
   SCRATCH$ CMD-TOK  s" /ad-gradcheck-launch.f" CMD-RAW
   s" --" CMD-TOK  dir diru CMD-TOK ;

: AGS-MISSING-CUBIN-RED ( -- )   \ empty cubin dir: launcher must die nonzero
   s" mkdir empty-cubins" RUN-IN RUN-OK
   SB-RESET
   SCRATCH$ SB-APPEND  s" /empty-cubins" SB-APPEND
   SB$ AGS-LAUNCH-CMD
   CMD$ RUN RC>N 0 = 0= TTRUE
   s" device failure class: missing cubin -> launcher dies nonzero (CUDA:RC0)" type cr ;

: AGS-GRADCHECK ( -- )   \ the real per-VJP device gradcheck run
   SCRATCH$ AGS-LAUNCH-CMD
   CMD$ RUN RC>N 0 T=
   OUT$ type ;

: AGS-MAIN ( -- )
   AVAILABLE? 0= if s" per-VJP device gradcheck needs HABU_ZED" SKIP exit then
   AGS-TIMEOUT-MS TIMEOUT!
   PING
   s" /usr/local/cuda/bin/ptxas" NEED-TOOL
   T-RESET
   AGS-PREPARE
   SCRATCH-MK
   AGS-KERNELS
   AGS-VJP-KERNELS
   s" tools/ptx/ad-gradcheck-launch.f" PUT-FILE
   AGS-PTXAS-RED
   AGS-MISSING-CUBIN-RED
   AGS-GRADCHECK
   SCRATCH-RM
   CLEANUP-RUN
   T-REPORT ;

AGS-MAIN

end-package
