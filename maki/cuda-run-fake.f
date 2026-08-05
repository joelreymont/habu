\ cuda-run-fake.f - shared recording fake CUDA driver for host-only leak tests.
\
\ Re-points the maki acquire seam (package MKD, maki/cuda-run.f) and CUDA-SCOPE's
\ release defers at recording fakes so the *-leak-test.f suites drive the REAL
\ migrated runners off-device: no GPU, no libcuda. Each acquire fake writes a
\ distinct fake handle into the runner's out-cell (device=100, ctx=200, module=
\ 300, function=400, allocation i -> 500+i*100) and returns an armed CUresult;
\ each release fake logs the handle it was asked to free, in order, so a test can
\ assert exactly-once, reverse-order release and outstanding-count-to-zero. Every
\ driver call is independently armable to fail (per-op rc variables + an indexed
\ allocation fault), which is the injected failure matrix the migrations need.
\ ON installs the fakes; OFF restores the real driver (call it before any real
\ device work runs in the same process).

require maki/cuda-run.f               \ MKD injection seam + (transitively) CUDA-SCOPE + CUDA + E-CUDA

package MKDF

public

100 constant DEV-H   200 constant CTX-H   300 constant MOD-H   400 constant FUNC-H
: ALLOC-H ( n -- n )  100 * 500 + ;   \ i-th successful allocation's fake handle

private

create RLOG 32 cells allot  variable RN
: LOG ( n -- )  RN @ cells RLOG + !  1 RN +! ;
variable NA                           \ cuMemAlloc call counter

public

\ per-op armed CUresult (0 = success); set directly by a test before driving a runner
variable CUINIT-RC   variable DEVGET-RC   variable CTXRET-RC   variable CTXSET-RC
variable MODLOAD-RC  variable FUNC-RC     variable MEMSET-RC   variable HTOD-RC   variable DTOH-RC
variable FAIL-A      variable A-RC        \ allocation index armed to fail (-1 = none) + its rc

private

: F-OPEN    ( -- )  ;
: F-CUINIT  ( n -- rc ) {: f:n :}  CUINIT-RC @ >RC ;

: F-DEVGET ( ptr n idx -- rc ) {: out:ptr idx:idx :}
   DEV-H out !
   DEVGET-RC @ >RC ;

: F-CTXRET ( ptr n cuda-dev -- rc ) {: out:ptr dev:cuda-dev :}
   CTX-H out !
   CTXRET-RC @ >RC ;

: F-CTXSET  ( cuda-ctx -- rc ) {: c:cuda-ctx :}  CTXSET-RC @ >RC ;

: F-MODLOAD ( ptr n ptr u8 -- rc ) {: out:ptr p:ptr :}
   MOD-H out !
   MODLOAD-RC @ >RC ;

: F-MODFUNC ( ptr n cuda-mod ptr u8 -- rc )
   {: out:ptr m:cuda-mod nm:ptr :}
   FUNC-H out !
   FUNC-RC @ >RC ;

: F-ALLOC ( ptr n len -- rc ) {: out:ptr len:len :}
   NA @ FAIL-A @ = if  1 NA +!  A-RC @ >RC  exit  then
   NA @ ALLOC-H  out !  1 NA +!  0 >RC ;
: F-MEMSET  ( cuda-devptr n count -- rc ) {: d:cuda-devptr v:n c:count :}  MEMSET-RC @ >RC ;
: F-HTOD    ( cuda-devptr ptr u8 len -- rc ) {: d:cuda-devptr s:ptr l:len :}  HTOD-RC @ >RC ;
: F-DTOH    ( ptr u8 cuda-devptr len -- rc ) {: d:ptr s:cuda-devptr l:len :}  DTOH-RC @ >RC ;
: F-REL     ( n -- n ) {: h:n :}  h LOG  0 ;

public

: ON ( -- )                          \ install the recording fakes into both seams
   [: F-OPEN ;]    MKD:OPEN!
   [: F-CUINIT ;]  MKD:CUINIT!
   [: F-DEVGET ;]  MKD:CUDEVICEGET!
   [: F-CTXRET ;]  MKD:CTXRETAIN!
   [: F-CTXSET ;]  MKD:CTXSET!
   [: F-MODLOAD ;] MKD:MODLOAD!
   [: F-MODFUNC ;] MKD:MODFUNC!
   [: F-ALLOC ;]   MKD:CUMEMALLOC!
   [: F-MEMSET ;]  MKD:CUMEMSETD32!
   [: F-HTOD ;]    MKD:HTOD!
   [: F-DTOH ;]    MKD:DTOH!
   [: F-REL ;]     CUDA-SCOPE:REL-CTX!
   [: F-REL ;]     CUDA-SCOPE:REL-MOD!
   [: F-REL ;]     CUDA-SCOPE:REL-DEVPTR! ;

: OFF ( -- )  MKD:USE-REAL  CUDA-SCOPE:USE-REAL-DRIVER ;   \ restore the real driver table

: RESET ( -- )                       \ clear the log, alloc counter, and every armed failure
   0 RN !  0 NA !  -1 FAIL-A !  0 A-RC !
   0 CUINIT-RC !  0 DEVGET-RC !  0 CTXRET-RC !  0 CTXSET-RC !
   0 MODLOAD-RC !  0 FUNC-RC !  0 MEMSET-RC !  0 HTOD-RC !  0 DTOH-RC !
   CUDA-SCOPE:RESET ;

: RN@  ( -- n )    RN @ ;             \ number of releases recorded
: REL@ ( n -- n )  cells RLOG + @ ;   \ i-th released handle (release order)

;package
