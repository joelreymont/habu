\ cuda-run.f - the maki injectable CUDA driver seam (package MKD).
\
\ The maki device runners (maki/gpu.f, maki/lower/launch.f, maki/eval/emit-device.f,
\ maki/eval/device.f, maki/eval/device-sm.f) acquire a primary context, load
\ modules, and allocate device memory, then hand ownership to CUDA-SCOPE
\ (lib/ptx/cuda-scope.f) so a throw between acquisitions or during a body op
\ unwinds every owned resource in reverse order. CUDA-SCOPE already exposes the
\ RELEASE side as injectable defers (REL-CTX!/REL-MOD!/REL-DEVPTR!); this package
\ is the symmetric ACQUIRE-and-body seam: every driver call a runner makes to
\ REACH an acquisition or throw point is a defer here, armed to the real sealed
\ CUDA package by USE-REAL at load. Host-only lifecycle tests re-point these
\ (CUMEMALLOC! ...) at recording/failing fakes to drive each runner off-device
\ and prove - with CUDA-SCOPE's release defers counting - that a failure after
\ any acquisition releases exactly the owned prefix, in reverse, exactly once.
\
\ Production behaviour is identical to calling CUDA: directly: each defer is armed
\ to a one-word wrapper over the sealed entry point, a single indirect call. The
\ word names mirror the CUDA spellings so a runner call site swaps CUDA: -> MKD:
\ with nothing else changed. RC0/the release entry points stay CUDA: (RC0 is not
\ an acquisition; releases flow through CUDA-SCOPE's own defers).

require lib/ptx/cuda-scope.f          \ CUDA-SCOPE + (transitively) package CUDA + E-CUDA

package MKD

\ ---- real-driver wrappers (the default arming target for each defer) ---------
: R-OPEN        ( -- )                        CUDA:OPEN ;
: R-CUINIT      ( n -- rc )                   CUDA:CU-INIT ;
: R-DEVGET      ( ptr a idx -- rc )           CUDA:CU-DEVICE-GET ;
: R-CTXRETAIN   ( ptr a cuda-dev -- rc )      CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN ;
: R-CTXSET      ( cuda-ctx -- rc )            CUDA:CU-CTX-SET-CURRENT ;
: R-MODLOAD     ( ptr a ptr u8 -- rc )        CUDA:CU-MODULE-LOAD ;
: R-MODFUNC     ( ptr a cuda-mod ptr u8 -- rc ) CUDA:CU-MODULE-GET-FUNCTION ;
: R-MEMALLOC    ( ptr a len -- rc )           CUDA:CU-MEM-ALLOC ;
: R-MEMSET      ( cuda-devptr n count -- rc ) CUDA:CU-MEMSET-D32 ;
: R-HTOD        ( cuda-devptr ptr u8 len -- rc ) CUDA:CU-MEMCPY-HTOD ;
: R-DTOH        ( ptr u8 cuda-devptr len -- rc ) CUDA:CU-MEMCPY-DTOH ;

public

\ ---- injectable driver table (one defer per reachable acquire/body call) -----
defer OPEN                     ( -- )
defer CUINIT                   ( n -- rc )
defer CUDEVICEGET              ( ptr a idx -- rc )
defer CUDEVICEPRIMARYCTXRETAIN ( ptr a cuda-dev -- rc )
defer CUCTXSETCURRENT          ( cuda-ctx -- rc )
defer CUMODULELOAD             ( ptr a ptr u8 -- rc )
defer CUMODULEGETFUNCTION      ( ptr a cuda-mod ptr u8 -- rc )
defer CUMEMALLOC               ( ptr a len -- rc )
defer CUMEMSETD32              ( cuda-devptr n count -- rc )
defer CUMEMCPYHTOD             ( cuda-devptr ptr u8 len -- rc )
defer CUMEMCPYDTOH             ( ptr u8 cuda-devptr len -- rc )

: USE-REAL ( -- )                              \ (re)arm the whole table to the sealed CUDA driver
   [: R-OPEN      ;] is OPEN
   [: R-CUINIT    ;] is CUINIT
   [: R-DEVGET    ;] is CUDEVICEGET
   [: R-CTXRETAIN ;] is CUDEVICEPRIMARYCTXRETAIN
   [: R-CTXSET    ;] is CUCTXSETCURRENT
   [: R-MODLOAD   ;] is CUMODULELOAD
   [: R-MODFUNC   ;] is CUMODULEGETFUNCTION
   [: R-MEMALLOC  ;] is CUMEMALLOC
   [: R-MEMSET    ;] is CUMEMSETD32
   [: R-HTOD      ;] is CUMEMCPYHTOD
   [: R-DTOH      ;] is CUMEMCPYDTOH ;

\ ---- per-op injection setters (host-only tests) -----------------------------
: OPEN!        ( [ -- ] -- )                        is OPEN ;
: CUINIT!      ( [ n -- rc ] -- )                   is CUINIT ;
: CUDEVICEGET! ( [ ptr a idx -- rc ] -- )           is CUDEVICEGET ;
: CTXRETAIN!   ( [ ptr a cuda-dev -- rc ] -- )      is CUDEVICEPRIMARYCTXRETAIN ;
: CTXSET!      ( [ cuda-ctx -- rc ] -- )            is CUCTXSETCURRENT ;
: MODLOAD!     ( [ ptr a ptr u8 -- rc ] -- )        is CUMODULELOAD ;
: MODFUNC!     ( [ ptr a cuda-mod ptr u8 -- rc ] -- ) is CUMODULEGETFUNCTION ;
: CUMEMALLOC!  ( [ ptr a len -- rc ] -- )           is CUMEMALLOC ;
: CUMEMSETD32! ( [ cuda-devptr n count -- rc ] -- ) is CUMEMSETD32 ;
: HTOD!        ( [ cuda-devptr ptr u8 len -- rc ] -- ) is CUMEMCPYHTOD ;
: DTOH!        ( [ ptr u8 cuda-devptr len -- rc ] -- ) is CUMEMCPYDTOH ;

USE-REAL

;package
