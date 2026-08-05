\ cuda-run.f - the maki injectable CUDA driver seam (package MKD).
\
\ The maki device runners and persistent session owner acquire a primary
\ context, load modules, and allocate device memory, then hand ownership to CUDA-SCOPE
\ (lib/ptx/cuda-scope.f) so a throw between acquisitions or during a body op
\ unwinds every owned resource in reverse order. CUDA-SCOPE already exposes the
\ RELEASE side as injectable defers (REL-CTX!/REL-MOD!/REL-DEVPTR!); this package
\ is the symmetric acquire-and-body seam: every driver call a runner or session
\ makes to reach an acquisition or throw point is a defer here, armed to the real sealed
\ CUDA package by USE-REAL at load. Host-only lifecycle tests re-point these
\ (CUMEMALLOC! ...) at recording/failing fakes to drive each runner off-device
\ and prove - with CUDA-SCOPE's release defers counting - that a failure after
\ any acquisition releases exactly the owned prefix, in reverse, exactly once.
\
\ Production behaviour is identical to calling CUDA: directly: each defer is armed
\ to a one-word wrapper over the sealed entry point, a single indirect call. The
\ word names mirror the CUDA spellings so a caller swaps CUDA: -> MKD: with
\ nothing else changed. Existing scoped releases still flow through CUDA-SCOPE.
\ Persistent GPU:session owns primary-context and stream release here;
\ persistent GPU:buffer owns device-allocation release here.

require lib/ptx/cuda-scope.f          \ CUDA-SCOPE + (transitively) package CUDA + E-CUDA

package MKD

\ ---- real-driver wrappers (the default arming target for each defer) ---------
: R-OPEN        ( -- )                        CUDA:OPEN ;
: R-CUINIT      ( n -- rc )                   CUDA:CU-INIT ;
: R-DEVGET      ( ptr n idx -- rc )           CUDA:CU-DEVICE-GET ;
: R-CTXRETAIN   ( ptr n cuda-dev -- rc )      CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN ;
: R-CTXRELEASE  ( cuda-dev -- rc )            CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE ;
: R-CTXSET      ( cuda-ctx -- rc )            CUDA:CU-CTX-SET-CURRENT ;
: R-STREAMCREATE ( ptr n n -- rc )            CUDA:CU-STREAM-CREATE ;
: R-STREAMSYNC  ( CUDA:stream -- rc )          CUDA:CU-STREAM-SYNCHRONIZE ;
: R-STREAMDESTROY ( CUDA:stream -- rc )       CUDA:CU-STREAM-DESTROY ;
: R-MODLOAD     ( ptr n ptr u8 -- rc )        CUDA:CU-MODULE-LOAD ;
: R-MODFUNC     ( ptr n cuda-mod ptr u8 -- rc ) CUDA:CU-MODULE-GET-FUNCTION ;
: R-MEMALLOC    ( ptr n len -- rc )           CUDA:CU-MEM-ALLOC ;
: R-MEMFREE     ( cuda-devptr -- rc )          CUDA:CU-MEM-FREE ;
: R-MEMSET      ( cuda-devptr n count -- rc ) CUDA:CU-MEMSET-D32 ;
: R-HTOD        ( cuda-devptr ptr u8 len -- rc ) CUDA:CU-MEMCPY-HTOD ;
: R-DTOH        ( ptr u8 cuda-devptr len -- rc ) CUDA:CU-MEMCPY-DTOH ;
: R-DTOD        ( cuda-devptr cuda-devptr len -- rc ) CUDA:CU-MEMCPY-DTOD ;

public

\ ---- injectable driver table (one defer per reachable acquire/body call) -----
defer OPEN                     ( -- )
defer CUINIT                   ( n -- rc )
defer CUDEVICEGET              ( ptr n idx -- rc )
defer CUDEVICEPRIMARYCTXRETAIN ( ptr n cuda-dev -- rc )
defer CUDEVICEPRIMARYCTXRELEASE ( cuda-dev -- rc )
defer CUCTXSETCURRENT          ( cuda-ctx -- rc )
defer CUSTREAMCREATE           ( ptr n n -- rc )
defer CUSTREAMSYNCHRONIZE      ( CUDA:stream -- rc )
defer CUSTREAMDESTROY          ( CUDA:stream -- rc )
defer CUMODULELOAD             ( ptr n ptr u8 -- rc )
defer CUMODULEGETFUNCTION      ( ptr n cuda-mod ptr u8 -- rc )
defer CUMEMALLOC               ( ptr n len -- rc )
defer CUMEMFREE                ( cuda-devptr -- rc )
defer CUMEMSETD32              ( cuda-devptr n count -- rc )
defer CUMEMCPYHTOD             ( cuda-devptr ptr u8 len -- rc )
defer CUMEMCPYDTOH             ( ptr u8 cuda-devptr len -- rc )
defer CUMEMCPYDTOD             ( cuda-devptr cuda-devptr len -- rc )

: USE-REAL ( -- )                              \ (re)arm the whole table to the sealed CUDA driver
   [: R-OPEN      ;] is OPEN
   [: R-CUINIT    ;] is CUINIT
   [: R-DEVGET    ;] is CUDEVICEGET
   [: R-CTXRETAIN ;] is CUDEVICEPRIMARYCTXRETAIN
   [: R-CTXRELEASE ;] is CUDEVICEPRIMARYCTXRELEASE
   [: R-CTXSET    ;] is CUCTXSETCURRENT
   [: R-STREAMCREATE ;] is CUSTREAMCREATE
   [: R-STREAMSYNC ;] is CUSTREAMSYNCHRONIZE
   [: R-STREAMDESTROY ;] is CUSTREAMDESTROY
   [: R-MODLOAD   ;] is CUMODULELOAD
   [: R-MODFUNC   ;] is CUMODULEGETFUNCTION
   [: R-MEMALLOC  ;] is CUMEMALLOC
   [: R-MEMFREE   ;] is CUMEMFREE
   [: R-MEMSET    ;] is CUMEMSETD32
   [: R-HTOD      ;] is CUMEMCPYHTOD
   [: R-DTOH      ;] is CUMEMCPYDTOH
   [: R-DTOD      ;] is CUMEMCPYDTOD ;

\ ---- per-op injection setters (host-only tests) -----------------------------
: OPEN!        ( [ -- ] -- )                        is OPEN ;
: CUINIT!      ( [ n -- rc ] -- )                   is CUINIT ;
: CUDEVICEGET! ( [ ptr n idx -- rc ] -- )           is CUDEVICEGET ;
: CTXRETAIN!   ( [ ptr n cuda-dev -- rc ] -- )      is CUDEVICEPRIMARYCTXRETAIN ;
: CTXRELEASE!  ( [ cuda-dev -- rc ] -- )            is CUDEVICEPRIMARYCTXRELEASE ;
: CTXSET!      ( [ cuda-ctx -- rc ] -- )            is CUCTXSETCURRENT ;
: STREAMCREATE! ( [ ptr n n -- rc ] -- )            is CUSTREAMCREATE ;
: STREAMSYNC!  ( [ CUDA:stream -- rc ] -- )          is CUSTREAMSYNCHRONIZE ;
: STREAMDESTROY! ( [ CUDA:stream -- rc ] -- )       is CUSTREAMDESTROY ;
: MODLOAD!     ( [ ptr n ptr u8 -- rc ] -- )        is CUMODULELOAD ;
: MODFUNC!     ( [ ptr n cuda-mod ptr u8 -- rc ] -- ) is CUMODULEGETFUNCTION ;
: CUMEMALLOC!  ( [ ptr n len -- rc ] -- )           is CUMEMALLOC ;
: CUMEMFREE!   ( [ cuda-devptr -- rc ] -- )          is CUMEMFREE ;
: CUMEMSETD32! ( [ cuda-devptr n count -- rc ] -- ) is CUMEMSETD32 ;
: HTOD!        ( [ cuda-devptr ptr u8 len -- rc ] -- ) is CUMEMCPYHTOD ;
: DTOH!        ( [ ptr u8 cuda-devptr len -- rc ] -- ) is CUMEMCPYDTOH ;
: DTOD!        ( [ cuda-devptr cuda-devptr len -- rc ] -- ) is CUMEMCPYDTOD ;

USE-REAL

;package
