\ cuda-driver.f - re-export of the checked CUDA Driver FFI (lib/ptx/cuda-driver.f).
\
\ The canonical driver now lives in lib/ptx/cuda-driver.f with repo-standard
\ hyphenated binding names (CU-INIT, CU-DEVICE-GET, ...). This thin re-export
\ preserves the historical maki spellings (CUDA:CUINIT ... CUDA:CUDEVICEPRIMARY-
\ CTXRELEASE) as typed aliases over the hyphenated bindings so existing consumers
\ (gpu.f, eval-device*.f, lower-launch.f) keep working unchanged. The resolver
\ (OPEN?/OPEN/HANDLE@/HANDLE0/RC0) and every hyphenated binding flow through the
\ require chain; only the legacy-spelling aliases are added here. New code should
\ call the hyphenated CUDA:CU-* names directly.

require maki/cuda-types.f
require lib/ptx/cuda-driver.f

package CUDA
public

: CUINIT ( n -- rc ) CU-INIT ;
: CUDEVICEGET ( ptr a idx -- rc ) CU-DEVICE-GET ;
: CUDEVICEPRIMARYCTXRETAIN ( ptr a cuda-dev -- rc ) CU-DEVICE-PRIMARY-CTX-RETAIN ;
: CUCTXSETCURRENT ( cuda-ctx -- rc ) CU-CTX-SET-CURRENT ;
: CUMODULELOAD ( ptr a ptr u8 -- rc ) CU-MODULE-LOAD ;
: CUMODULEGETFUNCTION ( ptr a cuda-mod ptr u8 -- rc ) CU-MODULE-GET-FUNCTION ;
: CUMEMALLOC ( ptr a len -- rc ) CU-MEM-ALLOC ;
: CUMEMFREE ( cuda-devptr -- rc ) CU-MEM-FREE ;
: CUMEMSETD32 ( cuda-devptr n count -- rc ) CU-MEMSET-D32 ;
: CUMEMCPYHTOD ( cuda-devptr ptr u8 len -- rc ) CU-MEMCPY-HTOD ;
: CUMEMCPYDTOH ( ptr u8 cuda-devptr len -- rc ) CU-MEMCPY-DTOH ;
: CUFUNCSETBLOCKSHAPE ( cuda-fn n n n -- rc ) CU-FUNC-SET-BLOCK-SHAPE ;
: CUPARAMSETSIZE ( cuda-fn len -- rc ) CU-PARAM-SET-SIZE ;
: CUPARAMSETV ( cuda-fn idx ptr u8 len -- rc ) CU-PARAM-SET-V ;
: CULAUNCHGRID ( cuda-fn n n -- rc ) CU-LAUNCH-GRID ;
: CUCTXSYNCHRONIZE ( -- rc ) CU-CTX-SYNCHRONIZE ;
: CUMODULEUNLOAD ( cuda-mod -- rc ) CU-MODULE-UNLOAD ;
: CUDEVICEPRIMARYCTXRELEASE ( cuda-dev -- rc ) CU-DEVICE-PRIMARY-CTX-RELEASE ;

end-package
