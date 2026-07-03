\ cuda-driver.f - shared checked CUDA Driver FFI bindings.

require maki/cuda-types.f

package CUDA

create CD-LIB 16 allot
create CD-SYM 64 allot
variable CD-H

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: NONZERO? ( n -- bool )
   0= if FALSE else TRUE then ;

: SYMBOL ( ptr u8 n -- n )
   CD-SYM >CSTR
   CD-H @ CD-SYM DLSYM ;

public

: OPEN? ( -- bool )
   CD-H @ NONZERO? if TRUE exit then
   s" libcuda.so.1" CD-LIB >CSTR
   CD-LIB RTLD-NOW DLOPEN dup CD-H !
   NONZERO? ;

: OPEN ( -- )
   OPEN? 0= if E-MK-GPU throw then ;

: HANDLE@ ( -- n )
   CD-H @ ;

: HANDLE0 ( n -- n )
   CUDA-HANDLE0 ;

: RC0 ( rc -- )
   CUDA-RC0 ;

FFI: CUINIT ( n -- rc ) SYMBOL cuInit FFI;
FFI: CUDEVICEGET ( ptr a idx -- rc ) SYMBOL cuDeviceGet FFI;
FFI: CUDEVICEPRIMARYCTXRETAIN ( ptr a cuda-dev -- rc ) SYMBOL cuDevicePrimaryCtxRetain FFI;
FFI: CUCTXSETCURRENT ( cuda-ctx -- rc ) SYMBOL cuCtxSetCurrent FFI;
FFI: CUMODULELOAD ( ptr a ptr u8 -- rc ) SYMBOL cuModuleLoad FFI;
FFI: CUMODULEGETFUNCTION ( ptr a cuda-mod ptr u8 -- rc ) SYMBOL cuModuleGetFunction FFI;
FFI: CUMEMALLOC ( ptr a len -- rc ) SYMBOL cuMemAlloc_v2 FFI;
FFI: CUMEMFREE ( cuda-devptr -- rc ) SYMBOL cuMemFree_v2 FFI;
FFI: CUMEMSETD32 ( cuda-devptr n count -- rc ) SYMBOL cuMemsetD32_v2 FFI;
FFI: CUMEMCPYHTOD ( cuda-devptr ptr u8 len -- rc ) SYMBOL cuMemcpyHtoD_v2 FFI;
FFI: CUMEMCPYDTOH ( ptr u8 cuda-devptr len -- rc ) SYMBOL cuMemcpyDtoH_v2 FFI;
FFI: CUFUNCSETBLOCKSHAPE ( cuda-fn n n n -- rc ) SYMBOL cuFuncSetBlockShape FFI;
FFI: CUPARAMSETSIZE ( cuda-fn len -- rc ) SYMBOL cuParamSetSize FFI;
FFI: CUPARAMSETV ( cuda-fn idx ptr u8 len -- rc ) SYMBOL cuParamSetv FFI;
FFI: CULAUNCHGRID ( cuda-fn n n -- rc ) SYMBOL cuLaunchGrid FFI;
FFI: CUCTXSYNCHRONIZE ( -- rc ) SYMBOL cuCtxSynchronize FFI;
FFI: CUMODULEUNLOAD ( cuda-mod -- rc ) SYMBOL cuModuleUnload FFI;
FFI: CUDEVICEPRIMARYCTXRELEASE ( cuda-dev -- rc ) SYMBOL cuDevicePrimaryCtxRelease FFI;

end-package
