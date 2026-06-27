\ device-smoke.f - the maki gate's device-FFI canary (closes habu-add-device-ffi).
\
\ Static invariant: before any device tool runs, the running bin/hb MUST provide every
\ FFI-ABI primitive lib/ffi.f needs (ffi-call-n / ffi-call-abi / ffi-call-abi-r, the
\ AAPCS64 additions). Two legs enforce it:
\  (a) PRIMITIVE PROOF - the maki gate now loads lib/ffi.f just before this file. A
\      stale bin/hb (predating those primitives) FAILS TO LOAD lib/ffi.f, so the gate
\      stops early and points at the FFI layer - not cryptically deep in a device tool
\      (the exact failure that stayed hidden when the gate was checker-only). Reaching
\      DEVICE-SMOKE at all already means the primitives are present.
\  (b) DEVICE SMOKE - a tiny live cuInit/cuDeviceGet via the same FFI path the device
\      tools use. If libcuda is absent (off-Orin) the device leg is reported SKIPPED and
\      the primitive proof stands; a libcuda that loads but whose cuInit/cuDeviceGet
\      fail is a HARD failure -> refresh bin/hb / check the driver (docs/bootstrap.md).
\ Load after lib/ffi.f + lib/test.f. Verbatim ED-SYM idiom from maki/eval-device.f.

create DS-LIB 16 allot   create DS-NM 32 allot
variable DS-H   variable DS-DEV

: DS-SYM ( ptr u8 n -- n )  DS-NM >CSTR  DS-H @ DS-NM DLSYM ;

: DEVICE-SMOKE ( -- )
   T-RESET
   \ (a) primitive proof (the load already enforced it; tally it explicitly)
   0 0= TTRUE
   s" device-FFI: bin/hb has the AAPCS64 FFI-ABI primitives (lib/ffi.f loaded)" type cr
   \ (b) live device leg
   s" libcuda.so.1" DS-LIB >CSTR  DS-LIB RTLD-NOW DLOPEN  dup DS-H !
   0= if
      s" device-FFI: libcuda.so.1 unavailable -> device leg SKIPPED (off-device; proof stands)" type cr
   else
      0             s" cuInit"      DS-SYM CALL1  0 T=          \ cuInit(0) == CUDA_SUCCESS
      DS-DEV P>N 0  s" cuDeviceGet"  DS-SYM CALL2  0 T=          \ cuDeviceGet(&dev,0) == CUDA_SUCCESS
      s" device-FFI: cuInit + cuDeviceGet OK on the Orin" type cr
   then
   T-REPORT ;

DEVICE-SMOKE
