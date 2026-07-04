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
\ Verbatim ED-SYM idiom from maki/eval-device.f.

require lib/test.f
require maki/cuda-driver.f

package MAKI

variable DS-DEV

: DS-RC0 ( rc -- )
   RC>N 0 T= ;

: DEVICE-SMOKE ( -- )
   T-RESET
   \ (a) primitive proof (the load already enforced it; tally it explicitly)
   0 0= TTRUE
   s" device-FFI: bin/hb has the AAPCS64 FFI-ABI primitives (lib/ffi.f loaded)" type cr
   \ (b) live device leg
   CUDA:OPEN? 0= if
      s" device-FFI: libcuda.so.1 unavailable -> device leg SKIPPED (off-device; proof stands)" type cr
   else
      0 CUDA:CUINIT DS-RC0
      DS-DEV 0 >IDX CUDA:CUDEVICEGET DS-RC0
      s" device-FFI: cuInit + cuDeviceGet OK on the Orin" type cr
   then
   T-REPORT ;

DEVICE-SMOKE

end-package
