\ device-support-test.f - focused fail-closed PTX device helper tests.

require lib/test.f
require tools/ptx/device-support.f

package PTX

T-RESET

: BAD-RC ( -- )
   7 CUDA-RC0 ;

: BAD-LIB ( -- )
   0 CUDA-LIB drop ;

: BAD-SYMBOL ( -- )
   0 CUDA-SYMBOL drop ;

' BAD-RC E-PTX-CUDA-DRIVER TTHROWS
' BAD-LIB E-PTX-CUDA-DLOPEN TTHROWS
' BAD-SYMBOL E-PTX-CUDA-DLSYM TTHROWS

T-REPORT

end-package
