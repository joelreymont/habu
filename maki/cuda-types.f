\ cuda-types.f - nominal CUDA Driver handle roles.
\
\ Part of the CUDA driver subsystem: the handle-check helpers live in `package CUDA`
\ (private) alongside maki/cuda-driver.f, which reopens the package and calls them by
\ bare name. The E-MK-GPU error code and the deftype role tokens stay global: error
\ codes are cross-cutting (like lib/errors.f's E-*), and deftypes register global
\ nominal roles whose >CUDA-DEV/... converters callers use unqualified.

require lib/ffi.f

-5002 constant E-MK-GPU

deftype cuda-dev
deftype cuda-ctx
deftype cuda-mod
deftype cuda-fn
deftype cuda-devptr

package CUDA

: CUDA-HANDLE0 ( n -- n )
   dup 0= if E-MK-GPU throw then ;

: CUDA-RC0 ( rc -- )
   RC>N dup 0 <> if E-MK-GPU throw then
   drop ;

end-package
