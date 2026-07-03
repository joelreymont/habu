\ cuda-types.f - nominal CUDA Driver handle roles.

require lib/ffi.f

-5002 constant E-MK-GPU

deftype cuda-dev
deftype cuda-ctx
deftype cuda-mod
deftype cuda-fn
deftype cuda-devptr

: CUDA-HANDLE0 ( n -- n )
   dup 0= if E-MK-GPU throw then ;

: CUDA-RC0 ( rc -- )
   RC>N dup 0 <> if E-MK-GPU throw then
   drop ;
