\ cuda-types.f - re-export of the checked CUDA Driver handle roles + guards.
\
\ The canonical checked CUDA Driver now lives in lib/ptx/cuda-driver.f. This
\ thin re-export preserves the historical maki spellings so existing consumers
\ keep working unchanged: the nominal cuda-dev/ctx/mod/fn/devptr roles (with
\ their >CUDA-* / CUDA-*>N casts), the fail-closed CUDA-HANDLE0 / CUDA-RC0
\ guards, and the E-MK-GPU throw code (an alias of the lib E-CUDA code, same
\ value). New code should depend on lib/ptx/cuda-driver.f directly.

require lib/ptx/cuda-driver.f

E-CUDA constant E-MK-GPU        \ historical maki spelling of the CUDA failure code
