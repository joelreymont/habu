\ cuda-load.f - CHECKED on-device proof: load a checked-emitted SAXPY cubin as a
\ live GPU module on the Orin and obtain its function handle.
\
\ Uses the checked FFI (lib/ffi.f): NO 0 set-check - the only trusted boundary is
\ lib/ffi.f's P>N/N>P (pointer<->cell). Uses cuDevicePrimaryCtxRetain (the Orin's
\ camera pipeline owns the primary context; ptx.md Resolved-M1/M2 #7) and the _v2
\ symbols are not needed for module ops. Load after lib/errors.f, lib/string.f,
\ lib/ffi.f. Prereq: a cubin at /tmp/saxpy.cubin (see tools/ptx/saxpy-cg.f + ptxas).

create CL-LIB  16 allot          \ "libcuda.so.1\0"
create CL-NM   64 allot          \ symbol-name C-string scratch
create CL-PATH 64 allot          \ cubin path C-string scratch
create CL-KN   32 allot          \ kernel-name C-string (separate from CL-NM)
variable CL-H  variable CL-DEV  variable CL-CTX  variable CL-MOD  variable CL-FUNC

: CL-OPEN ( -- )
   s" libcuda.so.1" CL-LIB >CSTR  CL-LIB RTLD-NOW DLOPEN CL-H ! ;
: CL-SYM ( ptr u8 n -- n )                          \ resolve a CUDA driver symbol
   CL-NM >CSTR  CL-H @ CL-NM DLSYM ;

: CUDA-LOAD-SAXPY ( -- bool )                        \ true if SAXPY loaded on the GPU
   CL-OPEN
   0                         s" cuInit"                CL-SYM CALL1 drop
   CL-DEV P>N 0              s" cuDeviceGet"           CL-SYM CALL2 drop
   CL-CTX P>N CL-DEV @       s" cuDevicePrimaryCtxRetain" CL-SYM CALL2 drop
   CL-CTX @                  s" cuCtxSetCurrent"       CL-SYM CALL1 drop
   s" /tmp/saxpy.cubin" CL-PATH >CSTR
   CL-MOD P>N CL-PATH P>N    s" cuModuleLoad"          CL-SYM CALL2 drop
   s" SAXPY" CL-KN >CSTR
   CL-FUNC P>N CL-MOD @ CL-KN P>N  s" cuModuleGetFunction" CL-SYM CALL3 drop
   CL-MOD @                  s" cuModuleUnload"        CL-SYM CALL1 drop
   CL-DEV @                  s" cuDevicePrimaryCtxRelease" CL-SYM CALL1 drop
   CL-FUNC @ 0 <> ;

: CUDA-LOAD-REPORT ( -- )
   CUDA-LOAD-SAXPY if s" SAXPY loaded on GPU: yes" else s" SAXPY loaded on GPU: NO" then type cr ;

CUDA-LOAD-REPORT
bye
