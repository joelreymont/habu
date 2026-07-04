\ cuda-load.f - fail-closed CUDA module-load proof for a checked SAXPY cubin.

require lib/ptx/cuda-driver.f

package CUDALOAD

variable DEV
variable CTX
variable MOD
variable FUNC

: RESET ( -- )
   0 DEV !
   0 CTX !
   0 MOD !
   0 FUNC ! ;

: SETUP ( -- )
   CUDA:RESET
   CUDA:INIT
   DEV CUDA:DEVICE-GET
   CTX DEV @ CUDA:PRIMARY-CTX-RETAIN
   CTX @ CUDA:CTX-CURRENT! ;

: LOAD-SAXPY ( -- )
   s" /tmp/saxpy.cubin" MOD CUDA:LOAD-MODULE
   MOD @ s" SAXPY" FUNC CUDA:MODULE-FUNCTION ;

: RELEASE ( -- )
   MOD @ CUDA:UNLOAD-MODULE
   CTX @ 0 <> if DEV @ CUDA:PRIMARY-CTX-RELEASE then
   RESET ;

: BODY ( -- )
   SETUP
   LOAD-SAXPY
   FUNC @ 0= if E-PTX-CUDA-DLSYM throw then ;

: RUN ( -- )
   RESET
   [: BODY ;] catch {: rc:n :}
   RELEASE
   rc 0 <> if rc throw then
   s" SAXPY loaded on GPU: yes" type cr ;

RUN

end-package
