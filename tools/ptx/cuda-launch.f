\ cuda-launch.f - checked on-device SAXPY launch proof.
\
\ Prereq: cubin at /tmp/saxpy.cubin. Data: x=2.0, y=0, a=3.0, n=4
\ => y' = a*x+y = 6.0 (f32 0x40C00000).

require tools/ptx/bench.f

package PTX

variable LL-DX
variable LL-DY
variable LL-ABITS
variable LL-NV
create LL-RBUF 4 allot

: LL-SETUP ( -- )
   BENCH-RESET
   s" /tmp/saxpy.cubin" BENCH-CUBIN!
   s" SAXPY" BENCH-KERNEL!
   s" SAXPY" BENCH-LABEL!
   256 BENCH-BLOCK!
   1 BENCH-GRID!
   24 KERNEL-PARAM-BYTES!
   DEVICE-OPEN
   MODULE-LOAD ;

: LL-ALLOC ( -- )
   16 LL-DX DEVICE-ALLOC
   16 LL-DY DEVICE-ALLOC
   LL-DX @ $40000000 4 DEVICE-MEMSET32
   LL-DY @ 0 4 DEVICE-MEMSET32 ;

: LL-PARAMS ( -- )
   $40400000 LL-ABITS !
   4 LL-NV !
   KERNEL-PREPARE-LAUNCH
   0 LL-DX KERNEL-PARAM-PTR!
   8 LL-DY KERNEL-PARAM-PTR!
   16 LL-ABITS KERNEL-PARAM-U32!
   20 LL-NV KERNEL-PARAM-U32! ;

: LL-LAUNCH ( -- )
   LL-ALLOC
   LL-PARAMS
   KERNEL-LAUNCH
   DEVICE-SYNC
   LL-RBUF LL-DY @ 4 DTOH ;

: LL-FREE-DEV ( n -- )
   dup 0 <> if DEVICE-FREE else drop then ;

: LL-RELEASE ( -- )
   MODULE-UNLOAD
   LL-DX @ LL-FREE-DEV
   LL-DY @ LL-FREE-DEV
   0 LL-DX ! 0 LL-DY !
   DEVICE-CLOSE ;

: SAXPY-GPU-BITS ( -- n )
   LL-RBUF U32@ ;

: LAUNCH-SAXPY ( -- )
   $DEADBEEF LL-RBUF U32!
   LL-SETUP LL-LAUNCH LL-RELEASE
   s" SAXPY on GPU: y=a*x+y=3*2+0 -> f32 bits " type SAXPY-GPU-BITS . cr
   s" expected 0x40C00000 ; PASS? " type
   SAXPY-GPU-BITS $40C00000 = if s" yes" else E-PTX-DEVICE-WRONG throw then type cr ;

LAUNCH-SAXPY

end-package
