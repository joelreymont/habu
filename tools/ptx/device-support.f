\ device-support.f - shared fail-closed PTX device-proof helpers.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package PTX

20000 constant PTXAS-TIMEOUT-MS

create PTXAS-PATH-BUF FS-PATH-CAP allot

: PTXAS-DEFAULT$ ( -- ptr u8 n )
   s" /usr/local/cuda-12.6/bin/ptxas" ;

: ENV-PTXAS$ ( -- ptr u8 n bool )
   s" PTXAS" GETENV dup 0= if 2drop s" " 0 0= 0= exit then
   0 0= ;

: DEFAULT-PTXAS$ ( -- ptr u8 n bool )
   PTXAS-DEFAULT$ 2dup EXECUTABLE? if 0 0= exit then
   2drop s" " 0 0= 0= ;

: PATH-PTXAS$ ( -- ptr u8 n bool )
   s" ptxas" >LEN PTXAS-PATH-BUF FIND-EXECUTABLE if
      LEN>N PTXAS-PATH-BUF swap 0 0= exit
   then
   drop s" " 0 0= 0= ;

public

: CUDA-RC0 ( n -- )
   dup 0 <> if drop E-PTX-CUDA-DRIVER throw then
   drop ;

: CUDA-LIB ( n -- n )
   dup 0= if E-PTX-CUDA-DLOPEN throw then ;

: CUDA-SYMBOL ( n -- n )
   dup 0= if E-PTX-CUDA-DLSYM throw then ;

: U32@ ( ptr u8 -- n )
   dup c@
   over 1 BYTE+ c@ 8 lshift or
   over 2 BYTE+ c@ 16 lshift or
   swap 3 BYTE+ c@ 24 lshift or ;

: U32! ( n ptr u8 -- )
   {: x:n dst:ptr :}
   x $FF and dst c!
   x 8 rshift $FF and dst 1 BYTE+ c!
   x 16 rshift $FF and dst 2 BYTE+ c!
   x 24 rshift $FF and dst 3 BYTE+ c! ;

: PTXAS$ ( -- ptr u8 n )
   ENV-PTXAS$ if exit then
   2drop
   DEFAULT-PTXAS$ if exit then
   2drop
   PATH-PTXAS$ if exit then
   2drop
   E-PTX-PTXAS throw ;

: PATH-COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: JOIN-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: TEMP-DIR! ( ptr u8 n ptr u8 ptr n -- )
   {: prefix:ptr prefixu:n dst:ptr lenp:ptr :}
   prefix prefixu TMPDIR-MKDIR {: root:ptr rootu:n :}
   root rootu CLEANUP-TREE+
   root rootu dst lenp PATH-COPY! ;

: PTXAS-RUN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: ptx:ptr ptxu:n cubin:ptr cubinu:n out:ptr outcap:n err:ptr errcap:n timeout:n :}
   PROC-ARGV-ENV-RESET
   s" -arch=sm_87" >LEN PROC-ARGV+
   ptx ptxu >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   cubin cubinu >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   PTXAS$ >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-ENV-CAPTURE
   {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: PTXAS-RUN-DEFAULT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- n n n )
   PTXAS-TIMEOUT-MS PTXAS-RUN ;

end-package
