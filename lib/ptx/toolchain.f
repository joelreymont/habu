\ toolchain.f - PTX device-proof temp roots and ptxas helpers.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package PTXTOOL

$4000 constant CAPTURE-CAP
20000 constant PTXAS-TIMEOUT-MS

create OUT-BUF CAPTURE-CAP allot
create ERR-BUF CAPTURE-CAP allot

: CAPTURE>N ( len len rc -- n n n )
   {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

public

: PTXAS$ ( -- ptr u8 n )
   s" PTXAS" GETENV dup 0= if
      2drop s" /usr/local/cuda-12.6/bin/ptxas"
   then ;

: TEMP-ROOT ( ptr u8 n -- ptr u8 n )
   TMPDIR-MKDIR ;

: JOIN-INTO ( ptr u8 n ptr u8 n ptr u8 ptr a -- )
   {: root:ptr rootu:n name:ptr nameu:n dst:ptr lenp:ptr :}
   root rootu name nameu dst JOIN-PATH lenp ! ;

: CHECK-PTXAS-RC ( n -- )
   0 <> if E-PTX-PTXAS throw then ;

: ASSEMBLE ( ptr u8 n ptr u8 n -- )
   {: ptx:ptr ptxu:n cubin:ptr cubinu:n :}
   PROC-ARGV-ENV-RESET
   s" -arch=sm_87" >LEN PROC-ARGV+
   ptx ptxu >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   cubin cubinu >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   PTXAS$ >LEN OUT-BUF CAPTURE-CAP >LEN ERR-BUF CAPTURE-CAP >LEN
   PTXAS-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   CAPTURE>N {: outn:n errn:n rc:n :}
   rc CHECK-PTXAS-RC
   cubin cubinu FILE? 0= if E-PTX-CUDA-CUBIN throw then
   cubin cubinu FILE-SIZE 0 <= if E-PTX-CUDA-CUBIN throw then ;

end-package
