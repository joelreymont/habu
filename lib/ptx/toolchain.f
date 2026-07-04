\ toolchain.f - checked PTX artifact paths and assembler runner.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f

package PTXTC

$2710 constant ASM-TIMEOUT-MS

create ROOT-BUF FS-PATH-CAP allot
create PTX-BUF FS-PATH-CAP allot
create CUBIN-BUF FS-PATH-CAP allot

variable ROOT-U
variable PTX-U
variable CUBIN-U

variable ERR-P            \ last ASSEMBLE stderr buffer + byte count, for diagnostics
variable ERR-U

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: root:ptr rootu:n name:ptr nameu:n dst:ptr lenp:ptr :}
   root rootu name nameu dst JOIN-PATH lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: CLEAN-ROOT ( -- )
   ROOT-U @ 0 > if ROOT$ REMOVE-TREE then
   0 ROOT-U ! ;

public

: CLEAN ( -- )
   CLEAN-ROOT ;

: PREPARE ( ptr u8 n -- )
   CLEAN-ROOT
   TMPDIR-MKDIR ROOT-BUF ROOT-U COPY!
   ROOT$ s" kernel.ptx" PTX-BUF PTX-U PATH!
   ROOT$ s" kernel.cubin" CUBIN-BUF CUBIN-U PATH! ;

: PTX$ ( -- ptr u8 n )
   PTX-BUF PTX-U @ ;

: CUBIN$ ( -- ptr u8 n )
   CUBIN-BUF CUBIN-U @ ;

: PTXAS$ ( -- ptr u8 n )
   s" PTXAS" GETENV dup 0= if
      2drop s" /usr/local/cuda-12.6/bin/ptxas"
   then ;

: ASSEMBLE ( ptr u8 len ptr u8 len -- n )
   {: out:ptr outcap:len err:ptr errcap:len :}
   PROC-ARGV-RESET
   s" -arch=sm_87" >LEN PROC-ARGV+
   PTX$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   CUBIN$ >LEN PROC-ARGV+
   PTXAS$ >LEN out outcap err errcap ASM-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   err ERR-P !  erru LEN>N ERR-U !
   rc RC>N ;

\ stderr from the last ASSEMBLE, so a failing assemble can be diagnosed not masked
: ERR$ ( -- ptr u8 n )  ERR-P @  ERR-U @ ;

\ surface a nonzero ptxas rc: type its captured stderr, pass rc through to the assert
: ASM-REPORT ( n -- n )
   dup 0= 0= if ERR$ type cr then ;

\ surface a spawned kernel-emit failure: nonzero child rc -> type its stderr, named throw
: EMIT-GUARD ( ptr u8 n n -- ) {: err:ptr erru:n rc:n :}
   rc 0= if exit then
   err erru type cr
   E-PTX-EMIT throw ;

end-package
