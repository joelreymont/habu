\ device-artifacts.f - private grader artifact paths.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f

package MAKI-GRADE

create ROOT-BUF FS-PATH-CAP allot
create DRIVER-BUF FS-PATH-CAP allot
create PTX-BUF FS-PATH-CAP allot
create CUBIN-BUF FS-PATH-CAP allot

variable ROOT-U
variable DRIVER-U
variable PTX-U
variable CUBIN-U

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
   ROOT$ s" driver.f" DRIVER-BUF DRIVER-U PATH!
   ROOT$ s" grade.ptx" PTX-BUF PTX-U PATH!
   ROOT$ s" grade.cubin" CUBIN-BUF CUBIN-U PATH! ;

: DRIVER$ ( -- ptr u8 n )
   DRIVER-BUF DRIVER-U @ ;

: PTX$ ( -- ptr u8 n )
   PTX-BUF PTX-U @ ;

: CUBIN$ ( -- ptr u8 n )
   CUBIN-BUF CUBIN-U @ ;

: PTXAS$ ( -- ptr u8 n )
   s" PTXAS" GETENV dup 0= if
      2drop s" /usr/local/cuda-12.6/bin/ptxas"
   then ;

;package
