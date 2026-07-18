\ toolchain.f - checked PTX artifact paths and assembler runner.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/adt/option.f             \ option<n>: TRY-PTXAS$ reports a resolved ptxas as present/absent

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

variable PTXAS-P          \ resolved ptxas path pointer, parked by TRY-PTXAS$ for PTXAS$
                          \ (env-override and string-literal pointers are process-stable, so no copy)

create TC-ARCH-BUF 32 allot   \ assembler target label (e.g. sm_121a); TC-ARCH-U 0 = unset
variable TC-ARCH-U            \ never defaulted: ASSEMBLE fails closed until a caller sets it

: TC-ARCH$ ( -- ptr u8 n )   \ configured arch, or fail closed (no fallback target)
   TC-ARCH-U @ 0= if E-PTXTC-ARCH throw then
   TC-ARCH-BUF TC-ARCH-U @ ;

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

\ Park a resolved ptxas path pointer and report it present with its byte length.
: PTXAS-FOUND ( ptr u8 n -- option<n> ) {: a:ptr u:n :}
   a PTXAS-P !  u OPTION:SOME ;

public

\ Set the ptxas target from the active target's label (maki resolves it by
\ probing the device). No default: a caller that never sets it cannot assemble.
: TC-ARCH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= u 32 > or if E-PTXTC-ARCH throw then
   a TC-ARCH-BUF u BYTE-COPY  u TC-ARCH-U ! ;

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

\ Probe ptxas without failing: the PTXAS env override wins (trusted, as before -
\ set means chosen); then the CUDA 13 install (its ptxas knows sm_121a); then the
\ legacy 12.6 path, each existence-checked with FILE?. SOME len parks the winning
\ path pointer in PTXAS-P; NONE means no ptxas resolved on any known path. The path
\ list lives here only. No throws - callers decide whether absence is fatal.
: TRY-PTXAS$ ( -- option<n> )
   s" PTXAS" GETENV dup 0 > if PTXAS-FOUND exit then 2drop
   s" /usr/local/cuda/bin/ptxas"      2dup FILE? if PTXAS-FOUND exit then 2drop
   s" /usr/local/cuda-12.6/bin/ptxas" 2dup FILE? if PTXAS-FOUND exit then 2drop
   OPTION:NONE ;

\ Resolve ptxas, fail closed: the single place that turns an unresolved ptxas into
\ a loud named throw. No silent dead-path default.
: PTXAS$ ( -- ptr u8 n )
   TRY-PTXAS$ MATCH option
     none OF E-PTXTC-PTXAS throw ENDOF
     some OF PTXAS-P @ swap ENDOF
   ;MATCH ;

: ASSEMBLE ( ptr u8 len ptr u8 len -- n )
   {: out:ptr outcap:len err:ptr errcap:len :}
   PROC-ARGV-RESET
   SB-RESET s" -arch=" SB-APPEND TC-ARCH$ SB-APPEND SB$ >LEN PROC-ARGV+
   PTX$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   CUBIN$ >LEN PROC-ARGV+
   PTXAS$ >LEN out outcap err errcap ASM-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   err ERR-P !                                      \ stderr buffer ptr is constant; store before MATCH
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} e LEN>N ERR-U !  0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} e LEN>N ERR-U !  c RC>N ENDOF
   ;MATCH ;

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

;package
