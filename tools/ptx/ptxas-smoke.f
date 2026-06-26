\ ptxas-smoke.f - Habu-native PTX assembler smoke for Orin.
\
\ Run on zed:
\ bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f
\ lib/process-argv.f lib/process-env.f src/arch/ptx/emit.f tools/ptx/ptxas-smoke.f

65536 constant PTXAS-CAP
20000 constant PTXAS-TIMEOUT-MS

create PTXAS-ROOT-BUF FS-PATH-CAP allot
create PTXAS-PTX-BUF FS-PATH-CAP allot
create PTXAS-CUBIN-BUF FS-PATH-CAP allot
create PTXAS-OUT PTXAS-CAP allot
create PTXAS-ERR PTXAS-CAP allot

variable PTXAS-ROOT-U
variable PTXAS-PTX-U
variable PTXAS-CUBIN-U
variable PTXAS-OUT-U

: PTXAS-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: PTXAS-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: PTXAS-ROOT ( -- ptr u8 n )
   PTXAS-ROOT-BUF PTXAS-ROOT-U @ ;

: PTXAS-PTX ( -- ptr u8 n )
   PTXAS-PTX-BUF PTXAS-PTX-U @ ;

: PTXAS-CUBIN ( -- ptr u8 n )
   PTXAS-CUBIN-BUF PTXAS-CUBIN-U @ ;

: PTXAS-PATH$ ( -- ptr u8 n )
   s" PTXAS" GETENV dup 0= if 2drop s" /usr/local/cuda-12.6/bin/ptxas" then ;

: PTXAS-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: PTXAS-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-ptxas" TMPDIR-MKDIR {: a:ptr u :}
   a u PTXAS-ROOT-BUF PTXAS-ROOT-U PTXAS-COPY!
   PTXAS-ROOT CLEANUP-TREE+
   PTXAS-ROOT s" saxpy.ptx" PTXAS-PTX-BUF PTXAS-PTX-U PTXAS-PATH!
   PTXAS-ROOT s" saxpy.cubin" PTXAS-CUBIN-BUF PTXAS-CUBIN-U PTXAS-PATH! ;

: PTXAS-RUN-HB ( -- n n n )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+
   s" lib/string.f" >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+
   s" tools/ptx/saxpy.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN PTXAS-OUT PTXAS-CAP >LEN PTXAS-ERR PTXAS-CAP >LEN
   PTXAS-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   PTXAS-CAPTURE>N ;

: PTXAS-EXPECT-HB ( n n n -- )
   0 <> if s" ptxas-smoke: hb emit failed" 1 die then
   dup 0 <> if s" ptxas-smoke: hb stderr" 1 die then drop
   dup 0 <= if s" ptxas-smoke: empty PTX" 1 die then
   PTXAS-OUT-U ! ;

: PTXAS-WRITE-PTX ( -- )
   PTXAS-RUN-HB PTXAS-EXPECT-HB
   PTXAS-PTX PTXAS-OUT PTXAS-OUT-U @ WRITE-ALL ;

: PTXAS-RUN-ASSEMBLER ( -- n n n )
   PROC-ARGV-ENV-RESET
   s" -arch=sm_87" >LEN PROC-ARGV+
   PTXAS-PTX >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   PTXAS-CUBIN >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   PTXAS-PATH$ >LEN PTXAS-OUT PTXAS-CAP >LEN PTXAS-ERR PTXAS-CAP >LEN
   PTXAS-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   PTXAS-CAPTURE>N ;

: PTXAS-EXPECT-ASM ( n n n -- )
   0 <> if s" ptxas-smoke: ptxas failed" 1 die then
   2drop
   PTXAS-CUBIN FILE? 0= if s" ptxas-smoke: missing cubin" 1 die then
   PTXAS-CUBIN FILE-SIZE 0 <= if s" ptxas-smoke: empty cubin" 1 die then ;

: PTXAS-MAIN ( -- )
   PTXAS-PREPARE
   PTXAS-WRITE-PTX
   PTXAS-RUN-ASSEMBLER PTXAS-EXPECT-ASM
   CLEANUP-RUN
   s" ptxas-smoke: ok" type cr ;

PTXAS-MAIN
