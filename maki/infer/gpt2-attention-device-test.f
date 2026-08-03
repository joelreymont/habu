\ gpt2-attention-device-test.f - real-CUDA GPT-2 decode attention proof.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/float.f
require lib/float32.f
require lib/float32-buffer.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/ffi-abi.f
require lib/test.f
require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f
require lib/ptx/cuda-scope.f
require maki/attention.f
require maki/eval/active-target.f
require maki/infer/gpt2-attention-cg.f

package GPT2-ATTN-DEVICE-TEST

using F32

private

17 constant CAP
12 constant HEADS
64 constant HD
128 constant BLOCK
HEADS HD * constant ROW-ELEMS
ROW-ELEMS 4 * constant ROW-BYTES
CAP ROW-ELEMS * constant CACHE-ELEMS
CACHE-ELEMS 4 * constant CACHE-BYTES
CACHE-BYTES 8 + constant CACHE-ALLOC-BYTES
ROW-BYTES 8 + constant ROW-ALLOC-BYTES
64 constant PARAM-BYTES
$4000 constant EMIT-CAP
$1000 constant ERR-CAP
0.001 constant TOL

create EMIT-OUT EMIT-CAP allot
create EMIT-ERR ERR-CAP allot
create ASM-OUT ERR-CAP allot
create ASM-ERR ERR-CAP allot
create PATH-BUF 64 allot
create NAME-BUF 32 allot

create HOST-Q CACHE-ELEMS cells allot
create HOST-K CACHE-ELEMS cells allot
create HOST-V CACHE-ELEMS cells allot
create REF-Q CAP HD * cells allot
create REF-K CAP HD * cells allot
create REF-V CAP HD * cells allot
create REF-S CAP CAP * cells allot
create REF-A CAP CAP * cells allot
create REF-O CAP HD * cells allot

create ROW-Q ROW-BYTES allot
create ROW-K ROW-BYTES allot
create ROW-V ROW-BYTES allot
create ROW-OUT ROW-ALLOC-BYTES allot
create K-READ CACHE-ALLOC-BYTES allot
create V-READ CACHE-ALLOC-BYTES allot

variable DEV
variable CTX
variable CU-MOD
variable FUNC
variable DQ
variable DK
variable DV
variable DKC
variable DVC
variable DOUT
variable PKC
variable PVC
variable POUT
variable POS-V
variable HEADS-V
variable HD-V
variable CAP-V
variable LAUNCH-N

: ROUND-F32 ( r -- r )
   NARROW WIDEN ;

: IDX ( n n n -- n ) {: pos:n head:n dim:n :}
   pos HEADS * head + HD * dim + ;

: RIDX ( n n -- n ) {: pos:n dim:n :}
   pos HD * dim + ;

: QVAL ( n n n -- r ) {: pos:n head:n dim:n :}
   pos 3 * head 5 * + dim + 17 mod 8 - s>f 16.0 f/ ROUND-F32 ;

: KVAL ( n n n -- r ) {: pos:n head:n dim:n :}
   pos 7 * head 3 * + dim 5 * + 19 mod 9 - s>f 16.0 f/ ROUND-F32 ;

: VVAL ( n n n -- r ) {: pos:n head:n dim:n :}
   pos 5 * head 7 * + dim 3 * + 23 mod 11 - s>f 8.0 f/ ROUND-F32 ;

: HOST-FILL-HEAD ( n n -- ) {: pos:n head:n :}
   HD 0 ?do
      pos head i IDX {: idx:n :}
      pos head i QVAL HOST-Q idx cells + !
      pos head i KVAL HOST-K idx cells + !
      pos head i VVAL HOST-V idx cells + !
   loop ;

: HOST-FILL ( -- )
   CAP 0 ?do HEADS 0 ?do j i HOST-FILL-HEAD loop loop ;

: ROW-PACK ( n -- ) {: pos:n :}
   HOST-Q pos ROW-ELEMS * cells + ROW-ELEMS ROW-Q F32-BUF:PACK
   HOST-K pos ROW-ELEMS * cells + ROW-ELEMS ROW-K F32-BUF:PACK
   HOST-V pos ROW-ELEMS * cells + ROW-ELEMS ROW-V F32-BUF:PACK ;

: REF-COPY ( n n -- ) {: pos:n head:n :}
   pos 1+ 0 ?do
      HD 0 ?do
         j head i IDX {: src:n :}
         j i RIDX {: dst:n :}
         HOST-Q src cells + @ REF-Q dst cells + !
         HOST-K src cells + @ REF-K dst cells + !
         HOST-V src cells + @ REF-V dst cells + !
      loop
   loop ;

: NEAR? ( r r -- bool ) {: got:r want:r :}
   got want f- fabs TOL f< ;

: REF-CHECK-HEAD ( n n -- ) {: pos:n head:n :}
   pos head REF-COPY
   REF-Q REF-K REF-V REF-S REF-A REF-O pos 1+ HD MAKI:ATTN-FWD
   HD 0 ?do
      ROW-OUT 4 + i head HD * + 4 * + F32-BUF:LOAD PTXSENT:GUARD WIDEN
      REF-O pos i RIDX cells + @ ROUND-F32
      NEAR? TTRUE
   loop ;

: REF-CHECK ( n -- ) {: pos:n :}
   HEADS 0 ?do pos i REF-CHECK-HEAD loop ;

: EMIT-WRITE ( len len rc -- ) {: outu:len erru:len code:rc :}
   EMIT-ERR erru LEN>N code RC>N PTXTC:EMIT-GUARD
   PTXTC:PTX$ EMIT-OUT outu LEN>N WRITE-ALL ;

: EMIT-PTX ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" maki/infer/gpt2-attention-device-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" emit" >LEN PROC-ARGV+
   ATGT:LABEL$ >LEN PROC-ARGV+
   ATGT:VER$ >LEN PROC-ARGV+
   s" bin/hb" >LEN EMIT-OUT EMIT-CAP >LEN EMIT-ERR ERR-CAP >LEN 20000 >MS RUN-ARGV-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE 0 >RC EMIT-WRITE ENDOF
      err OF PCAP-FAILED:UNMAKE EMIT-WRITE ENDOF
   ;MATCH ;

: ASSEMBLE ( -- )
   ATGT:LABEL$ PTXTC:TC-ARCH!
   ASM-OUT ERR-CAP >LEN ASM-ERR ERR-CAP >LEN PTXTC:ASSEMBLE
   PTXTC:ASM-REPORT 0 T= ;

: DEVICE-SETUP ( -- )
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   CTX DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   DEV @ >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ PATH-BUF FFI:CSTR
   CU-MOD PATH-BUF CUDA:CU-MODULE-LOAD CUDA:RC0
   CU-MOD @ >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   s" GPT2_ATTN" NAME-BUF FFI:CSTR
   FUNC CU-MOD @ >CUDA-MOD NAME-BUF CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   DQ ROW-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   DQ @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   DK ROW-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   DK @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   DV ROW-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   DV @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   DKC CACHE-ALLOC-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   DKC @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   DVC CACHE-ALLOC-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   DVC @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   DOUT ROW-ALLOC-BYTES >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   DOUT @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   DKC @ 4 + PKC !
   DVC @ 4 + PVC !
   DOUT @ 4 + POUT !
   DKC @ >CUDA-DEVPTR PTXSENT:POISON CACHE-ELEMS 2 + >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0
   DVC @ >CUDA-DEVPTR PTXSENT:POISON CACHE-ELEMS 2 + >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0 ;

: PARAMS! ( n n n n -- ) {: pos:n heads:n width:n cap:n :}
   pos POS-V ! heads HEADS-V ! width HD-V ! cap CAP-V !
   FUNC @ >CUDA-FN PARAM-BYTES >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   FUNC @ >CUDA-FN 0 >IDX DQ 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 8 >IDX DK 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 16 >IDX DV 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 24 >IDX PKC 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 32 >IDX PVC 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 40 >IDX POUT 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 48 >IDX POS-V 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 52 >IDX HEADS-V 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 56 >IDX HD-V 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   FUNC @ >CUDA-FN 60 >IDX CAP-V 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0 ;

: DEVICE-LAUNCH ( n n n n -- ) {: pos:n heads:n width:n cap:n :}
   pos heads width cap GPT2-ATTN:LAUNCH-CHECK {: shared:n :}
   FUNC @ >CUDA-FN BLOCK 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   FUNC @ >CUDA-FN shared CUDA:CU-FUNC-SET-SHARED-SIZE CUDA:RC0
   pos heads width cap PARAMS!
   LAUNCH-N @ 1+ LAUNCH-N !
   FUNC @ >CUDA-FN heads 1 CUDA:CU-LAUNCH-GRID CUDA:RC0 ;

: ROW-RUN ( n -- ) {: pos:n :}
   pos ROW-PACK
   DQ @ >CUDA-DEVPTR ROW-Q ROW-BYTES >LEN CUDA:HTOD
   DK @ >CUDA-DEVPTR ROW-K ROW-BYTES >LEN CUDA:HTOD
   DV @ >CUDA-DEVPTR ROW-V ROW-BYTES >LEN CUDA:HTOD
   DOUT @ >CUDA-DEVPTR PTXSENT:POISON ROW-ELEMS 2 + >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0
   pos HEADS HD CAP DEVICE-LAUNCH
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   ROW-OUT DOUT @ >CUDA-DEVPTR ROW-ALLOC-BYTES >LEN CUDA:DTOH
   ROW-OUT F32-BUF:LOAD PTXSENT:POISON T=
   ROW-OUT ROW-ALLOC-BYTES 4 - + F32-BUF:LOAD PTXSENT:POISON T=
   pos REF-CHECK ;

: CACHE-CHECK-ONE ( ptr u8 ptr a -- ) {: read:ptr host:ptr :}
   read F32-BUF:LOAD PTXSENT:POISON T=
   read CACHE-ALLOC-BYTES 4 - + F32-BUF:LOAD PTXSENT:POISON T=
   CACHE-ELEMS 0 ?do
      read 4 i 4 * + + F32-BUF:LOAD
      host i cells + @ NARROW T=
   loop ;

: CACHE-CHECK ( -- )
   K-READ DKC @ >CUDA-DEVPTR CACHE-ALLOC-BYTES >LEN CUDA:DTOH
   V-READ DVC @ >CUDA-DEVPTR CACHE-ALLOC-BYTES >LEN CUDA:DTOH
   K-READ HOST-K CACHE-CHECK-ONE
   V-READ HOST-V CACHE-CHECK-ONE ;

: BAD-NEG-POS ( -- ) -1 HEADS HD CAP DEVICE-LAUNCH ;
: BAD-END-POS ( -- ) CAP HEADS HD CAP DEVICE-LAUNCH ;
: BAD-HEADS ( -- ) 0 0 HD CAP DEVICE-LAUNCH ;
: BAD-HD ( -- ) 0 HEADS 0 CAP DEVICE-LAUNCH ;
: BAD-CAP ( -- ) 0 HEADS HD 0 DEVICE-LAUNCH ;

: REFUSALS ( -- )
   LAUNCH-N @ {: before:n :}
   [: BAD-NEG-POS ;] E-PTX-BLOCK TTHROWSQ
   [: BAD-END-POS ;] E-PTX-BLOCK TTHROWSQ
   [: BAD-HEADS ;] E-PTX-BLOCK TTHROWSQ
   [: BAD-HD ;] E-PTX-BLOCK TTHROWSQ
   [: BAD-CAP ;] E-PTX-BLOCK TTHROWSQ
   LAUNCH-N @ before T= ;

: DEVICE-RUN ( -- )
   [: DEVICE-SETUP
      0 LAUNCH-N !
      REFUSALS
      CAP 0 ?do i ROW-RUN loop
      CACHE-CHECK
      LAUNCH-N @ CAP T=
   ;] CUDA-SCOPE:SCOPE ;

: EMIT-CHILD ( -- )
   1 SCRIPT-ARGV$ PTX-ARCH!
   2 SCRIPT-ARGV$ PTX-VER!
   GPT2-ATTN:EMIT ;

: EMIT-MODE? ( -- bool )
   SCRIPT-ARGC 3 = if 0 SCRIPT-ARGV$ s" emit" STR= else false then ;

public

: MAIN ( -- )
   EMIT-MODE? if EMIT-CHILD exit then
   T-RESET
   HOST-FILL
   s" habu-gpt2-attention" PTXTC:PREPARE
   EMIT-PTX
   ASSEMBLE
   DEVICE-RUN
   PTXTC:CLEAN
   s" gpt2 attention: positions 0, 1, 7, 8, and 16 match independent F32 golden; cache and output canaries intact" type cr
   T-REPORT ;

MAIN

;using
;package
