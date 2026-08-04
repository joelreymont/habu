\ maki/lower/red-device-test.f - the Orin device leg for the row-reduce region lowering.
\
\ docs/archive/cad-plan.md section 11 device-vs-host GOLDEN, slice 2. For each row-reduce model (RMSNORM,
\ LAYERNORM, SOFTMAX-ROW, the GELU->RMSNORM prologue-fusion case, the BIAS/SCALE broadcast
\ prologues, and a hand-built Rx1 column-broadcast prologue) it builds the model, fuses it,
\ and lowers region 0 onto a REGION_0 block-per-row PTX kernel: spawn a fresh
\ bin/hb (via LOWER-DRIVER!) to EMIT the kernel to PTXTC:PTX$, EMIT-GUARD, ptxas ASSEMBLE +
\ ASM-REPORT, then LOWER-GOLDEN (maki/lower/golden.f) runs the host executor and the device
\ kernel on the same synthetic inputs and compares under the reduction tolerance. Off the
\ Orin (no libcuda) the device leg is reported SKIPPED and the host build still runs.
\
\ Each MODEL: line runs at load time (a parse-time IR build, like the slice-1 device test);
\ LRD-GOLD1 lowers + launches whatever IR is current, so the same model text feeds the host
\ reference, the launch staging, and the spawned child that emits the kernel PTX. Not part of
\ the maki gate (maki/test.f) - it needs the CUDA toolkit + a device. Run on the Orin: scp to
\ zed:Work/habu then `bin/hb --load maki/lower/red-device-test.f`.

require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require lib/fs.f
require lib/fs-mutate.f
require lib/float.f
require lib/float32.f
require lib/fmt.f
require lib/ptx/toolchain.f
require maki/device-artifacts.f
require maki/cad.f
require maki/lower/golden.f
require maki/eval/active-target.f

package MAKI

using F32

create LRD-OUT $4000 allot  create LRD-ERR $1000 allot
create LRD-QO  $1000 allot  create LRD-QE  $2000 allot

\ ---- hand-built Rx1 MIR source (no MODEL: capture reaches an Rx1 into a reduction) --------
\ ADD(x 4x8, col 4x1) -> RMSNORM: the 4x1 col is BC-COL, so block r reads element r. Not a
\ legal capture class (cad.f SHP-LEGAL?), so there is no MODEL: text - the source is the raw
\ MIR builders. LOWER-DRIVER! appends its own FP-BUILD, so this text omits it; the parent
\ EVALUATEs the SAME text (one source of truth for the host IR and the child device IR).
create LRD-CS $800 allot  variable LRD-CS-U
: LRD-CS-RESET ( -- )  0 LRD-CS-U ! ;
: LRD-CS+ ( ptr u8 n -- ) {: a:ptr u:n :}
   LRD-CS-U @ u + $800 > if exit then
   a LRD-CS LRD-CS-U @ + u BYTE-COPY  LRD-CS-U @ u + LRD-CS-U ! ;
: LRD-COL-SRC ( -- ptr u8 n )
   LRD-CS-RESET
   S\" MIR-RESET\n"                                                                                        LRD-CS+
   S\" 4 8 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop\n"                                        LRD-CS+
   S\" 4 1 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop\n"                                        LRD-CS+
   S\" MAKI-OPKIND:ADD MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+\n"   LRD-CS+
   S\" 4 8 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop\n"                                       LRD-CS+
   S\" MAKI-OPKIND:RMSNORM MIR-OP-BEGIN 0 MIR-NODE-ID MIR-NODE-REF MIR-IN+\n"                              LRD-CS+
   S\" 4 8 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop\n"                                       LRD-CS+
   LRD-CS LRD-CS-U @ ;

\ ---- spawn bin/hb to emit region 0's PTX into PTXTC:PTX$ (child re-builds the IR) --
: LRD-EMIT ( ptr u8 n -- ) {: sa:ptr su:n :}
   sa su  s" require maki/lower/red.f"  s" LRED-EMIT"  0 FP-REGION-ID  MAKI-GRADE:DRIVER$  LOWER-DRIVER!
   PROC-ARGV-RESET
   s" --load"           >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$   >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  LRD-OUT $4000 >LEN  LRD-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC ENDOF          \ clean exit -> rc 0
     err OF PCAP-FAILED:UNMAKE ENDOF                  \ nonzero child: (out err code) on stack
   ;MATCH
   {: outu:len erru:len rc:rc :}
   LRD-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ surface stderr + throw on a nonzero child
   PTXTC:PTX$ LRD-OUT outu LEN>N WRITE-ALL ;

: LRD-PTXAS ( -- n )  LRD-QO $1000 >LEN LRD-QE $2000 >LEN PTXTC:ASSEMBLE ;

\ ---- per-element evidence (device value | host value rounded to f32) --------
: LRD-FP ( r -- )  SB-RESET 6 FMT:SB-FIX SB$ type ;
: LRD-HOST@ ( n -- r )  LLA-OUT-NODE@ EX-OUT@ swap T-GET  NARROW WIDEN ;   \ narrowed host elem
: LRD-EVIDENCE ( -- )
   s" elem   device        host_f32" type cr
   LLA-ELEMS@ 0 ?do
      s"   " type i SB-RESET FMT:SB-INT SB$ type s"    " type
      i LLA-OUT@ LRD-FP  s"    " type  i LRD-HOST@ LRD-FP  cr
   loop ;

\ ---- one model: emit -> assemble -> golden -> evidence -> assert V-PASS ------
\ Skips (no emit/ptxas) off-device, so the file still loads where there is no CUDA.
: LRD-GOLD1 ( ptr u8 n -- ) {: sa:ptr su:n :}
   CUDA:OPEN? 0= if exit then
   sa su LRD-EMIT
   LRD-PTXAS PTXTC:ASM-REPORT 0 T=                        \ ptxas rc 0 (stderr surfaced on failure)
   PTXTC:CUBIN$ LLA-CUBIN!
   0 FP-REGION-ID LOWER-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   LRD-EVIDENCE
   v V-PASS T= ;

: LRD-BEGIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" lower-red-device: libcuda unavailable -> device leg SKIPPED (host build OK)" type cr
      exit then
   s" habu-lred-drv" MAKI-GRADE:PREPARE
   s" habu-lred-ptx" PTXTC:PREPARE
   ATGT:LABEL$ PTXTC:TC-ARCH! ;         \ assembler arch from the probed active target (sm_87 Orin / sm_121a GB10), before any ASSEMBLE

: LRD-END ( -- )
   CUDA:OPEN? 0= if  0 0= TTRUE  T-REPORT exit then
   PTXTC:CLEAN  MAKI-GRADE:CLEAN
   T-REPORT ;

;using
;package

package MAKI
LRD-BEGIN

s" == RMSNORM 4x8 ==" type cr
MODEL: RM ( x:4x8 -- y ) RMSNORM ;  FP-BUILD
s" MODEL: RM ( x:4x8 -- y ) RMSNORM ;" LRD-GOLD1

s" == LAYERNORM 4x8 ==" type cr
MODEL: LN ( x:4x8 -- y ) LAYERNORM ;  FP-BUILD
s" MODEL: LN ( x:4x8 -- y ) LAYERNORM ;" LRD-GOLD1

s" == SOFTMAX-ROW 4x8 ==" type cr
MODEL: SM ( x:4x8 -- y ) SOFTMAX-ROW ;  FP-BUILD
s" MODEL: SM ( x:4x8 -- y ) SOFTMAX-ROW ;" LRD-GOLD1

s" == GELU->RMSNORM 4x8 ==" type cr
MODEL: GR ( x:4x8 -- y ) GELU RMSNORM ;  FP-BUILD
s" MODEL: GR ( x:4x8 -- y ) GELU RMSNORM ;" LRD-GOLD1

\ BIAS->RMSNORM 4x8 + 1x8: a 1xC row-broadcast prologue pinned to row 0 (same row/block);
\ the device kernel reads element tid of the single bias row and the host reads EX-BC@ 1xC.
s" == BIAS->RMSNORM 4x8 + 1x8 (1xC row-broadcast) ==" type cr
MODEL: BR ( x:4x8 b:1x8 -- y ) BIAS RMSNORM ;  FP-BUILD
s" MODEL: BR ( x:4x8 b:1x8 -- y ) BIAS RMSNORM ;" LRD-GOLD1

\ SCALE->RMSNORM 4x8 + 1x1: a 1x1 scalar-broadcast prologue (row 0 + zero column offset);
\ every lane reads element 0 on both legs.
s" == SCALE->RMSNORM 4x8 + 1x1 (1x1 scalar-broadcast) ==" type cr
MODEL: SR ( x:4x8 s:1x1 -- y ) SCALE RMSNORM ;  FP-BUILD
s" MODEL: SR ( x:4x8 s:1x1 -- y ) SCALE RMSNORM ;" LRD-GOLD1

\ COL->RMSNORM 4x8 + 4x1: an Rx1 column-broadcast prologue (hand-built IR - not a legal
\ capture class, so no MODEL: text). Every lane in block r reads element r of the 4x1 col
\ (EX-BC@ [e/C]); the device kernel's stride-1 span (base + row*4) must match the host.
\ The parent EVALUATEs the same source the child rebuilds (LRD-COL-SRC), so one text drives
\ both legs; the host-executor leg for this fold is pinned off-device by maki/executor-test.f.
s" == COL->RMSNORM 4x8 + 4x1 (Rx1 column-broadcast, hand-built) ==" type cr
LRD-COL-SRC EVALUATE FP-BUILD
LRD-COL-SRC LRD-GOLD1

LRD-END
;package
