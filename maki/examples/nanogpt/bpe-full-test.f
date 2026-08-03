\ maki/bpe-full-test.f - full runtime GPT-2 vocab load, end to end (dot habu-bpe-full-50k-a598ba57).
\ Presence-gated on the runtime-fetched vocab.bpe (fetch-gpt2-vocab.sh, default DEST gpt2-vocab/;
\ never committed), following the landed device-leg SKIP pattern (maki/lower/mm-device-test.f):
\ absent -> the leg is SKIPPED and reports ok; present -> BPF-LOAD installs all 50000 merges and
\ this proves exact tiktoken full-vocab parity, real-text round-trip, the 50257 vocab structure,
\ and measures encode throughput. This is the full-table EVIDENCE harness; the hermetic gate lives
\ in maki/bpe-test.f (committed 79-merge subset + the full-table red-first locks). BPFT-prefixed.

require lib/test.f
require lib/prelude.f
require lib/time.f
require maki/examples/nanogpt/bpe-full-data.f
require maki/infer/gpt2-pin.f

package MAKI

512 constant BPFT-CAP
create BPFT-IDS BPFT-CAP cells allot
create BPFT-BK  4096 allot
variable BPFT-M

: BPFT-PATH ( -- ptr u8 n )  s" gpt2-vocab/vocab.bpe" ;

\ encode s and compare to a pinned tiktoken id vector
: BPFT-ENC-EQ? ( ptr a n ptr u8 n -- bool )  {: ea:ptr en:n s:ptr slen:n :}
   s slen BPFT-IDS BPFT-CAP BPR-ENCODE {: m:n :}
   m en <> if false exit then
   en 0 ?do
      BPFT-IDS i T-GET f>s  ea i cells + @  <> if false unloop exit then
   loop true ;
: BPFT-PARITY? ( -- bool )
   BPFD-I1  BPFD-N1  BPFD-S1  BPFT-ENC-EQ?
   BPFD-I2  BPFD-N2  BPFD-S2  BPFT-ENC-EQ? and
   BPFD-I3  BPFD-N3  BPFD-S3  BPFT-ENC-EQ? and
   BPFD-I4  BPFD-N4  BPFD-S4  BPFT-ENC-EQ? and
   BPFD-I5  BPFD-N5  BPFD-S5  BPFT-ENC-EQ? and
   BPFD-I6  BPFD-N6  BPFD-S6  BPFT-ENC-EQ? and
   BPFD-I7  BPFD-N7  BPFD-S7  BPFT-ENC-EQ? and
   BPFD-I8  BPFD-N8  BPFD-S8  BPFT-ENC-EQ? and
   BPFD-I9  BPFD-N9  BPFD-S9  BPFT-ENC-EQ? and
   BPFD-I10 BPFD-N10 BPFD-S10 BPFT-ENC-EQ? and ;

: BPFT-RT? ( ptr u8 n -- bool )  {: a:ptr u:n :}
   a u BPFT-IDS BPFT-CAP BPR-ENCODE {: m:n :}
   BPFT-IDS m BPFT-BK 4096 BPR-DECODE {: nb:n :}
   nb u =  BPFT-BK nb a u T-STR=  and ;

: BPFT-STRUCT? ( -- bool )
   BPR-VOCAB 50257 =  BPR-EOT 50256 =  and  BPR-MERGES 50000 =  and ;

\ encode the corpus BPFT-REPS times and report tokens/sec + bytes/sec
2000 constant BPFT-REPS
: BPFT-THROUGHPUT ( -- )
   BPFD-CORPUS {: ca:ptr cu:n :}
   ca cu BPFT-IDS BPFT-CAP BPR-ENCODE BPFT-M !
   TIME:MONO-NS {: t0:n :}
   BPFT-REPS 0 ?do  ca cu BPFT-IDS BPFT-CAP BPR-ENCODE drop  loop
   TIME:MONO-NS t0 - {: dt:n :}
   s" throughput: reps=" type BPFT-REPS .  s"  bytes/enc=" type cu .
   s"  tokens/enc=" type BPFT-M @ .  s"  total-ns=" type dt . cr
   s" tokens/sec=" type BPFT-M @ BPFT-REPS *  1000000000 *  dt /  .
   s"  bytes/sec=" type cu BPFT-REPS *  1000000000 *  dt /  . cr ;

: BPFT-RUN ( -- )
   BPFT-PATH BPF-PRESENT? 0= if
      s" bpe-full: gpt2-vocab/vocab.bpe absent -> full-load leg SKIPPED (run maki/examples/nanogpt/fetch-gpt2-vocab.sh)" type cr
      0 0= TTRUE  exit then
   BPFT-PATH GPT2PIN:MERGES-SHA256$ BPF-LOAD
   s" full vocab loaded: merges=" type BPR-MERGES . cr
   BPFT-STRUCT?  TTRUE
   BPFT-PARITY?  TTRUE
   BPFD-RT-S BPFT-RT?  TTRUE
   BPFD-S1   BPFT-RT?  TTRUE
   BPFT-THROUGHPUT ;

T-RESET
BPFT-RUN
T-REPORT

;package
