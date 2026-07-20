\ maki/examples/nanogpt/generate-test.f - acceptance for the autoregressive
\ generation / sampling loop (maki/examples/nanogpt/generate.f). A tiny nanoGPT-shaped
\ LM (token+position embed -> LM head: GATHER ADD LINEAR, T=4 C=4 V=6) is captured as
\ ONE MODEL:, deterministically LCG-initialised, and trained a few Adam steps on a
\ shift task (committed mean-CE lock, run-twice bit-identical) - a genuinely TRAINED
\ model. Generation then runs FORWARD-ONLY over it and every sampling property is
\ proven mechanically:
\
\   (A) training halves the mean CE and is deterministic (committed milli, run-twice).
\   (B) greedy decode (temp=0, true argmax) is a LOCKED token sequence, run-twice equal.
\   (C) the window ROLLS: after generating, the model's id window holds the last-T ids.
\   (D) forward READS the window: the last-position argmax differs by the last token.
\   (E) fixed-seed multinomial sampling is a LOCKED sequence, run-twice equal.
\   (F) the temperature 0-limit EQUALS argmax: at T=0.01 the argmax prob > 1-1e-6, a
\       small-T sample returns the argmax, and greedy(T=0) returns the same argmax.
\   (G) top-k provably EXCLUDES out-of-k tokens: total out-of-k prob < 1e-6.
\   (H) inverse-CDF sampling stays IN BOUNDS: u in [0,1) never falls off the row end
\       (one-hot rows hit the sure index; a sum-just-under-1 row still stays in [0,n)).
\   (I) generated ids DECODE round-trip through the tokenizer (every id in-vocab).
\   (J) guards are red-first: empty prompt, k<1, k>vocab and negative temperature throw
\       named errors; a prompt longer than the block CROPS to the last T (does not die).
\
\ Names are GN-prefixed (the maki suite shares one dictionary across every -test.f).
\ maki -> habu only.

require lib/test.f
require lib/prelude.f
require lib/float.f
require maki/cad.f
require maki/backward.f
require maki/executor.f
require maki/loss-tensor.f
require maki/optim-tensor.f
require maki/array.f
require maki/embedding.f
require maki/matmul.f
require maki/softmax.f
require maki/examples/nanogpt/generate.f
require maki/examples/nanogpt/tokenizer.f

package MAKI

\ ---- shapes: T positions, C channels, V vocab; logit element count ------------------
4 constant GNT   4 constant GNC   6 constant GNV   24 constant GNON

\ ---- bound host buffers: 5 model inputs (4 trained params + integer ids) + targets/seed
create GN-WTE GNV GNC * cells allot   create GN-IDS GNT cells allot   create GN-WPE GNT GNC * cells allot
create GN-WLM GNC GNV * cells allot   create GN-BLM GNV cells allot
create GN-TGT GNT cells allot         create GN-SEED GNON cells allot

\ ---- Adam moment buffers (m,v) for the 4 trained params -----------------------------
create GN-WTEM GNV GNC * cells allot  create GN-WTEV GNV GNC * cells allot
create GN-WPEM GNT GNC * cells allot  create GN-WPEV GNT GNC * cells allot
create GN-WLMM GNC GNV * cells allot  create GN-WLMV GNC GNV * cells allot
create GN-BLMM GNV cells allot        create GN-BLMV GNV cells allot

\ ---- init LCG (own stream, independent of generate.f's SC-* sampling LCG) ------------
variable GN-RNG
: GN-NEXT ( -- r )  GN-RNG @ 1664525 * 1013904223 + $FFFFFFFF and dup GN-RNG !  s>f 4294967296.0 f/ ;
: GN-UNIT ( -- r )  GN-NEXT 2.0 f* 1.0 f- ;
: GN-SMALL ( ptr a n -- ) {: p:ptr n:n :}  n 0 ?do  GN-UNIT 0.1 f*  p i T-SET  loop ;

\ committed init: small weights + a shift task (id t = t mod V, target = (t+1) mod V)
: GN-INIT ( -- )
   $C0FFEE GN-RNG !
   GN-WTE GNV GNC * GN-SMALL  GN-WPE GNT GNC * GN-SMALL  GN-WLM GNC GNV * GN-SMALL  GN-BLM GNV GN-SMALL
   GNT 0 ?do  i GNV mod s>f GN-IDS i T-SET   i 1+ GNV mod s>f GN-TGT i T-SET  loop ;

\ ---- Adam state (step count + running decay powers) ---------------------------------
: GN-BET1 ( -- r ) 0.9 ;  : GN-BET2 ( -- r ) 0.999 ;  : GN-EPS ( -- r ) 0.00000001 ;  : GN-LR ( -- r ) 0.1 ;
variable GN-T  variable GN-B1T  variable GN-B2T
: GN-ARESET ( -- )  0 GN-T !  1.0 GN-B1T !  1.0 GN-B2T !
   0.0 GN-WTEM GNV GNC * T-FILL 0.0 GN-WTEV GNV GNC * T-FILL 0.0 GN-WPEM GNT GNC * T-FILL 0.0 GN-WPEV GNT GNC * T-FILL
   0.0 GN-WLMM GNC GNV * T-FILL 0.0 GN-WLMV GNC GNV * T-FILL 0.0 GN-BLMM GNV T-FILL 0.0 GN-BLMV GNV T-FILL ;
: GN-TICK ( -- )  GN-T @ 1+ GN-T !  GN-B1T @ GN-BET1 f* GN-B1T !  GN-B2T @ GN-BET2 f* GN-B2T ! ;
: GN-C1 ( -- r ) 1.0 GN-B1T @ f- ;  : GN-C2 ( -- r ) 1.0 GN-B2T @ f- ;

\ ---- executor forward output (last node = the TxV logits) + per-slot gradient node ---
: GN-OUT ( -- ptr a )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;
: GN-GRAD ( n -- ptr a )  MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;
: GN-INVR ( -- r )  1.0 GNT s>f f/ ;

\ write the mean-scaled y-onehot(target) seed cotangent from the logits; return mean CE
: GN-LOSS-SEED ( -- r )
   GN-OUT {: ob:ptr :}
   ob GN-TGT GN-SEED GNT GNV GNT LOSS:TT-XENT-SEED
   GNON 0 ?do  GN-SEED i T-GET GN-INVR f*  GN-SEED i T-SET  loop
   ob GNT GNV GN-TGT GNT LOSS:TT-XENT  GN-INVR f* ;

: GN-UPD ( ptr a n ptr a ptr a n -- ) {: p:ptr slot:n mp:ptr vp:ptr n:n :}
   GN-LR GN-BET1 GN-BET2 GN-EPS GN-C1 GN-C2  p  slot GN-GRAD  mp vp n  OPTIM:TT-ADAM! ;

: GN-BIND ( -- )
   GN-WTE 0 MIR-SLOT-ID EX-BIND  GN-IDS 1 MIR-SLOT-ID EX-BIND  GN-WPE 2 MIR-SLOT-ID EX-BIND
   GN-WLM 3 MIR-SLOT-ID EX-BIND  GN-BLM 4 MIR-SLOT-ID EX-BIND  GN-SEED BW-SEED-SLOT@ EX-BIND ;

\ forward slice -> loss+seed -> full IR -> Adam-update every trained param (skip ids)
: GN-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N  GN-LOSS-SEED {: loss:r :}  EX-RUN  GN-TICK
   GN-WTE 0 GN-WTEM GN-WTEV GNV GNC * GN-UPD   GN-WPE 2 GN-WPEM GN-WPEV GNT GNC * GN-UPD
   GN-WLM 3 GN-WLMM GN-WLMV GNC GNV * GN-UPD   GN-BLM 4 GN-BLMM GN-BLMV GNV GN-UPD
   loss ;

: GN-MILLI ( r -- n )  1000.0 f* 0.5 f+ f>s ;
variable GN-IL  variable GN-FL
: GN-TRAIN ( n -- ) {: n:n :}
   n 0 ?do  GN-STEP {: l:r :}  i 0= if l GN-IL ! then  i n 1- = if l GN-FL ! then  loop ;

\ build backward once, init params, bind every buffer
: GN-SETUP ( -- )  GN-INIT  GN-ARESET  BW-BUILD  EX-RESET  GN-BIND ;

\ ---- generation drivers: forward-only over the trained model ------------------------
create GN-PROMPT GNT cells allot     \ length-T prompt (its LAST token drives the orbit)
create GN-GEN 8 cells allot          \ generated ids (float cells)
create GN-BACK 16 allot              \ decoded bytes (round-trip)
8 constant GN-G                      \ tokens to generate

\ forward the current WIN (bound to the ids slot) and return the (TxV) logit buffer base
: GN-FWD ( -- ptr a )  BW-FWD-N@ EX-RUN-N  GN-OUT ;

\ committed prompt: ids ending in token 0 (t = (T-1-i) mod V) -> a varied greedy orbit
: GN-SEED-PROMPT ( -- )  GNT 0 ?do  GNT 1- i - GNV mod s>f  GN-PROMPT i T-SET  loop ;

: GN-GREEDY ( -- )
   GN-SEED-PROMPT  GN-PROMPT GNT GN-IDS GNT GEN-CROP drop
   GN-IDS GNT GN-GEN GN-G GNV 0.0 GNV [: GN-FWD ;] GEN-RUN ;   \ temp=0 greedy, k=V

: GN-SAMPLED ( -- )
   GN-SEED-PROMPT  GN-PROMPT GNT GN-IDS GNT GEN-CROP drop
   424242 SC-SEED!
   GN-IDS GNT GN-GEN GN-G GNV 1.0 GNV [: GN-FWD ;] GEN-RUN ;  \ temp=1 seeded, k=V

\ last-position argmax for a window whose every id is x (forward-reads-window probe)
: GN-FX ( n -- n ) {: x:n :}
   GNT 0 ?do  x s>f GN-IDS i T-SET  loop
   GN-FWD  GNT 1- GNV * T-AT  GNV GEN-ARGMAX ;

\ GN-GEN cell i as an int
: GN-GEN@ ( n -- n )  GN-GEN swap T-GET f>s ;
\ the model's id window cell i as an int
: GN-IDS@ ( n -- n )  GN-IDS swap T-GET f>s ;

\ ---- synthetic rows for the model-independent sampling proofs ------------------------
create GN-ROW GNV cells allot        \ a ramp logit row [1,2,3,4,5,6]
create GN-PRB GNV cells allot        \ softmax scratch
: GN-RAMP ( -- )  GNV 0 ?do  i 1+ s>f  GN-ROW i T-SET  loop ;
: GN-OUTK ( -- r )  0.0  4 0 ?do  GN-PRB i T-GET f+  loop ;      \ prob mass outside top-2 (idx 0..3)

create GN-P3  3 cells allot          \ a proper distribution (sums to 1)
create GN-P3U 3 cells allot          \ a distribution summing to just under 1
create GN-OH0 3 cells allot          \ one-hot at index 0
create GN-OH2 3 cells allot          \ one-hot at index 2
: GN-SETP ( -- )
   0.2 GN-P3 0 T-SET  0.3 GN-P3 1 T-SET  0.5 GN-P3 2 T-SET
   0.2 GN-P3U 0 T-SET 0.3 GN-P3U 1 T-SET 0.49 GN-P3U 2 T-SET
   1.0 GN-OH0 0 T-SET 0.0 GN-OH0 1 T-SET 0.0 GN-OH0 2 T-SET
   0.0 GN-OH2 0 T-SET 0.0 GN-OH2 1 T-SET 1.0 GN-OH2 2 T-SET ;

\ sample a 3-wide row over many seeds; return true iff every draw equals EXP (EXP<0: only
\ require in-bounds [0,3)). Proves inverse-CDF never falls off the row and hits sure indices.
variable GN-BOK
: GN-BOUNDS? ( ptr a n -- n ) {: p:ptr exp:n :}
   true GN-BOK !
   300 0 ?do  i SC-SEED!  p 3 GEN-SAMPLE {: s:n :}
      exp 0< if  s 0 < s 3 >= or if false GN-BOK ! then
      else  s exp <> if false GN-BOK ! then  then
   loop  GN-BOK @ ;

\ ---- red-first guard triggers (each throws its named error) --------------------------
: GN-G-EMPTY ( -- )  GN-PROMPT 0 GN-IDS GNT GEN-CROP drop ;                 \ empty prompt
: GN-G-K0    ( -- )  GN-RAMP  GN-ROW GNV 1.0 0 GEN-NEXT drop ;              \ k < 1
: GN-G-KBIG  ( -- )  GN-RAMP  GN-ROW GNV 1.0 GNV 1+ GEN-NEXT drop ;        \ k > vocab
: GN-G-NEGT  ( -- )  GN-RAMP  GN-ROW GNV -1.0 GNV GEN-NEXT drop ;           \ temp < 0

\ prompt longer than the block crops to the last T (does NOT die)
create GN-LONG 6 cells allot
: GN-FILL-LONG ( -- )  6 0 ?do  i s>f  GN-LONG i T-SET  loop ;              \ [0,1,2,3,4,5]
: GN-CROP-LONG ( -- n )  GN-FILL-LONG  GN-LONG 6 GN-IDS GNT GEN-CROP ;      \ -> 4, window = [2,3,4,5]

\ round-trip corpus: 6 distinct bytes -> vocab size 6 = GNV
: GN-CORPUS ( -- ptr u8 n )  s" abcdef" ;
: GN-RT-DECODE ( -- n )  GN-CORPUS TOK-BUILD  GN-GREEDY  GN-GEN GN-G GN-BACK 16 TOK-DECODE ;

T-RESET

\ ================= (A) training: mean CE halved, deterministic (committed milli) =======
MODEL: GNMOD ( wte:6x4 ids:4x1 wpe:4x4 wlm:4x6 blm:1x6 -- logits ) GATHER ADD LINEAR ;
GN-SETUP
BW-FWD-N@ 3 T=                                  \ forward slice = GATHER ADD LINEAR
GN-G GN-TRAIN
GN-IL @ GN-MILLI 1779 T=                         \ committed initial mean CE (determinism lock)
GN-FL @ GN-MILLI 429  T=                         \ committed final mean CE   (determinism lock)
GN-FL @ GN-IL @ f< TTRUE                         \ loss decreased
GN-FL @ GN-IL @ 0.5 f* f< TTRUE                  \ at least halved
GN-INIT GN-ARESET  GN-G GN-TRAIN  GN-FL @ GN-MILLI 429 T=   \ run-twice bit-identical

\ ================= (B) greedy decode is a deterministic LOCKED sequence ================
GN-GREEDY
0 GN-GEN@ 1 T=  1 GN-GEN@ 2 T=  2 GN-GEN@ 4 T=  3 GN-GEN@ 4 T=
4 GN-GEN@ 4 T=  5 GN-GEN@ 4 T=  6 GN-GEN@ 4 T=  7 GN-GEN@ 4 T=       \ [1,2,4,4,4,4,4,4]
GN-GREEDY  0 GN-GEN@ 1 T=  1 GN-GEN@ 2 T=  7 GN-GEN@ 4 T=            \ run-twice identical

\ ================= (C) the window ROLLED to the last-T generated ids ===================
GN-GREEDY
0 GN-IDS@ 4 T=  1 GN-IDS@ 4 T=  2 GN-IDS@ 4 T=  3 GN-IDS@ 4 T=       \ last 4 of [..4,4,4,4]

\ ================= (D) forward READS the window (last-token sensitivity) ================
0 GN-FX 1 T=                                     \ window ending in 0 -> argmax 1
3 GN-FX 4 T=                                     \ window ending in 3 -> argmax 4 (differs)

\ ================= (E) fixed-seed sampling is a deterministic LOCKED sequence ==========
GN-SAMPLED
0 GN-GEN@ 3 T=  1 GN-GEN@ 4 T=  2 GN-GEN@ 4 T=  3 GN-GEN@ 4 T=
4 GN-GEN@ 3 T=  5 GN-GEN@ 5 T=  6 GN-GEN@ 3 T=  7 GN-GEN@ 4 T=       \ [3,4,4,4,3,5,3,4]
GN-SAMPLED  0 GN-GEN@ 3 T=  5 GN-GEN@ 5 T=  7 GN-GEN@ 4 T=          \ run-twice identical

\ ================= (F) temperature 0-limit EQUALS argmax ==============================
GN-RAMP  GN-ROW GNV GEN-ARGMAX 5 T=                                  \ argmax of [1..6] = idx 5
GN-RAMP  GN-ROW GNV 0.0 GNV GEN-NEXT 5 T=                            \ greedy(T=0) = argmax (no divide)
GN-RAMP  GN-ROW GNV 0.01 GEN-TEMP!  GN-ROW GN-PRB GNV SM-FWD
GN-PRB 5 T-GET 0.999999 f> TTRUE                                     \ argmax prob -> 1 as T->0
GN-RAMP  777 SC-SEED!  GN-ROW GNV 0.01 GNV GEN-NEXT 5 T=             \ a small-T sample returns argmax

\ ================= (G) top-k provably EXCLUDES out-of-k tokens =========================
GN-RAMP  GN-ROW GNV 2 GEN-TOPK!  GN-ROW GN-PRB GNV SM-FWD
GN-OUTK 0.000001 f< TTRUE                                            \ out-of-top-2 prob < 1e-6
GN-PRB 4 T-GET GN-PRB 5 T-GET f+ 0.999999 f> TTRUE                   \ the kept top-2 hold ~all mass

\ ================= (H) inverse-CDF sampling stays IN BOUNDS ============================
GN-SETP
GN-P3  -1 GN-BOUNDS? TTRUE                       \ proper distribution: always in [0,3)
GN-P3U -1 GN-BOUNDS? TTRUE                       \ sum just under 1: still always in [0,3)
GN-OH0  0 GN-BOUNDS? TTRUE                       \ one-hot@0: every draw is 0
GN-OH2  2 GN-BOUNDS? TTRUE                       \ one-hot@2: every draw is 2

\ ================= (I) generated ids DECODE round-trip through the tokenizer ===========
GN-RT-DECODE GN-G T=                             \ decode returns GN-G bytes (every id in-vocab)
GN-BACK 0 + c@ 97 >=  GN-BACK 0 + c@ 102 <=  and TTRUE   \ first decoded byte in 'a'..'f'

\ ================= (J) guards are red-first (named throws) + crop-not-die ==============
' GN-G-EMPTY E-GEN-PROMPT TTHROWS
' GN-G-K0    E-GEN-TOPK   TTHROWS
' GN-G-KBIG  E-GEN-TOPK   TTHROWS
' GN-G-NEGT  E-GEN-TEMP   TTHROWS
GN-CROP-LONG 4 T=                                \ prompt of 6 crops to the block T (=4), no throw
0 GN-IDS@ 2 T=  1 GN-IDS@ 3 T=  2 GN-IDS@ 4 T=  3 GN-IDS@ 5 T=       \ window = last T = [2,3,4,5]

T-REPORT

;package
