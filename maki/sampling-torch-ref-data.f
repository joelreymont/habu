\ maki/sampling-torch-ref-data.f - committed PyTorch reference for the host-side
\ sampling ops (maki/sampling.f, dot habu-infer-sampling-ops-d9c456f7). GENERATED
\ DATA - regenerate rather than hand-edit.
\
\ Provenance: torch 2.9.1a0+git87f2f4c (CPU, f64), seed torch.manual_seed(20260721):
\   logits = torch.randn(4, 8, dtype=torch.float64)      \ R=4 rows, V=8 vocab
\   probs  = torch.softmax(logits, dim=1)
\   argmax = logits.argmax(dim=1)                         \ -> [4, 5, 5, 2]
\   top-p mask (P=0.9): per row sort probs descending, cumulative-sum, take the
\     smallest prefix whose cumulative reaches P (>= 1 token), tau = that prefix's
\     boundary probability, membership = (probs >= tau). randn logits have no ties,
\     so this positional nucleus and maki/sampling.f's value-based keep (p_i >= tau)
\     agree exactly. Nucleus sizes per row: [6, 6, 7, 5].
\
\ These are the DISTRIBUTION reference (probs), the EXACT-ID reference (argmax, the
\ greedy / top-k=1 target), and the nucleus SUPPORT reference (mask). The RNG-locked
\ sampled sequences live in maki/sampling-test.f: torch's multinomial RNG differs
\ from the committed LCG, so exact sampled-id parity is not meaningful and the suite
\ instead pins maki's own LCG draws and cross-checks their empirical shape against
\ these torch probs. Measured maki SM-FWD-vs-torch softmax deviation on this table:
\ max abs 4.6e-10 (f64), the floor behind the suite's 1e-7 parity tolerance.
\
\ Layout: LOGITS (R x V row-major), PROBS (R x V), ARGMAX (R ids as floats), MASK
\ (R x V, 1.0 in the P=0.9 nucleus else 0.0). Fill words load fail-closed: a count
\ mismatch throws E-SREF-DATA. sampling-torch-ref owns -5511. maki -> habu only.

require lib/prelude.f
require lib/float.f
require maki/array.f

-5511 constant E-SREF-DATA    \ committed table fill arity mismatch

package MAKI
public

4 constant SREF-R             \ committed rows
8 constant SREF-V             \ committed vocab width
: SREF-TOPP ( -- r )  0.9 ;   \ committed top-p threshold behind MASK

private

\ ---- sequential fail-closed table fill (open, append, close-with-count) ----
variable SREF-TP              \ open table base
variable SREF-TI              \ next element index

: SPD( ( ptr a -- )  SREF-TP !  0 SREF-TI ! ;
: SPD  ( r -- )      SREF-TP @ SREF-TI @ T-SET  SREF-TI @ 1+ SREF-TI ! ;
: )SPD ( n -- )      SREF-TI @ <> if E-SREF-DATA throw then ;

create SREF-LOGITS SREF-R SREF-V * cells allot
create SREF-PROBS  SREF-R SREF-V * cells allot
create SREF-ARGMAX SREF-R cells allot
create SREF-MASK   SREF-R SREF-V * cells allot

: SREF-FILL-LOGITS ( -- )
   SREF-LOGITS SPD(
   0.17509006400548108 SPD    -0.78835955981860062 SPD   -1.2050540031989012 SPD    0.46764612040894576 SPD
   1.3606449896518964 SPD     -0.11359982303616108 SPD    0.035653130900053308 SPD  -0.64686964547361769 SPD
   0.50944836341740274 SPD    1.2558041817154559 SPD     -0.50985939668753111 SPD    0.2520807840624788 SPD
   -0.68204235054595286 SPD   1.2919414050066111 SPD     -0.87811986442568957 SPD    1.0816686062672256 SPD
   -0.74025869866471972 SPD   0.36280456050134247 SPD    -0.5827051512927397 SPD    -0.28411726582492791 SPD
   -0.1455044062977566 SPD    0.8766351630119148 SPD      0.78158041361311348 SPD    0.4634649846892468 SPD
   0.4679873524925065 SPD     -1.0904866885583819 SPD     1.8723756733239973 SPD    -0.57932107111678521 SPD
   -0.80472045741038312 SPD   1.5295534361265246 SPD     -0.92704701076461693 SPD   -0.41780650165892314 SPD
   32 )SPD ;

: SREF-FILL-PROBS ( -- )
   SREF-PROBS SPD(
   0.12042202757841267 SPD    0.045949953904090624 SPD   0.030291235166763564 SPD   0.16134719372119399 SPD
   0.39408136380450054 SPD    0.090225544227958962 SPD   0.10474884540390929 SPD    0.052933836193170519 SPD
   0.11421010408800134 SPD    0.24090329532089025 SPD    0.041212104441566762 SPD   0.08829400317413863 SPD
   0.034693385752793154 SPD   0.24976808117996765 SPD    0.028516177066015679 SPD   0.20240284897662658 SPD
   0.046457285814061181 SPD   0.13999358007603507 SPD    0.054384916933404956 SPD   0.073308366187292057 SPD
   0.084207803757491845 SPD   0.23402481750992063 SPD    0.21280418381731672 SPD    0.15481904590447745 SPD
   0.10564625685328845 SPD    0.022233991637404131 SPD   0.43030085833390924 SPD    0.037069253898611487 SPD
   0.029588580821580633 SPD   0.30541221764746496 SPD    0.026181732911704343 SPD   0.043567107896036766 SPD
   32 )SPD ;

: SREF-FILL-ARGMAX ( -- )
   SREF-ARGMAX SPD(
   4.0 SPD  5.0 SPD  5.0 SPD  2.0 SPD
   4 )SPD ;

: SREF-FILL-MASK ( -- )
   SREF-MASK SPD(
   1.0 SPD  0.0 SPD  0.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD
   1.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD  0.0 SPD  1.0 SPD  0.0 SPD  1.0 SPD
   0.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD  1.0 SPD
   1.0 SPD  0.0 SPD  1.0 SPD  1.0 SPD  0.0 SPD  1.0 SPD  0.0 SPD  1.0 SPD
   32 )SPD ;

public

\ load every committed table (fail-closed on arity)
: SREF-LOAD ( -- )
   SREF-FILL-LOGITS  SREF-FILL-PROBS  SREF-FILL-ARGMAX  SREF-FILL-MASK ;

\ row bases / cells for the four tables
: SREF-LOG ( n -- ptr a )  SREF-V * cells SREF-LOGITS + ;
: SREF-PRB ( n -- ptr a )  SREF-V * cells SREF-PROBS + ;
: SREF-MSK ( n -- ptr a )  SREF-V * cells SREF-MASK + ;
: SREF-ARG ( n -- n )      SREF-ARGMAX swap T-GET f>s ;

;package
