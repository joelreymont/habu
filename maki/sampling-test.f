\ maki/sampling-test.f - acceptance for the host-side sampling module
\ (maki/sampling.f, dot habu-infer-sampling-ops-d9c456f7). Model-independent:
\ every property is proven over a committed synthetic logit table
\ (maki/sampling-torch-ref-data.f, torch 2.9.1 f64, seed 20260721, R=4 rows x V=8)
\ and the committed shared LCG (maki/train-core.f). Sections:
\
\   (A) f64 softmax parity: SM-FWD over each committed logit row matches torch's
\       softmax within 1e-7 (measured max abs deviation 4.6e-10).
\   (B) greedy == torch argmax EXACTLY: SMP-ARGMAX and SMP-NEXT(temp=0) both return
\       torch's argmax id for every row (exact-id parity - deterministic leg).
\   (C) top-k == 1 == argmax EXACTLY: SMP-NEXT(k=1) returns torch's argmax.
\   (D) top-p == 1.0 == plain multinomial: SMP-NEXT(p=1) and a bare softmax+sample
\       draw the identical id sequence at a fixed seed.
\   (E) top-p nucleus SUPPORT == torch: SMP-TOPP!(0.9) keeps exactly torch's P=0.9
\       nucleus per row and renormalises the kept mass to 1.
\   (F) fixed-seed multinomial is a LOCKED sequence, run-twice bit-identical (the
\       committed LCG's own expected ids - torch's multinomial RNG differs, so this
\       is maki's pinned sequence, not a torch cross-check).
\   (G) distribution SHAPE vs torch: a 4096-draw histogram is a locked run-twice
\       count vector AND every bin frequency is within 0.02 of the torch softmax
\       probability (measured max 0.006).
\   (H) guards are RED-FIRST, named, before any table read: temp<0 -> E-SMP-TEMP,
\       k<1 and k>vocab -> E-SMP-TOPK, p<=0 and p>1 -> E-SMP-TOPP.
\   (I) nucleus is monotone / degenerate: top-p 0.2 collapses row 0 to {argmax}.
\   (J) temperature -> 0 concentrates: temp 0.01 drives the argmax probability > 1-1e-6.
\
\ Names are SMT-/SREF- prefixed (the maki suite shares one dictionary across every
\ -test.f). maki -> habu only.

require lib/test.f
require lib/prelude.f
require lib/float.f
require maki/array.f
require maki/softmax.f
require maki/train-core.f
require maki/sampling.f
require maki/sampling-torch-ref-data.f

package MAKI

12 constant SMT-M              \ locked id-sequence length
4096 constant SMT-N            \ histogram draw count

create SMT-WRK SREF-V cells allot     \ working logit/prob row (SMP-NEXT mutates it)
create SMT-PRB SREF-V cells allot     \ softmax probability row
create SMT-SC  SREF-V cells allot     \ caller scratch for top-p
create SMT-HIST SREF-V cells allot    \ multinomial histogram (float counts)
create SMT-A 16 cells allot           \ sampled id sequence A
create SMT-B 16 cells allot           \ sampled id sequence B

: SMT-A@ ( n -- n )  SMT-A swap T-GET f>s ;
: SMT-B@ ( n -- n )  SMT-B swap T-GET f>s ;
: SMT-HIST@ ( n -- n )  SMT-HIST swap T-GET f>s ;

\ copy committed logit row 0 into the working buffer
: SMT-CP0 ( -- )  SREF-V 0 ?do  0 SREF-LOG i T-GET  SMT-WRK i T-SET  loop ;
\ softmax committed logit row 0 into SMT-PRB
: SMT-SOFT0 ( -- )  0 SREF-LOG SMT-PRB SREF-V SM-FWD ;

\ ---- (A) softmax parity: max abs deviation vs the committed torch probs ------------
: SMT-ROW-DEV ( n -- r ) {: row:n :}
   row SREF-LOG SMT-PRB SREF-V SM-FWD
   0.0
   SREF-V 0 ?do
      SMT-PRB i T-GET  row SREF-PRB i T-GET  f-  fabs
      2dup f< if nip else drop then
   loop ;
: SMT-PARITY-DEV ( -- r )
   0.0
   SREF-R 0 ?do  i SMT-ROW-DEV  2dup f< if nip else drop then  loop ;

\ ---- (D/F) sampled id sequences ---------------------------------------------------
\ plain multinomial: softmax row 0 once, then draw M ids at a fixed seed
: SMT-SEQ-PLAIN ( n n ptr a -- ) {: seed:n m:n buf:ptr :}
   SMT-SOFT0  seed SC-SEED!
   m 0 ?do  SMT-PRB SREF-V SMP-SAMPLE s>f  buf i T-SET  loop ;
\ via SMP-NEXT(temp=1,k=V,p=1): copy row 0, dispatch, draw M ids at a fixed seed
: SMT-SEQ-NEXT ( n n ptr a -- ) {: seed:n m:n buf:ptr :}
   seed SC-SEED!
   m 0 ?do
      SMT-CP0
      SMT-WRK SMT-SC SREF-V SREF-V 1.0 1.0 SMP-NEXT s>f  buf i T-SET
   loop ;
: SMT-AB-MATCH? ( -- bool )
   true  SMT-M 0 ?do  SMT-A i T-GET  SMT-B i T-GET  f= 0= if drop false then  loop ;

\ ---- (G) histogram + shape check --------------------------------------------------
: SMT-HINIT ( -- )  SREF-V 0 ?do  0.0 SMT-HIST i T-SET  loop ;
: SMT-HISTOGRAM ( n n -- ) {: seed:n nn:n :}
   SMT-SOFT0  SMT-HINIT  seed SC-SEED!
   nn 0 ?do
      SMT-PRB SREF-V SMP-SAMPLE {: id:n :}
      SMT-HIST id T-GET 1.0 f+  SMT-HIST id T-SET
   loop ;
: SMT-SHAPE-OK? ( -- bool )   \ every bin frequency within 0.02 of torch prob[0]
   true
   SREF-V 0 ?do
      SMT-HIST i T-GET  SMT-N s>f f/
      0 SREF-PRB i T-GET  f-  fabs
      0.02 f>  if drop false then
   loop ;

\ ---- (E) nucleus support match vs torch mask --------------------------------------
: SMT-NUC-OK? ( -- bool )     \ SMP-TOPP!(0.9) support == torch P=0.9 mask, all rows
   true
   SREF-R 0 ?do
      i SREF-LOG SMT-PRB SREF-V SM-FWD
      SMT-PRB SMT-SC SREF-V SREF-TOPP SMP-TOPP!
      SREF-V 0 ?do
         SMT-PRB i T-GET 0.0 f>  if 1 else 0 then          \ kept?
         j SREF-MSK i T-GET f>s                            \ torch mask bit
         <> if drop false then
      loop
   loop ;
: SMT-NUC-SUM ( n -- r )      \ renormalised nucleus mass for a row after topp(0.9)
   {: row:n :}
   row SREF-LOG SMT-PRB SREF-V SM-FWD
   SMT-PRB SMT-SC SREF-V SREF-TOPP SMP-TOPP!
   SMT-PRB SREF-V T-SUM ;

\ ---- (I) nucleus degenerate: top-p 0.2 on row 0 -> {argmax} -----------------------
: SMT-NUC02 ( -- )
   SMT-SOFT0  SMT-PRB SMT-SC SREF-V 0.2 SMP-TOPP! ;
: SMT-OUT-MASS ( n -- r )     \ mass outside index keep (argmax id)
   {: keep:n :}
   0.0  SREF-V 0 ?do  i keep <> if  SMT-PRB i T-GET f+  then  loop ;

\ ---- (J) temperature concentration ------------------------------------------------
: SMT-TEMP-MAX ( -- r )       \ argmax prob of row 0 after temp 0.01
   SMT-CP0  SMT-WRK SREF-V 0.01 SMP-TEMP!
   SMT-WRK SMT-PRB SREF-V SM-FWD
   SMT-PRB 4 T-GET ;

\ ---- (H) red-first guard triggers -------------------------------------------------
: SMT-G-NEGT ( -- )  SMT-CP0  SMT-WRK SMT-SC SREF-V SREF-V -1.0 1.0 SMP-NEXT drop ;
: SMT-G-K0   ( -- )  SMT-CP0  SMT-WRK SMT-SC SREF-V 0 1.0 1.0 SMP-NEXT drop ;
: SMT-G-KBIG ( -- )  SMT-CP0  SMT-WRK SMT-SC SREF-V SREF-V 1+ 1.0 1.0 SMP-NEXT drop ;
: SMT-G-P0   ( -- )  SMT-CP0  SMT-WRK SMT-SC SREF-V SREF-V 1.0 0.0 SMP-NEXT drop ;
: SMT-G-PBIG ( -- )  SMT-CP0  SMT-WRK SMT-SC SREF-V SREF-V 1.0 1.1 SMP-NEXT drop ;
: SMT-G-PNEG ( -- )  SMT-CP0  SMT-WRK SMT-SC SREF-V SREF-V 1.0 -0.5 SMP-NEXT drop ;

SREF-LOAD
T-RESET

\ ================= (A) f64 softmax parity vs torch ===================================
SMT-PARITY-DEV 0.0000001 f< TTRUE               \ max |maki - torch| prob < 1e-7

\ ================= (B) greedy == torch argmax EXACTLY ================================
0 SREF-LOG SREF-V SMP-ARGMAX  0 SREF-ARG T=
1 SREF-LOG SREF-V SMP-ARGMAX  1 SREF-ARG T=
2 SREF-LOG SREF-V SMP-ARGMAX  2 SREF-ARG T=
3 SREF-LOG SREF-V SMP-ARGMAX  3 SREF-ARG T=
SMT-CP0  SMT-WRK SMT-SC SREF-V SREF-V 0.0 1.0 SMP-NEXT  0 SREF-ARG T=   \ temp=0 greedy

\ ================= (C) top-k == 1 == argmax EXACTLY =================================
SMT-CP0  SMT-WRK SMT-SC SREF-V 1 1.0 1.0 SMP-NEXT  0 SREF-ARG T=

\ ================= (D) top-p == 1.0 == plain multinomial ============================
424242 SMT-M SMT-A SMT-SEQ-PLAIN
424242 SMT-M SMT-B SMT-SEQ-NEXT
SMT-AB-MATCH? TTRUE                              \ p=1.0 dispatch == bare softmax+sample

\ ================= (E) top-p nucleus SUPPORT == torch mask ==========================
SMT-NUC-OK? TTRUE                               \ kept set == torch P=0.9 nucleus, all rows
0 SMT-NUC-SUM 0.999999 f> TTRUE                 \ nucleus renormalises to 1 (row 0)
0 SMT-NUC-SUM 1.000001 f< TTRUE

\ ================= (F) fixed-seed multinomial is a LOCKED sequence ==================
424242 SMT-M SMT-A SMT-SEQ-PLAIN
0 SMT-A@ 4 T=  1 SMT-A@ 4 T=  2 SMT-A@ 5 T=  3 SMT-A@ 4 T=
4 SMT-A@ 4 T=  5 SMT-A@ 7 T=  6 SMT-A@ 3 T=  7 SMT-A@ 3 T=
8 SMT-A@ 5 T=  9 SMT-A@ 4 T= 10 SMT-A@ 3 T= 11 SMT-A@ 5 T=      \ [4,4,5,4,4,7,3,3,5,4,3,5]
424242 SMT-M SMT-A SMT-SEQ-PLAIN
0 SMT-A@ 4 T=  5 SMT-A@ 7 T= 11 SMT-A@ 5 T=                     \ run-twice bit-identical

\ ================= (G) distribution SHAPE vs torch =================================
777 SMT-N SMT-HISTOGRAM
0 SMT-HIST@ 468 T=  1 SMT-HIST@ 168 T=  2 SMT-HIST@ 141 T=  3 SMT-HIST@ 668 T=
4 SMT-HIST@ 1636 T= 5 SMT-HIST@ 369 T=  6 SMT-HIST@ 434 T=  7 SMT-HIST@ 212 T=
SMT-SHAPE-OK? TTRUE                             \ every bin freq within 0.02 of torch prob
777 SMT-N SMT-HISTOGRAM  4 SMT-HIST@ 1636 T=    \ run-twice bit-identical

\ ================= (H) guards are red-first (named throws) ==========================
' SMT-G-NEGT E-SMP-TEMP TTHROWS
' SMT-G-K0   E-SMP-TOPK TTHROWS
' SMT-G-KBIG E-SMP-TOPK TTHROWS
' SMT-G-P0   E-SMP-TOPP TTHROWS
' SMT-G-PBIG E-SMP-TOPP TTHROWS
' SMT-G-PNEG E-SMP-TOPP TTHROWS

\ ================= (I) nucleus degenerate: top-p 0.2 -> {argmax} ====================
SMT-NUC02
SMT-PRB 4 T-GET 0.999999 f> TTRUE               \ all mass on the argmax
4 SMT-OUT-MASS 0.000001 f< TTRUE                \ out-of-nucleus mass ~ 0

\ ================= (J) temperature -> 0 concentrates ===============================
SMT-TEMP-MAX 0.999999 f> TTRUE                  \ temp 0.01 -> argmax prob > 1-1e-6

T-REPORT

;package
