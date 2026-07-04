\ maki/scratch-model.f - the from-scratch training flagship: a windowed MLP over
\ feature-sequence windows plus its committed seeded synthetic dataset (dot
\ habu-maki-from-scratch; docs/model-cad.md Phase 9b; CAD-PLAN section 12).
\
\ Model (captured with MODEL: into the shared model IR): a batch of SC-BATCH=8
\ feature windows, each SC-FEAT=6 features (3 timesteps x 2 channels) flattened to
\ x:8x6, mapped by LINEAR GELU LINEAR to y:8x2 = per-window (mu, logvar):
\   x:8x6 -LINEAR(w1:6x16,b1:1x16)-> 8x16 -GELU-> 8x16 -LINEAR(w2:16x2,b2:1x2)-> 8x2.
\
\ Dataset is DETERMINISTIC and committed: a 32-bit LCG (Numerical-Recipes
\ constants a=1664525, c=1013904223, mod 2^32) seeded by a fixed integer produces
\ every input feature and the target. The target is a fixed linear+nonlinear
\ function of the window features plus a small deterministic LCG "noise" term, so
\ the mapping is learnable and re-running from the same seed reproduces the exact
\ same data - no runtime randomness. Parameters init from a SEPARATELY-seeded LCG
\ stream to small non-zero values (breaks the all-zero symmetry, keeps early
\ logits small). One concern: the model + its data; the training loop lives in
\ maki/scratch-train.f. maki -> habu only.

require maki/cad.f
require maki/array.f

package MAKI
public

\ ---- shapes (batch windows, features, hidden width, per-window outputs) ------
8  constant SC-BATCH       \ windows per batch
6  constant SC-FEAT        \ features per window (3 timesteps x 2 channels)
16 constant SC-HID         \ hidden width
2  constant SC-OUT         \ per-window outputs: mu, logvar

\ ---- element counts of each bound buffer (kept in sync with the shapes) ------
48 constant SC-XN          \ SC-BATCH * SC-FEAT
8  constant SC-YN          \ SC-BATCH targets (one per window)
96 constant SC-W1N         \ SC-FEAT * SC-HID
16 constant SC-B1N         \ 1 * SC-HID
32 constant SC-W2N         \ SC-HID * SC-OUT
2  constant SC-B2N         \ 1 * SC-OUT

\ ---- model-input slot indices (capture order = signature order) -------------
0 constant SC-X-SLOT
1 constant SC-W1-SLOT
2 constant SC-B1-SLOT
3 constant SC-W2-SLOT
4 constant SC-B2-SLOT

\ ---- bound host buffers (parameters are updated in place by the trainer) -----
create SC-X  SC-XN  cells allot
create SC-Y  SC-YN  cells allot
create SC-W1 SC-W1N cells allot
create SC-B1 SC-B1N cells allot
create SC-W2 SC-W2N cells allot
create SC-B2 SC-B2N cells allot

private

\ ---- deterministic 32-bit LCG (Numerical Recipes constants) -----------------
1664525    constant SC-LCG-A
1013904223 constant SC-LCG-C
$FFFFFFFF  constant SC-LCG-MASK       \ mod 2^32
variable SC-RNG

: SC-LCG-M ( -- r )  4294967296.0 ;   \ 2^32 as a float divisor

: SC-NEXT ( -- r )                    \ advance the LCG; return a float in [0,1)
   SC-RNG @ SC-LCG-A *  SC-LCG-C +  SC-LCG-MASK and  {: s:n :}
   s SC-RNG !
   s s>f SC-LCG-M f/ ;

: SC-UNIT ( -- r )  SC-NEXT 2.0 f*  1.0 f- ;   \ float in [-1,1)

public

\ committed seeds: data and parameters draw from independent LCG streams
$12345678 constant SC-DATA-SEED       \ 305419896
$9E3779B9 constant SC-PARAM-SEED      \ 2654435769 (golden-ratio constant)

private

\ ---- the true target function (a fixed linear part + one nonlinear product) ---
\ y(window) = 0.6*x0 - 0.4*x1 + 0.3*x2 + 0.5*x3*x4 - 0.2*x5  (+ noise, added by
\ the generator). The x3*x4 product is the nonlinearity the GELU MLP must fit.
: SC-ROW-TARGET ( ptr a -- r ) {: xr:ptr :}
   xr 0 T-GET {: a0:r :}  xr 1 T-GET {: a1:r :}  xr 2 T-GET {: a2:r :}
   xr 3 T-GET {: a3:r :}  xr 4 T-GET {: a4:r :}  xr 5 T-GET {: a5:r :}
   a0 0.6 f*
   a1 0.4 f* f-
   a2 0.3 f* f+
   a3 a4 f* 0.5 f* f+
   a5 0.2 f* f- ;

: SC-NOISE ( -- r )  SC-UNIT 0.05 f* ;   \ small deterministic target noise

\ generate one window: its SC-FEAT features (row-major), then its target + noise
: SC-GEN-ROW ( n -- ) {: r:n :}
   SC-FEAT 0 ?do  SC-UNIT  SC-X  r SC-FEAT * i +  T-SET  loop
   SC-X r SC-FEAT * T-AT  SC-ROW-TARGET  SC-NOISE f+  SC-Y r T-SET ;

public

\ fill SC-X (8x6) and SC-Y (8 targets) from the committed data seed
: SC-GEN-DATA ( -- )
   SC-DATA-SEED SC-RNG !
   SC-BATCH 0 ?do  i SC-GEN-ROW  loop ;

private

\ small non-zero init in [-0.1, 0.1)
: SC-SMALL ( -- r )  SC-UNIT 0.1 f* ;
: SC-FILL-SMALL ( ptr a n -- ) {: base:ptr len:n :}
   len 0 ?do  SC-SMALL base i T-SET  loop ;

public

\ initialise every parameter buffer from the committed parameter seed
: SC-INIT-PARAMS ( -- )
   SC-PARAM-SEED SC-RNG !
   SC-W1 SC-W1N SC-FILL-SMALL
   SC-B1 SC-B1N SC-FILL-SMALL
   SC-W2 SC-W2N SC-FILL-SMALL
   SC-B2 SC-B2N SC-FILL-SMALL ;

\ ---- canonical model capture (also leaves SCRATCH-MLP live in the IR) --------
\ MODEL: is a top-level parsing word (it cannot be wrapped in a definition), so
\ every consumer re-issues this exact line before use; this is the reference.
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;

end-package
