\ ad-saved.f - typed saved-value vocabulary for AUTO-DERIVED backward kernels.
\
\ The reverse pass (lib/ptx/ad.f) emits nonlinear adjoints that reference saved forward
\ primals/outputs by name (SAVED-X / SAVED-Y / SAVED-MX / SAVED-S / SAVED-Z) and a sign
\ flip NEG. Until now those were bare string tokens, so a generated backward could not
\ TYPE-CHECK. Giving each a real typed effect makes the auto-derived backward a CHECKED
\ kernel (the "checked" half of habu-ad-softmax-rows; emit/buffer-reload is the
\ save-vs-recompute lowering, habu-ad-thread-saved).
\
\ TRUSTED: because the value is materialised by the save-vs-recompute pass the checker
\ cannot infer; the declared effect is the contract. SAVED-* bodies throw E-PTX-NOIMPL
\ until the buffer stash/reload lands; NEG already has codegen (EMIT-NEG = neg.f32). Load
\ after lib/ptx/cg-collective.f (EMIT-NEG) and lib/ptx/collective.f.
\ Retirement owner: habu-adg-lowering-multi-24043a69.

require lib/ptx/rep.f

\ sign flip - polymorphic over tile / uniform (forward tile-NEG is self-adjoint; the
\ B-/B/ adjoints negate a block-uniform). One emit (neg.f32) serves both. The
\ phantom `a` flows THROUGH the ( n -- n ) neg emitter, so NEG is a CHECKED caller
\ of PTXREP:REP1 (lib/ptx/rep.f) - no strip-and-re-mint trusted boundary.
: NEG ( a -- a )  [: EMIT-NEG ;] PTXREP:REP1 ;

\ saved register tiles (a nonlinear op's saved input / output)
TRUSTED: SAVED-X ( -- tile<f32,b,m> )  E-PTX-NOIMPL throw ;
TRUSTED: SAVED-Y ( -- tile<f32,b,m> )  E-PTX-NOIMPL throw ;
TRUSTED: SAVED-Z ( -- tile<f32,b,m> )  E-PTX-NOIMPL throw ;

\ saved block-uniform scalars (BLOCK-MAX's max; B/'s divisor)
TRUSTED: SAVED-MX ( -- uniform<f32> )  E-PTX-NOIMPL throw ;
TRUSTED: SAVED-S  ( -- uniform<f32> )  E-PTX-NOIMPL throw ;

\ SAVED-A and ZERO. are NOT words here: they exist only as vjp.f table TOKENS,
\ resolved by the ad-gen lowering (recompute binding / EMIT-ZERO). Only the
\ tokens above have a checked-KERNEL: certification consumer (ad-saved-test.f).
