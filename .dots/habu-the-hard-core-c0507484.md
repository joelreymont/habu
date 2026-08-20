---
title: "The hard core: phantom mints, self-arming, typestate"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:44.320860+02:00"
---

Phase 6 of 4fd12d60, the honest irreducibles: (a) ~59 PTX phantom-mint sites (cg-attention.f:157 shape: body pushes 0, signature mints extent/mask/stage no input carries; ad-saved.f has E-PTX-NOIMPL throw bodies under phantom signatures) - needs a witness design for dependent GPU indices, a redesign not a migration; (b) ~47 checker self-arming sites (hide.f:21 BFR-CHECK-OFF 0 set-check, UEND!, verify-source TRUST-SIGNATURE) - a capability to defeat the checker must not be expressible in checked habu: SEAL INSIDE THE ENGINE (move to engine primitives, delete the checked-language surface); (c) maki/typestate.f 10 sites where phase tokens have no runtime witness - redesign with witnesses; (d) 16 machine-code-emission sites stay PRIM-TRUSTED-ONLY! sealed prims by design (checker.f:5814). Each is its own design probe; none blocks phases 1-5. Blocks the final deletion.

MEASURED 2026-08-20 (trusted-5, from the cast sweep habu-cast-definer-330-1f5980b8).

(c) maki/typestate.f, the 10 phantom-typestate rows. Nine of them are
shape-honest one-cell retypes and are now CAST: - RAW>DECL, RAW>ELAB, RAW>SOLVED,
RAW>LEGAL, RAW>DRAFT, RAW>COMPLETE, RAW>DRAFTED, RAW>VERIFIED, RAW>EMITTED. That
removes nine trust rows and changes NOTHING about what this leaf owns: every mint
still takes a literal 0, so a stage value still carries no evidence that its
transition ran. The cast makes the shape honest; it does not manufacture a
witness. The tenth row, ART:MINT-BUILD-PROOF ( -- build-proof ) 0 ;, is
arity-0-in, and the cast form cannot express it (7129 E-CAST-ARITY) - a nullary
token constructor is not a retype, so it stays a word. The witness redesign this
leaf owns is untouched, and no statement here was weakened to make a conversion
fit.

(a) the ~59 PTX phantom mints. Six of them are refused by CAST: with 7135
E-CAST-OWNER purely because of where they sit: lib/ptx/cg-attention.f
Q-REG/K-REG/V-REG/O-REG inside package ATTN, and maki/infer/gpt2-attention-cg.f
ROW-REG/CACHE-REG inside package GPT2-ATTN. lib/ptx/cg.f's nine identical
register mints CERTIFY cleanly because that file is at global scope, where the
engine declares span/matrix/uniform - but they could not be landed either: the
package lint refuses any edit to a definition outside a package, and it refuses
the untouched line on pristine master too, so those nine plus tile-v4a.f
V4-ALIGN are frozen until this surface gets a package. So the witness design this
leaf owns must also settle which package owns the kernel families; today the
answer is "the global one", any package that wants to mint into them is refused
by name, and the global file cannot be edited without a packaging decision.
