---
title: Bind forge quantifiers by the argument, not the context
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T21:26:44.790360+02:00"
---

Problem: the fetch R1 fix (parked, see habu-decide-linear-safety-ed03eec8) refuses a bare @ whose output a concrete type binds, but a one-line generic helper launders it: `: GETP ( ptr a -- a ) @ ;` certifies, and `8 MEM-ALLOC-PTR GETP` then yields hnd (G1), a null ptr n (G2), a code value (G3), a fresh QLIN:tok (G4) or a pointer made from an integer (G5), because the raw argument leaves a free and the context binds it. Binding the pointee by a comparison also gets through: `( ptr hnd -- hnd ) 8 MEM-ALLOC-PTR tuck = drop @` (E1), since = unifies the raw pointer with ptr hnd. The store side has the same shape: the body rule's wrapper hole `[the_tick] ST swap STORE2`. Acceptance: at a call, a callee quantifier that appears in the inputs only under ptr or a storage family and bare in the outputs is bound by the pointee of the argument; when that pointee is raw the quantifier is raw (POINTEE-RAW), never left free for the context, so G1-G5 are refused while a typed-pointer argument still yields its pointee; a pointer comparison binds no pointee, so E1 is refused; the store wrapper hole closes by the same binding; refused fixtures for G1-G5, E1 and the STORE2 shape beside the R1 fixtures, certifying controls for generic accessors over typed pointers and spans (SPAN:AT and kin); the docs/forth.md fetch-forge paragraph states the rule. Depends: habu-decide-linear-safety-ed03eec8 (the body rule and the R1 fix land first); until then the R1 docs state how far the primitive rule reaches. Claim: unassigned.
