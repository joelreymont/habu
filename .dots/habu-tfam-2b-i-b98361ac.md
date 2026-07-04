---
title: "TFAM 2b-i: boot latch + raw-write protection (land together)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T02:07:47.900971+02:00\""
---

Sealing slice 1 of habu-tfam-2b-sealed-1b77662c. The friend latch is only sound if user source cannot forge it: today user stdin can do '5 CHECKER-PACKAGE-MODE !' and 'data-base <off> + !' (proven live). So the latch cell and checker-internal state must be write-protected FROM user source in the same change: census cat-3 raw writes (!, c!, +!, atomics, here/allot/,/c,, patch32) + checker-variable exposure + data-base/dbase@ leaked-pointer provenance. Latch seal chokepoint (validated): appended SEAL-FRIEND token in the cold-prefix generator at EMIT-COLD-PREFIX-SHARED/LCOLDPFX end (habu2.f ~801-806 after PFX-PROVIDE-FILES) and C-SOURCE-BAKED (~766); friend ON across PFX-LOAD-BASE-FILES (habu2.f 450-472). No per-file origin signal exists in include.f - the boot-latch-token approach is required. Design the protection mechanism first (see design scout artifact when it lands). Depends: TFAM 4 merge (checker.f serialization).

## DESIGN DECIDED (2026-07-04): docs/design-tfam-2b-i.md
Mechanism = Candidate C: one contiguous relocated protected arena holding the
friend latch + crown-jewel cells (CUR/WIDN/HOOK/PKG-*/DEFER-* from layout.f,
plus scattered checker.f variables like CHECKER-PACKAGE-MODE); address check
(~sub;sub;cmp;b.lo, PLEN in reserved reg, 0 pre-seal) at EVERY raw write sink
(! c! +! atomics patch32 cp! ndict! , c, allot snap-rebase + syscall/FFI
writers); latch lives inside the arena so it is self-sealing; SEAL-FRIEND token
appended by the cold-prefix generator. Plus narrow A (drop zero-tool-use engine
mutators from post-seal search order, non-load-bearing) and B provenance only on
syscall/FFI pointer args in checked code. D (mprotect) rejected - no sealable
window (checker writes interleave with user evaluate). Migration burden of C is
~zero (all legit dangerous-word uses target cells outside the arena; the 16-file
dict-introspection list in the design doc is what a BROAD A would break - keep A
narrow). set-check friend-gating is a separate dot.
