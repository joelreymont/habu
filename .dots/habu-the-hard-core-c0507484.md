---
title: "The hard core: phantom mints, self-arming, typestate"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:44.320860+02:00"
---

Phase 6 of 4fd12d60, the honest irreducibles: (a) ~59 PTX phantom-mint sites (cg-attention.f:157 shape: body pushes 0, signature mints extent/mask/stage no input carries; ad-saved.f has E-PTX-NOIMPL throw bodies under phantom signatures) - needs a witness design for dependent GPU indices, a redesign not a migration; (b) ~47 checker self-arming sites (hide.f:21 BFR-CHECK-OFF 0 set-check, UEND!, verify-source TRUST-SIGNATURE) - a capability to defeat the checker must not be expressible in checked habu: SEAL INSIDE THE ENGINE (move to engine primitives, delete the checked-language surface); (c) maki/typestate.f 10 sites where phase tokens have no runtime witness - redesign with witnesses; (d) 16 machine-code-emission sites stay PRIM-TRUSTED-ONLY! sealed prims by design (checker.f:5814). Each is its own design probe; none blocks phases 1-5. Blocks the final deletion.
