---
title: "Package-owned PRIM axioms retire the TRUSTED: definer"
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T17:47:16.868624+02:00"
---

User directive 2026-08-11: the standard is PRIM:, TRUSTED: is to be retired ENTIRELY - and the mechanism check proves it feasible and stronger. Evidence: the trusted-only gate is one bit consulted at one site (src/core/checker.f:7615, TRY-PRIMS rejects a PE-TRUSTED-ONLY prim from any checked body; the flag is PE-TRUSTED-ONLY in PE-FLAGS, set by PRIM-TRUSTED-ONLY! at :5125) with no location awareness, while the checker already tracks its open package (CHECKER-PKG-CONTEXT). Capability: replace the bit with a package OWNER on the PE row (PRIM-OWNED-BY! <pkg>); the gate compares the checker's open package and admits a CHECKED caller inside the owner, rejecting everywhere else. The boundary strengthens: today any file may open a TRUSTED: body and leave the checker; after, callers stay checked and the capability is location-bound. Failing probe: a checked caller of addrmap-set inside publish.f's own package earns E-CAP-TRUSTED today. First consumers: the chain's 10 unconvertible sites (publish.f CODE-WINDOW/RELOC-EXTERNAL/RELOC-ADDR/RETARGET-REC, reach.f POKE, then EV->evaluate and RUN-WORD->execute owned by their packages; TRAILER@/PTR>N keep their type-level owner dots). Scale warning: the tree carries 1243 TRUSTED: sites total (core decl machinery, cuda-driver, ~700 test rows) - retiring the DEFINER is a campaign of per-owner sweeps after this capability lands, not one landing. Two-stage rules apply if the gate change is seed-affecting. Files: src/core/checker.f. Depends: habu-turn-the-registry-4c064064 (the 21 Class-A conversions land first).
