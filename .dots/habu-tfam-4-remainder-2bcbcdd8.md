---
title: "TFAM 4 remainder: SC-QUOT, uncapped arity, package-aware SIG"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T04:20:40.889455+02:00"
---

Deferred from habu-tfam-4-replace-97a246e3 (landed: registry-driven parse, family-id identity, reentrant scratch, arity diagnostics). (1) SC-QUOT quotation payload schema nodes: net-new kind mirroring VR-QUOT/EN-QUOT/MK-QUOT - parse, persist, instantiate, copy, render, malformed-row negatives (PLAN item 4 acceptance line). (2) Uncapped per-param argc: SoA arg rows + VN/EN nodes still 4-slot (PARAM-MAX-ARGS); scratch is growable but PARAM-ARGC-FULL? caps at 4 with SGBAD-ARITY - grow the SoA/VN/EN arg storage so arity>4 families work (all current families <=4). (3) Package-aware SIG resolution: SIG-FAM? resolves in global scope only; package-local family resolution + same-tail cross-package non-unification AT SIG-PARSE level blocked on item 6 grammar. Also see [[habu-tfam-nested-param-09fa2004]] (stored nested-sig SIGSEGV, two engine-suite regressions held out with NOTE). Depends: TFAM 6 for (3); (1)/(2) independent.
