---
title: Read the wid cells in the x86-64 kernel
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T20:51:57.292814+02:00"
---

Problem: arm64's lookup and namespace primitives (src/habu/habu1.f LFIND, LHEADROW, BPOLICYADMIT and the package-row tests) read BOOT:GLOBAL-WL-CELL and NAMESPACE-WL-CELL, and the shared Habu code calls BOOT:GLOBAL-WL and BOOT:NAMESPACE-WL; x86-64's twins still use the immediate WL:NAMESPACE: src/habu/kernel-x64.f :666, :673, :3847, :4147, :5006, :5091, :5121, :5151, :5196 (HEAD-ROW-HELPER, DEF-CODE-RECLAIM-BODY, POLICY-ADMIT-BODY, REAL-WID,, NAMESPACE-RECORD-BODY, NAMESPACE-PRIVATE-BODY, ALIAS-RECORD-BODY, PACKAGE-SCOPE-BODY) and src/habu/prof-x64.f QUAL-INDEX, (:221); whether any of them takes 0 as the global wordlist is not yet surveyed. src/habu/boot-x64.f DATA-INIT, already writes both cells (:254-255). A machine asymmetry, needed once x86-64 moves onto the linker (habu-build-x86-64-3ed6f45d). Acceptance: every x86-64 kernel site that means the package marker or the global wordlist loads the cell, as habu1.f does; a probe on pulsar that writes other values into both cells sees the lookups follow them; test/x86-64-kernel-engine.f, test/x86-64-kernel-scope-find.f and test/x86-64-kernel-definition.f pass there. Depends: none (before x86-64 moves onto the linker). Claim: unassigned.
