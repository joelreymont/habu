---
title: Read the wid cells in the x86-64 kernel
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T20:51:57.292814+02:00"
---

Problem: arm64's lookup and namespace primitives (src/habu/habu1.f LFIND, LHEADROW, BPOLICYADMIT and the package-row tests, step 3a) read BOOT:GLOBAL-WL-CELL and NAMESPACE-WL-CELL, and the shared Habu code calls BOOT:GLOBAL-WL and BOOT:NAMESPACE-WL (steps 3c-3d); x86-64's twins still compare the immediate DICT-WL:NAMESPACE: src/habu/kernel-x64.f HEAD-ROW-HELPER, (two sites), DEF-CODE-RECLAIM-BODY, POLICY-ADMIT-BODY, REAL-WID,, NAMESPACE-RECORD-BODY, NAMESPACE-PRIVATE-BODY, ALIAS-RECORD-BODY, PACKAGE-SCOPE-BODY, and src/habu/prof-x64.f QUAL-INDEX,; whether any of them takes 0 as the global wordlist is not yet surveyed. boot-x64.f DATA-INIT, already writes both cells (step 3b). A machine asymmetry (DECISIONS.md one compiler); needed once x86-64 moves onto LINKER (/tmp/jerry-big/xbuild-order.md item 8). Found by the readers review (2026-11-04). Acceptance: every x86-64 kernel site that means the package marker or the global wordlist loads the cell, as habu1.f does; a probe on pulsar that writes other values into both cells sees the lookups follow them; test/x86-64-kernel-engine.f, test/x86-64-kernel-scope-find.f and test/x86-64-kernel-definition.f pass there. Files: src/habu/kernel-x64.f, src/habu/prof-x64.f. Verify: the probe and rows on pulsar. Depends: none (before item 8). Ownership: the named kernel-x64.f and prof-x64.f bodies. Claim: unassigned.
