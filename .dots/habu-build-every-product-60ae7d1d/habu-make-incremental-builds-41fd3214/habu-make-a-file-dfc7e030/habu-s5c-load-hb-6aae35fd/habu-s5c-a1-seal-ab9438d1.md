---
title: "S5c A1: seal a package by a flag on its record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T13:19:27.263819+02:00"
closed-at: "2026-11-06T17:03:25.781167+02:00"
close-reason: "Landed as 7b497647 under B 97ee2eea and A2a 3bfcbf1c on master 7bafa920. Proof: chain run on the stack's tip, fixpoint at generation 2, 574 s, rc 0 (/tmp/jerry-gate/chain.log); full suite on that engine 396 of 397, the red check-cli-boundary a load timeout (CHILD-HANG-MS at load 50, dot habu-bound-the-lsp-c5cbc535) that passes alone on the same engine (354 s). Review fold-ins after the gate are comments only."
---

Problem: the lead decided (2026-11-06): a sealed package is one flag on its record, set by seal-package, read only when package reopens. The typed loop still seals by protected wordlists and a name list (src/habu/packages.f:147-160 PKG-SEALED?, :236-241 PKG-SEAL). Acceptance: DNAME-SEALED at bit 45 (layout.f DNAME-FLAG-BITS 19 and its three literal/mirror twins); PKG-SEAL sets it on the open package's record through an XREF word; PKG-SEALED? reads only it; the five engine idiom lines become seal-package; outer-interpret.f seal cases that test the old machinery leave the engine/loop parity; a build case proves the flag lands in the build's record. Files: src/habu/layout.f, src/core/checker.f, src/habu/engine-origin-early.f, bootstrap/cg/forth.fs, src/habu/packages.f, src/habu/xref.f, src/core/internal-mark.f, src/core/generated-declaration.f, src/core/generated-declaration-dictionary.f, test/outer-interpret.f, test/linker.f. Verify: workspace candidate build, then rows linker outer-interpret package-seal sealed-system-package checker-surface. Depends: none. Ownership: those files. Claim: agent=worker workspace=.jj-ws.noindex/jerry-seal
