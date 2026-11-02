---
title: Resolve a qualified type through a part
status: closed
priority: 2
issue-type: task
created-at: "2026-11-02T14:51:51.196131+02:00"
closed-at: "2026-11-02T16:13:36.687535+02:00"
close-reason: "landed: a qualified type resolves through a part; claims and family gate scope-free; full arm64 suite 514/514, chain gen 4=5"
---

Problem: inside package A and its parts a qualifier B:X names the part A:B first (docs/forth.md Packages; checker.f HEAD-ROW does it for words), but a qualified type in a signature resolved only against a top-level package (src/core/type-family.f TFQ-SPLIT? copied the written qualifier), so Move T0 into COMP's LABEL:branch was refused 'unknown type'. Acceptance: qualified family types resolve through the part first, a part's private type only inside it, nothing reaches a part from outside its parent; TFAM-CLAIMED? stays scope-free (a DEFLINEAR/VALUE-RECORD name is claimed when any scope reads it as a family). Files: src/core/type-family.f, docs/forth.md, docs/forth-card.md, test/type-decl-suite.f, tools/check-test-lib.f. Verify: candidate and whitebox builds; type-decl, outer-interpret, checker-completion, using-import, package-seal, enum-decl, check-cli-boundary; full arm64 suite and generation chain at landing with Move T0 into COMP. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-parttype (wrlznpkm plus a fold-in after review).
