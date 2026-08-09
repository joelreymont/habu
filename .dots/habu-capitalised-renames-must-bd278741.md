---
title: Capitalised renames must cost zero, not a call
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:23:52.569553+02:00"
---

Invisible-to-census defect (2026-08-10): a rename or op word in capitals (SWAP, DROP, +, @) does not refuse - RESOLVE-STEP falls through to HIR-WORD:RESOLVE-CALLABLE, the case-insensitive engine dictionary answers, and the chain compiles a real call where lower case costs zero instructions. The census scores these as SUCCESSES, so no bucket sizes it. May be closed by habu-fold-the-dialect-bcd8fe87 IF its fold reaches renames/ops and its module-identity twins prove it (the lane has been told); this dot exists so the concern cannot silently drop if that lane's seam only covers control words. Acceptance: a capitalised rename compiles to a module structurally identical to its lower-case twin (zero extra instructions), pinned by fixture; same for an op word. Files: src/compiler/native/hir-word.f or elaborate.f RESOLVE-STEP. Depends: habu-fold-the-dialect-bcd8fe87 (verify before claiming: it may already close this).
