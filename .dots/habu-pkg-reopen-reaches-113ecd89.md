---
title: "Package reopen reaches privates: rc 134 from user source"
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T16:31:49.670433+02:00"
---

VERIFIED ON MASTER (mark-1, 2026-08-20): 'package PRIM-LINK private KEY-SYM ;package' in ordinary user source SIGABRTs rc 134 - reopening a package reaches its privates, the third live defect of the marking class. Fix direction: sealed packages (get-current prot-wid-add, the xref.f PKG-AUTH shape C-PACKAGE-SEAL-GUARD refuses to reopen) for the engine's own packages, or reopen-time marking of private records. ACCEPTANCE MUST INCLUDE: retire the interim src/core/internal-mark.f row in package-diff-lint-core.f GLOBAL-IMPLEMENTATION? and convert internal-mark.f to a sealed package (the row's own header names this; the asm.f precedent shows the shape - row leaves, its positives become fixture negatives). An interim exception whose correctness rests on a defect staying open retires with the defect, by acceptance, never by comment. Blocks nothing; the seal pilot c65f76cc is unblocked NOW by 41532ee7's landing.
