---
title: "Float L1: arm64 d-register files in the T0 seam"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T11:33:45.509721+02:00"
closed-at: "2026-11-10T21:52:35.428266+02:00"
close-reason: "obsolete: its design /tmp/jerry-float/design.md, its lane changes and workspace jerry-t0f1 are gone; the parked work stays in habu's c277c8a5"
---

Workspace jerry-t0f1 (trzopyok on rsnyxvvn). Design /tmp/jerry-float/design.md (Plan a50e3cff, lead cut: no FCMP0; f0< f0= are 0.0 + FCMP2). RFILE/GPR/FPR, FVREGS, FILE-OF, E-T0-FILES; arm64 NFVREG 24 d8-d31, FREG, file-aware LIT/PUSH/POP/SPILL/FILL/MOV/EXCHANGE/SLOT-*; x64 FVREGS 0. Seam-driven rows.
