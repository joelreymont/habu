---
title: "Delete the checker's file-boundary scratch retire"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T11:15:17.009511+02:00"
---

Problem: CHECKER-REG:FILE-SCRATCH-RETIRE (src/core/checker.f, package CHECKER-REG after CHECKER-TOP-SAVE) has no caller since the file-object tools were deleted, so CHECKER-CAPTURE-SCRATCH-PREPARE, the REG-EXT-SCRATCH-XT, REG-SCRATCH-SNAP-XT and REG-LATE-SCRATCH-SNAP-XT hooks and their installers (type-family.f TFAM-SCRATCH-RETIRE, render.f REND-SNAPSHOT-RESET, sumtype.f TDECL-SCRATCH-SNAPSHOT-RESET), SR-RETIRE, TDECL-OWNER-SNAPSHOT-XT and the declaration SNAPSHOT-RESET chain (generated-declaration-protection.f and its five participants) are never run, and comments (checker.f header, decl-event.f before OPEN) describe them as live. Acceptance: the chain and every word only it reached are gone; no comment describes it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xscr
