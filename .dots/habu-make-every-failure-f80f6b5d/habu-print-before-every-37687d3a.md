---
title: Print before every silent kernel exit
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T14:02:21.825720+02:00"
---

Problem: the arm64 kernel's one exit line, `<class> habu: <text>[ <16 hex>] lr <16 hex>` written by src/habu/crash.f LDIE-LINE, is not yet written everywhere. Still silent or off the shape: B-TASK-GUARD's exit 79 (src/habu/habu1.f:1546, reached by a task calling cp! or ndict!), B-LOWER-GUARD, DEFWRITE:REFUSE-AT (src/habu/habu2.f:1095), the habu1.f exits at :5468 and :5500, `hb: catch frame corrupt` (habu2.f:3038, no class or lr), the E-POLICY lines (habu1.f:4513ff, no lr), NFETCH-CHECK:BAD-TAG (src/compiler/native/fetch-check.f:20, `hb: bad layout tag`) and src/lib/map.f:118. habu2.f:3041 entered from LFLOORREC prints the return after LPLACE's BL, not its caller's. Acceptance: each of these writes the one line through LDIE-LINE with its exit status unchanged; NFETCH-CHECK:BAD-TAG prints `E-BAD-TAG habu: <path>:<line>: layout tag <n> outside 0..<limit-1>`, exit 85; src/lib/map.f:118 prints before throwing 85; habu2.f:3041 prints its caller's lr on every entry. Depends: none. Claim: unassigned.
