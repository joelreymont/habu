---
title: "Locate the engine's interpret-level refusals"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T14:31:40.370636+01:00"
closed-at: "2026-11-13T22:10:41.859885+01:00"
---

Problem: interpret-level refusals print no place. In the engine, LFLOORREC (src/habu/habu2.f:2696-2703, a data-stack underflow caught by the guard page) prints its class head, its place through LPLACE (:2699), its message and token, and throws through LEVALREC; each die site that ends in LCOMPILEDIE (:2745) writes its place after its class head through LPLACE (:2709). It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces. Bare library throws and the Maki case are habu-uncaught-throws-name-1a43b055's.
Acceptance: each of these refusals prints its file and line through the one located tail (LPLACE in the engine), with its exit status and throw code unchanged; a file holding the one line `then` under bin/hb --load on a rebuilt hb prints its path and line 1 and exits 70.
Depends: none.
Claim: unassigned.
