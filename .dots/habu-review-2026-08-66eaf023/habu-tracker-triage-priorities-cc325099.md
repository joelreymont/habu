---
title: "tracker triage: priorities, empty epics, malformed leaves"
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.037454+02:00"
---

Problem: 1488 open + 5 active, 991 ready; 505 open P1 (a third of the backlog); epics with no open children: habu-epic-type-habu-a34713f0, habu-epic-hard-cut-a684f24d, habu-epic-maki-autograd-132b247a (the last two have no child directory); two sibling epics coordinate sections of one file (habu-epic-model-cad-70b629a9 / -9549fdd8); 'status: done' x2 (non-canonical: habu-bisect-engine-growth-759ffd33, habu-maki-cad-f-040aab6a); .dots/habu-pkg-bootstrap-codegen-c2e644a7.md holds two documents; 15 open dots cite /tmp/ paths as evidence; habu-epic-hard-cut names /tmp/REVIEW-CODEGEN-1.md as canonical; tools/lint/text.f:337 SPLIT+ dies past 1024 lines with no filename; three process-on-process dots (habu-make-the-dot-1f0a7a5c, habu-run-remote-gpu-b523f6b2, habu-bind-promotion-evidence-e9218897) have the process as their only consumer. Acceptance: P1 <= ~30 by ruling, empty epics closed or merged, statuses canonical, the two-document leaf merged, /tmp evidence inlined or dropped, SPLIT+ names the file, the three process dots declined unless a failing probe exists. Files: .dots/, tools/lint/text.f. Verify: dot ls counts recorded before/after; dot-dep-lint 0 findings. Depends: none. Ownership: tracker. Claim: unassigned.
