---
title: "Cut BOOT:USER-END's comment to its rule"
status: closed
priority: 3
issue-type: task
created-at: "2026-11-04T20:19:23.001163+02:00"
closed-at: "2026-11-04T21:31:42.196332+02:00"
close-reason: "landed lmxltsuo 8df6fcac: USER-END's comment is two lines (reviewed U1, wording from the review)."
---

Problem: the comment above USER-END, moved as is into src/habu/boot-cells.f (BOOT move, habu-move-the-boot-42e58e9f), runs 10 lines and tells history ("Arming the seed without splitting the streams fired it..."), against docs/forth-card.md §9 (one or two lines; state the rule and the fact, no narrative). Found by the BOOT-move review. Acceptance: the comment states what USER-END is, who writes and reads it and the rule that keeps it right, in at most a few lines with no narrative; no code change. Files: src/habu/boot-cells.f. Verify: read against card §9; no build needed (comment only). Depends: habu-move-the-boot-42e58e9f. Ownership: that comment. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fold (commit lmxltsuo).
