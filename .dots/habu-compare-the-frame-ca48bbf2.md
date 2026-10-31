---
title: Compare the frame e2e dependency line
status: open
priority: 3
issue-type: task
created-at: "2026-10-27T07:55:17.967177+02:00"
---

test/native-file-frame-e2e.f saves DEP-LINE (con-dep.f's object line) but never compares it, so a change in that object's inputs passes unseen. Acceptance: the saved line is asserted against the warm build's, or the save is deleted if another assertion already covers the inputs. Workspace: none yet. Claim: unassigned.
