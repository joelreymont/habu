---
title: "Make the checker's global internals private"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.889423+01:00"
---

Problem: checked code still reaches checker machinery: two staged include.f globals a program can call (835ee1dc) and XREF-REQUIRE-INDEX (88e188e1). Acceptance: its children are closed. Depends: none. Claim: unassigned.
