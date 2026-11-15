---
title: "Look up an x86-64 part's head without a frame"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:35:39.059922+01:00"
---

Problem: src/habu/kernel-x64.f:499-509 still copies a part's path into a PACKAGE-PATH:CAP (256-byte) stack frame and refuses a longer one, while the arm64 kernel looks the head up in place (habu-name-the-pkg-741eb597 step 3); PACKAGE-PATH:CAP (src/habu/layout.f) survives only for it. Acceptance: at the x86-64 step, the x86-64 head lookup takes a path of any length with no frame, and PACKAGE-PATH goes. Depends: habu-name-the-pkg-741eb597. Claim: unassigned.
