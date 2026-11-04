---
title: "Drop the TFAM- prefix from TFAM's words"
status: open
priority: 3
issue-type: task
created-at: "2026-11-04T13:08:16.077394+02:00"
---

Problem: src/core/type-family.f names 139 words TFAM-..., inside package TFAM, so callers write TFAM:TFAM-STORAGE-QUOT? (16 distinct words are called qualified from other files; layout-buffer.f calls seven TFAM:TFAM-STORAGE-* words). Joel 2026-11-04: "why the TFAM prefix? do you have any other storage?"; the prefix only repeats the package. Acceptance: no word in package TFAM starts TFAM-; each new name is the old one without the prefix; callers updated; a tail that clashes with another TFAM word or a global is listed for its own name before the lane starts. A rename lane: names by that one rule, no other change. Files: src/core/type-family.f and its callers (rg 'TFAM-'). Verify: engine rebuild, full suite. Depends: T1b (habu-mark-typed-storage-1cc20b92), which renames TFAM-STORAGE-HAS-QUOT? to TFAM:HAS-ADDRESS?. Claim: unassigned. Workspace: none yet.
