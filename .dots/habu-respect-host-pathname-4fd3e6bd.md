---
title: Respect host pathname byte limits
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T19:11:20.496958+03:00"
---

Otto demonstrated a 1024-byte Darwin scan path passes PATHZ but fails lstat with ENAMETOOLONG and is silently treated as a vanished entry by Dots. PATH-CAP in src/core/util.f is 1024 excluding NUL, while Darwin PATH_MAX is 1024 including NUL. Linux currently inherits 1024 though its limit is 4096 including NUL. Set the target-specific public path capacity at the owning OS boundary and propagate the existing shared constant. Inspect existing path-boundary E2Es before edits; verify the longest valid path, the first refused path, and preserved failing-syscall errno through the public FS API. The separate FS errno feature is habu-preserve-filesystem-os-9b2a57a7; do not duplicate it. Dots owns its caller behavior.

Status 2026-10-21: design in progress. PATH-CAP (src/core/util.f) is used about 1600 times in about 330 files, including fixed layouts (src/lib/fs.f walk PATHS slot; tools/native-definition-cache.f frame offsets), so a target-specific value needs a design for compile-time sizing on native and cross builds before any edit. Claim: agent=carl workspace=none (design).
