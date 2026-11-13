---
title: "Read the engine's tree at exec on macOS"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T00:16:17.449325+01:00"
---

Problem: on macOS src/core/include.f ENGINE-INIT (:1042) resolves APPLE-EXE's path (:1003) when the root is first needed, not at exec, so an hb started through ~/.local/lib/habu as an install renames the link can pair its old image with the new tree (tools/install.f header). Linux resolves /proc/self/exe, which names the running image's own tree. Acceptance: on the Mac, an hb started through the link with stdin held, the link then moved to another tree, prints its own tree for SOURCE-ROOT:ENGINE$. Depends: none. Claim: unassigned.
