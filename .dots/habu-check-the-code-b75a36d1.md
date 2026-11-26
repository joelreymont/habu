---
title: Check the code floor a failed build rewinds to
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T11:16:18.935218+01:00"
---

Problem: read, not reproduced: XREF REC-LOW (src/habu/xref.f:683-692) gives an alias with an inline name its source's start, and FLOOR-FROM (:707-711) takes the lowest over the build's rows, so a build that EXPORTs an earlier word and then fails may set the floor at a surviving word's start, against xref.f:658-661; FORGET-SPACE (src/habu/linker.f:5551) would then free code a live record names. Acceptance: a build-mode reproducer shows it and its fix lands, or the code that rules it out is named. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rewind.
