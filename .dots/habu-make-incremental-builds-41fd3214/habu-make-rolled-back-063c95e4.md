---
title: Make rolled-back cold builds deterministic
status: active
priority: 2
issue-type: task
created-at: "2026-10-25T04:38:32.755908+01:00"
---

Workspace carl-roll (base qtstsksr). A STRUCTURE declaration rolled back inside evaluate-closed+catch gives two cold builds in different caches differing by 34 bytes (2 at ~10551379 + signature). Blocks wl GENERATED-CASE fresh-cache compare. Brief ~/.cache/tmp/carl-file/brief-roll.md.
