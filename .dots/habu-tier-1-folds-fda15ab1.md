---
title: Tier 1 folds a pre-window created word into AOT window code
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T18:35:58.507799+02:00"
---

When a window word mentions a `variable` created before AOT-ARM:WINDOW-OPEN, tier 1 compiles that word's DATA address as a literal, so AOT-CAPTURE:CAPTURE refuses with "recorded address site outside both window spans". Tier 0 (jerry stack, xynwzssy "Fold constants and created words at tier 0") keeps such a mention a call when the word's entry is below AOT-CELLS:B0-CELL (habu2.f faddr), and captures it. Fix: apply the same B0 rule to tier 1 folding. Repro (refuses on master d11d9a68 with "recorded address site outside both window spans"; with `0 set-tier` in place of `1 set-tier` it prints "captured"):

```
\ A window word mentions a created word made before the window opened.
require src/habu/aot-arm.f
require src/habu/aot-capture.f
variable PREV
AOT-ARM:WINDOW-OPEN
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
1 set-tier
: XUSE ( -- n ) PREV @ ;
AOT-ARM:WINDOW-CLOSE
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" captured" type cr
```

Reported by jerry.
