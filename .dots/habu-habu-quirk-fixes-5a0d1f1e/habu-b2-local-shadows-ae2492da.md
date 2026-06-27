---
title: "B2: local-shadows-builtin error"
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T13:15:33.496641+02:00"
---

Checker/compiler: when a {: :} local name collides with a built-in word (i/j/k loop indices, code, dup,...), error at the {: :} ('local X shadows built-in; rename') instead of emitting the bare token + exit 75 with no diagnostic. src/habu locals compiler. Pairs with C4.
