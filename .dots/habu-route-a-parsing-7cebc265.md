---
title: "Route a parsing word's target through reference resolution"
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T12:29:00.136683+02:00"
---

Found closing b83bcfa5 (probe on the leaf): with NO global of the name and a used public defer HOOK, bare 'is HOOK' is structurally unreachable by the checker - 'is' parses its target, so it never becomes a checked reference, and the only net is the runtime diagnostic (rc 70 with the qualify hint, merged 75337472). The capability: route parsing words' targets ('is', and by inspection its class) through the same reference resolution checked references use, so the checker can refuse or bind them; same family as the retired CHECKER-DEFINED? split (1504bbde) and the E-USING-SHADOW-GLOBAL ambiguity rule, which covers the global+public collision but not the public-only case. Files: src/core/checker.f, src/habu/habu2.f (J-IS). Depends: none.
