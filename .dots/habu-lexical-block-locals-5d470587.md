---
title: Lexical block locals
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T16:55:56.893909+02:00"
---

Problem: existing local-first lookup works, but locals are still definition-top only; {: :} inside if/loop/dead paths is rejected by compiler/checker guards. Fix: give locals lexical block lifetime with scope push/pop on control frames, local shadowing ordinary words inside scope, and no leakage across else/then/loop/repeat/quotation boundaries. Acceptance: mid-block locals compile/check where scoped; out-of-scope local use rejects; quotation capture remains rejected unless explicitly modeled; C1 dot can close.
