---
title: Name the quotation-reads-local refusal
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T11:20:33.532729+02:00"
---

Found by the typed-locals acceptance audit: {: x:n :} [: x ;] execute prints only the bare token x on stderr and exits 75 - no error name, unlike E-UNDEFINED: x on the sibling path. Diagnostics quality: the refusal should carry its name. Files: src/core/checker.f (the quotation-boundary scope refusal). Depends: none.
