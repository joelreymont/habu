---
title: Give the dialect key its own type
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:54:02.158536+02:00"
---

hir-word.f's fold key (KEY-SPELL/KEY-SYM, merged ce6488f7) is an ordinary ir-symbol-id, so a future reader querying with a RAW symbol gets a fail-closed no-row (pinned by test) but the checker cannot catch the mistake at compile time. A distinct HIR-WORD:key type would make it a compile error; sized at ~22 public signatures plus call sites - a migration, not a quick edit. Acceptance: querying with a raw symbol no longer type-checks; the fold is the only constructor of the key type. Files: src/compiler/native/hir-word.f, src/compiler/native/elaborate.f + callers. Depends: none.
