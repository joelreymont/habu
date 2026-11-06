---
title: "Design the checker's facts on dictionary entries"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T09:25:20.915632+02:00"
---

Problem: DECISIONS 'where the checker's facts live' (2026-11-05/06): a word's facts go on its dictionary entry and types into the declaring program's wordlists; today the checker keeps its own symbol table (src/core/checker.f:8323 SYM-FIND, USIGS :7535) and type registry (src/core/type-family.f:617, :703, :772), and S5b2-1 (d6b69973) switches those stores per build instead. Census 4's declaration, does> and registration stops come from the separate tables. Acceptance: a design at /tmp/jerry-big/design-s5b-facts.md (brief /tmp/jerry-big/s5b-facts-plan-prompt-r2.md) taken to Joel, one question per public item and significant change with context and pros and cons; lanes start only after. Files: design only. Verify: Joel's answers recorded in DECISIONS.md. Depends: none. Ownership: jerry. Claim: agent=jerry (Plan on Fable, read-only).
