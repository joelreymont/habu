---
title: Dispatch DO-TOK1 through one hashed keyword lookup
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T19:18:06.305560+03:00"
---

Problem: src/core/checker.f DO-TOK1 asks about ninety spelling questions per token (CORE-STR= against literal keywords, 19-deep IF ladder), 4.6 percent exclusive and 11.1 percent inclusive of tier-0 compile time on the corpus (tools/tier0-profile.f, jit-path lane, 2026-09-16), after the store-guard hoist made it the next largest checker cost. Acceptance: the folded token is looked up once in a shared keyword table (hash or perfect index built at load from the same keyword list the rest of the checker uses), DO-TOK1 dispatches on the resulting id (a case or a table, no ladder, which also closes the docs/forth.md rule the audit cited), every keyword keeps its exact spelling and case-insensitivity, the checker suites and test/run.f green, tier0-profile corpus and trivial numbers before and after. Files: src/core/checker.f, test/. Verify: tools/tier0-profile.f; the checker suites; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: checker tokenizer. Claim: unassigned.
