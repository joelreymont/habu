---
title: Strip essay comments from the compiler chain
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-15T19:21:13.896795+02:00\""
---

User ruling 2026-08-15: no essay comments in source. Scope: src/compiler/native/*.f + src/arch/arm64/asm.f (54% comment/blank measured, 21246 of 39512 lines). Keep: stack effect signatures (the managed contract), lint pragmas (typed-local-lint/schedule-lint allow-* lines, TRUST rows), constraint comments <=2 lines stating what the code cannot. Delete: design essays, narrated rationale, refutation histories, file-header essays (keep a 1-3 line header saying what the file is). Design history moves to dots/LESSONS if not already there. Code tokens untouched. Gates: codegen-compare 0 findings sizes identical, judge --check 46-row agreement, full test/run.f idle, aot suites (chain capture census recs/sites/blob must be IDENTICAL - comments compile to nothing; chaindigest changes, it hashes source bytes, that is derived not checked), install fixpoint x2 with engine sha unchanged (these files are not in the engine build). If any source-shape test counts tokens in these files, stop and report rather than adjusting the test. Follow-up dots for other directories after this proves the method. Claim: agent=comment-sweep workspace=.jj-ws/habu-comment-sweep
