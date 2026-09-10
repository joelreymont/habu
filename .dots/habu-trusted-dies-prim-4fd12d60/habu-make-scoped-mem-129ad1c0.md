---
title: Make scoped memory and context cleanup checked
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.354934+03:00"
---

Owner: Cedar; paused Astra work in .jj-ws/cedar-cleanup, change svtnlsoo revision e08e925102afd92bbf5374d02f7048671bcca714. Existing catch joins identical input/output rows and cannot express a body R -> S followed by cleanup; WB-SCOPE/CE-SCOPE currently hide this with untyped parked quotations. A minimal finally engine operation is partially written in checker.f (+33/-2) and habu1.f (+16), untested: execute body, run a stack-neutral cleanup on normal return or throw, then rethrow; cleanup failure supersedes body failure. Finish the smallest justified engine model/native dispatch and tests before replacing lib/memory.f and src/compiler/ir/context.f scopes with checked code and typed resource frames. No new handler format or general IR/backend framework. Acceptance: zero/one/multiple results, preserved prefix, nesting, exact body/cleanup throw codes, invalid cleanup rejection, and actual release on normal/throw paths. Old scopes are unchanged. Existing related dot habu-make-retire-on-051d25aa. Cold seed needs only the new engine operation before loading migrated scopes; private cedar-crossing keeps old scopes for minting it.
