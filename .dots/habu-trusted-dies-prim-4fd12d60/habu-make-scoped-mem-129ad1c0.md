---
title: Make scoped memory and context cleanup checked
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-10T18:03:13.354934+03:00\""
---

Owner: Cedar; paused Astra work in .jj-ws/cedar-cleanup, change svtnlsoo revision e08e925102afd92bbf5374d02f7048671bcca714. Existing catch joins identical input/output rows and cannot express a body R -> S followed by cleanup; WB-SCOPE/CE-SCOPE currently hide this with untyped parked quotations. A minimal finally engine operation is partially written in checker.f (+33/-2) and habu1.f (+16), untested: execute body, run a stack-neutral cleanup on normal return or throw, then rethrow; cleanup failure supersedes body failure. Finish the smallest justified engine model/native dispatch and tests before replacing lib/memory.f and src/compiler/ir/context.f scopes with checked code and typed resource frames. No new handler format or general IR/backend framework. Acceptance: zero/one/multiple results, preserved prefix, nesting, exact body/cleanup throw codes, invalid cleanup rejection, and actual release on normal/throw paths. Old scopes are unchanged. Existing related dot habu-make-retire-on-051d25aa. Cold seed needs only the new engine operation before loading migrated scopes; private cedar-crossing keeps old scopes for minting it.

Checked finally implementation7d277071 passed native rebuild, finally/memory/context/generic-call/register tests and independent Astra review. Updated test integration is9ed31909 plus pending abs-specific dstack assertion. Integrate into strict source and run combined suite. Capture during an active MEM scope must honor quiescent resource capture; do not retain scope pointers in an image. Separate fetched-SUM defect is reduced to typed/static or dynamic buffer @ of a sum with a nominal CAD-NUM payload: checker accepts but LOWER-CERT:GUARD-PUSH rejects empty domain before native lowering. /tmp/cedar-sum-fetch-guard.f; read parent term/domain across SCHEMA-TERM in QUEUE-VARIANT. Integer-only payloads pass. Keep validation intact and fix its producer.
