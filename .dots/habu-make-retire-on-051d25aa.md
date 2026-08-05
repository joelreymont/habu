---
title: Make retire-on-throw a checker capability
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.513667+02:00"
---

Review suggestion: four independent instances this round wrote registry-mutating words that clean up only on normal return (IR context teardown, migrate RUN cleanup, TFX/SVX rollback, arena builder construction). A checker rule — a word that mutates registry state must retire it on every exit path, or a with-combinator that structurally owns the retirement — makes the whole class unwritable. Per Checker-Miss RCA discipline (docs/forth.md): this is the answer to 'why didn't the checker catch CG-07/08/23?'. Design the capability, don't scatter catch frames. Reconcile with habu-linear-ownership-for-1d7e0b63 before starting.

Design input from the CG-23 fix (2026-08-05, merged 1e93253b): the interesting rule is NOT 'retire on throw' but 'ONE owner writes the watermark, and retirement is inside that write'. Both participants in the TFX bug were individually correct; the composition was not — the outer restore's retirement ran after the declaration layer's rewind, popped nothing, and stamped the index current, erasing the rewind signal. The capability must therefore also express: a later retirement against an already-restored mark is an ERROR (refuse, name the caller), because a no-op retire is an assertion of cleanliness. The shipped fix (TFAM-REWIND + refusing TFX-RETIRE/SVX-TRUNCATE guards) is the reference instance.

Second design input (2026-08-05, CG-07/08 lane): the capability is a [: body ;] [: cleanup ;] PROTECT combinator with a ROW-POLYMORPHIC body — it would replace WB-SCOPE, CE-SCOPE, and TABLES-TRY with one audited word. Its blocker is stated at elaborate.f:84-85: a checked catch takes a stack-neutral quotation, so it cannot carry the body's result row. Named consumers now: three existing scope words plus elaborate.f's declined unwind. Also learned: the capability is only REQUIRED when the body's result row must survive the catch — TABLES-TRY needed no trusted code once its inputs were parked first.
