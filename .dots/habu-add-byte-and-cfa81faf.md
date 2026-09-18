---
title: Add byte and inline-blob fields to layout families
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T10:24:05.110170+03:00"
---

Problem: a layout family's fields are cell slots; lib/task.f's TCB carries inline semaphore byte blobs and reaches its five pointer fields (STACK, REGION, RSTACK, LSTACK, MSG-SENDER) through PTR-FIELD:, lib/signal.f SA-ACT is a foreign struct sigaction with a code address at offset 0 and flags at SA-FLAGS-OFF, and src/arch/tic6x/ uses CFIELD: byte fields; none can be declared until a byte-extent field kind exists (TYPE-FIELD already records BYTE-OFF@, BYTES@ and ALIGN@) or a nested family stands for the blob, and BEGIN-STRUCTURE cannot be retired before them. Acceptance: a byte-extent field kind or a nested-blob family with its schema, projection and generator support; CFIELD:'s users converted; TCB declared with its five pointer fields and its semaphore blobs; SA-ACT declared as an asserted foreign layout with a test; BEGIN-STRUCTURE and PTR-FIELD: retired; fixpoint; test/run.f. Files: src/core/type-family.f, src/core/type-schema.f, src/core/structures.f, lib/task.f, lib/signal.f, src/arch/tic6x/*.f. Verify: the family suites; fixpoint; test/run.f. Depends: habu-generate-typed-field-ba63866e. Ownership: type system. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
