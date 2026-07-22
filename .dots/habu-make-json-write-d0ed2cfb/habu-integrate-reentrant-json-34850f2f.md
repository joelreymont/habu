---
title: Integrate reentrant JSON writer cutover
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.738654+02:00"
blocks:
  - habu-build-explicit-json-399f5929
  - habu-migrate-ptx-manifest-3cf77235
  - habu-migrate-build-report-9e7244d3
  - habu-migrate-diagnostic-json-ebf17ff0
  - habu-migrate-json-writer-c4063806
---

Why: provider and consumer commits must land as one coherent cutover; the explicit provider deliberately breaks the singleton surface and must never reach master without every caller migrated. Exact result: assemble the reviewed provider and four consumer migrations, resolve only semantic integration conflicts, delete every old module-global writer field and raw JSON-WRITE:$ surface, and update the parent contract/FILEMAP consistently. Acceptance: structural search finds zero old singleton calls or definitions; every writer has exclusive caller state and closes once; all focused child checks pass on the combined tree; host-lint, filemap-lint, native dot gate, Maki, PTX/native touched slices, fixpoint, and performance gate are green before master advances. Smallest check: the combined focused child suite list before the batched master gate. Depends: Build explicit JSON writer core; Migrate PTX manifest JSON writer; Migrate build-report JSON writer; Migrate diagnostic JSON writer; Migrate JSON writer test consumers. Ownership: integration conflicts and parent metadata only; no new behavior. Claim: unassigned.
