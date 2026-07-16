---
title: Mirror checker-defer registration into stage0
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T16:17:17.951692+02:00"
---

Follow-up capability from the deferis lane (habu-mirror-defer-is-4461fe23, landed): the stage0 mirror's defer/is works on the UNCHECKED load path only (boot-prefix files before check-hook.f INSTALL, and TRUSTED: bodies). Native C-DEFER also calls C-CALL-CHECKER-DEFER to register the defer with the checker; the stage0 mirror lacks LCHKDEFER/C-FIND-GLOBAL, so in a CHECKED definition 'is NAME' on an unregistered defer is rejected by the check hook - fail-closed (rejects, never miscompiles), but it means any stage-2 hook migration that places defer/is inside checked definitions in stage0-loaded prefix files would break no-binary recovery at build time. Work: mirror the checker-defer registration bridge (habu2.f C-CALL-CHECKER-DEFER + the checker's is/defer usig rows) into bootstrap/cg/forth.fs, with a checked-path defer/is round-trip added to test/bootstrap-wide-memory-src.f (a defer used inside a checked : definition after check-hook INSTALL). Acceptance: bootstrap-wide-memory green with the checked-path fixture; bootstrap-codegen-test/boot-pin/mirror-lint ok; fixpoint x2 byte-identical; full recovery CHECK_ONLY chain OK. Only needed if habu-migrate-engine-hooks-e4f31fc6 stage 2 chooses defer shapes in checked prefix code - the migration can alternatively keep defers in unchecked-region files and skip this. Files: bootstrap/cg/forth.fs, test/bootstrap-wide-memory-src.f. Ownership: bootstrap/stage0 codegen.
