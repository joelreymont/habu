---
title: Typed spans and nominal class ids for loss APIs
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T23:24:28.747105+02:00"
---

Deferred capabilities from habu-validate-cross-entropy-4b176d46 (its lane could not change public signatures): (1) typed logit/target span parameters for the loss-tensor API so undersized-buffer rejection is expressible (today only no-over-write canaries are possible on the untyped ptr surface); (2) a checker-enforced nominal class-id type replacing the runtime target-validation boundary in XENT-TGT; (3) promote MAKI:MUL-DIM/NONNEG to public so LOSS and siblings reuse the canonical overflow-safe dimension law instead of local DIM-CEIL copies. Also correct the closed dot habu-cross-entropy-loss-93356943's close-reason overclaim (GC-RUN proves the LINEAR adjoint, not the composed CE gradient - that proof now lives in xent-train-test.f).
