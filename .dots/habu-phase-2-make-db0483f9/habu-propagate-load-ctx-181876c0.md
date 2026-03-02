---
title: Propagate load context through batch streams
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.242521+02:00"
blocks:
  - habu-canonize-file-create-72f00d29
---

Problem: batch-stream and batchload-stream lose truthful load pathname and truename context. Acceptance: batch and test-batch preserve load context through file-backed streams. Files: ../maxima/src/mload.lisp:50-73,172-205, src/runtime/primitives/io.zig, loader special bindings. Verify: batch/test-batch probes on file-backed streams. Blockers: habu-canonize-file-create-72f00d29.
