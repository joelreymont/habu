---
title: Isolate bootstrap fixtures under a private root
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:48.221039+02:00"
---

test/nf.fs and bootstrap-wide-memory currently write fixed global /tmp/nf-bin, /tmp/nf-out, /tmp/nf-src, and /tmp/nf-repl paths. Thread one validated private HB_TMP root through the complete no-binary bootstrap fixture and derive every executable, source, output, and REPL path inside it. Create the root and files exclusively, bind every artifact to the producing child, and clean it through an ownership scope on success and failure. Parallel fixtures must never observe, replace, execute, or remove another run's artifacts. Add two-process collision tests, hostile preexisting file/symlink cases, interrupted child, cleanup failure, path-length boundaries, and proof that every created path is beneath the validated root. A PID suffix at individual call sites is not sufficient. Files: checked Habu bootstrap fixture owners and tests; do not add host logic or extend tools/bootstrap.sh. Verify wide-memory/no-binary recovery fixtures, parallel native gate, host/filemap/dot lints, and full native gate.
