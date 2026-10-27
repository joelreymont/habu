---
title: Canonicalize the temp root
status: active
priority: 1
issue-type: task
created-at: "2026-10-27T14:54:27.310789+02:00"
---

macOS /tmp is a symlink to /private/tmp. Builds record canonical paths, but temp trees made under a non-canonical TMPDIR or HB_TMP keep the symlinked prefix, so path matches fail. Evidence (carl-inst, master engine, 2026-10-27): fs-mutate-test, fs-test, runner-test, lsp-batch-test and gate-diagnostics fail with TMPDIR and HB_TMP under /tmp and pass under /private/tmp; native-file-state, types, auto, frame, slots and build-fixpoint-test fail the same way (slots and frame shown passing under /private/tmp by the KERN proof). Fix at the temp-root layer: the word that reads TMPDIR or HB_TMP returns the canonical directory (src/lib/fs-mutate.f TMPDIR-MKDIR, HB-TMP-MKDIR; other TMPDIR readers src/lib/zip.f:57, tools/build-fixpoint.f:1536, test/gate-pool.f:961 go through it). Done when every listed test passes with TMPDIR=/tmp and HB_TMP under /tmp.
