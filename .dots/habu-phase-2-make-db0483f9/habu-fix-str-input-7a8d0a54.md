---
title: Fix string input stream slicing
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.206095+02:00\""
closed-at: "2026-04-02T17:51:32.507577+02:00"
close-reason: done (zig build; zig build test back to known 5-error baseline)
blocks:
  - habu-add-maxima-reader-192066c9
---

Problem: make-string-input-stream start or end semantics are wrong. Acceptance: CL slicing semantics hold for Maxima parser and batch callers. Files: src/runtime/primitives/io.zig:1687-1725. Verify: focused stream slicing regressions and Maxima parser probes. Blockers: habu-add-maxima-reader-192066c9.
