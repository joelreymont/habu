---
title: Remove task ASCII tables
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:12:35.449049+02:00"
blocks:
  - habu-add-readable-static-e267f68e
---

lib/task.f:61-90 hand-encodes ten human strings as decimal byte-by-byte create arrays: two library paths and eight pthread/munmap symbol names. The arrays reserve exactly 156 DATA bytes before dictionary headers; the longest string needs 27 bytes including NUL. lib/task-test.f:51-56 duplicates the same pattern for strlen and both library paths. This is unreadable generated-looking source, makes spelling review depend on manually decoding ASCII, and permanently owns ten production dictionary/data objects although every dlopen/dlsym call consumes its name synchronously during module load. After the common checked static C-string declaration lands, migrate the task library paths and pthread/munmap symbols to that surface. Do not create a task-owned scratch conversion, compatibility array, or second C-string mechanism. Preserve exact library/symbol spellings, macOS/Linux selection, eager symbol resolution, errors, FFI behavior, pointer lifetime, and terminators. Measure source tokens/lines, dictionary entries, DATA bytes, load JIT bytes, and module load time before/after; the end state must have zero production hand-coded ASCII arrays and lower resident size. Add exact C-string byte assertions and the shared capability's embedded-NUL, over-capacity, and lifetime regressions. Files: lib/task.f, task-test.f. Ownership: task FFI-name migration only; no lifecycle, FFI ABI, or general binary fixture rewrite.
