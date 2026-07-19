---
title: Remove task ASCII tables
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:12:35.449049+02:00"
---

lib/task.f:61-90 hand-encodes ten human strings as decimal byte-by-byte create arrays: two library paths and eight pthread/munmap symbol names. The arrays reserve exactly 156 DATA bytes before dictionary headers; the longest string needs 27 bytes including NUL. lib/task-test.f:51-56 duplicates the same pattern for strlen and both library paths. This is unreadable generated-looking source, makes spelling review depend on manually decoding ASCII, and permanently owns ten production dictionary/data objects although every dlopen/dlsym call consumes its name synchronously during module load. Replace the arrays with readable counted s" ..." literals passed through one package-owned bounded C-string scratch conversion at TASK-OPEN/TASK-SYM; reject embedded NUL/over-capacity before FFI, and keep the scratch private. Do the same in the test instead of duplicating encoded paths. Preserve exact library/symbol spellings, macOS/Linux selection, eager symbol resolution, errors, and FFI behavior. Measure source tokens/lines, dictionary entries, DATA bytes, load JIT bytes, and module load time before/after; the end state must have zero production hand-coded ASCII arrays and lower resident size. Add exact C-string byte assertions and injected over-capacity/NUL failures. Files: lib/task.f, task-test.f. Depends: none. Ownership: task FFI-name representation only; no lifecycle, FFI ABI, or general binary fixture rewrite.
