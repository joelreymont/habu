---
title: Report an FFI load failure as a record
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T22:00:35.054114+01:00"
---

Problem: src/lib/ffi-abi.f SYMBOL-REPORT, OPEN-REPORT with REPORT-NAMES, and REPORT-LIBRARY-TARGET write lines such as 'E-FFI-DLSYM: crc32' straight to stderr through REPORT-LINE: an E- name with no statement of the issue and no help, against the Rust-style diagnostics ruling campaign 3b6de147 quotes, and outside the record channel, so no capture can hold them. ffi-cabi's Linux FFI-T-GLOBAL case (src/lib/ffi-test.f:395-396) provokes the miss on purpose and leaks the line on the test's stderr. Acceptance: each failure prints one diagnostic in the record form of src/core/rejection.f (a header error[E-...] saying what failed and naming the library or symbol, a note per library tried with the loader's reason, a help) in one write and with no process-wide buffer, since tasks fail at once and rejection.f builds its record in process-wide TEXT; a foreign spelling written after LIBRARY is rejected with a record placed at the path; each still throws its code; ffi-cabi asserts each report it provokes from a child's stderr, and its own stderr is empty. Depends: none. Claim: unassigned.
