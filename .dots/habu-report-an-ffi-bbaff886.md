---
title: Report an FFI load failure as a record
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T22:00:35.054114+01:00"
---

Problem: src/lib/ffi-abi.f SYMBOL-REPORT, OPEN-REPORT with REPORT-NAMES, and REPORT-LIBRARY-TARGET write lines such as 'E-FFI-DLSYM: crc32' straight to stderr through REPORT-LINE: an E- name with no statement of the issue and no help, against the Rust-style diagnostics ruling campaign 3b6de147 quotes, and outside the record channel, so no capture can hold them. ffi-cabi's Linux FFI-T-GLOBAL case (src/lib/ffi-test.f:395-396) provokes the miss on purpose and leaks the line on the test's stderr. Acceptance: each failure prints one record through src/core/rejection.f saying what failed, the library or symbol, the loader's reason and a help, JSON only under --json-errors, and still throws its code; ffi-cabi captures and asserts the records it provokes and its stderr is empty. Depends: none. Claim: unassigned.
