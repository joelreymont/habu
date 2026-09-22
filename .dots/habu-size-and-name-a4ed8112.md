---
title: Size and name the FFI library table refusal
status: active
priority: 3
issue-type: task
created-at: "2026-09-17T06:05:37.459382+03:00"
---

Problem: package FFI keeps LIB-MAX ($08) dlopened library paths and refuses the ninth with E-FFI-ARITY naming nothing, the same shape the declaration table had before habu-size-the-ffi-fca5a67f; five bindings use three paths today, so it is not binding yet. Acceptance: LIB-MAX becomes a named published ceiling with its cost in the header (FFI:LIBRARY-MAX), a full table is refused with its own named FFI code whose diagnostic names the library path and the Habu word being declared, and a child-engine test in lib/ffi-test.f declares past it and asserts the code and the path in stderr. Files: lib/ffi-abi.f, lib/ffi-test.f, lib/errors.f, docs/stdlib.md. Verify: lib/ffi-test.f, engine-suite, error-code-lint on a rebuilt engine. Depends: habu-size-the-ffi-fca5a67f. Ownership: lib/ffi-abi.f. Claim: Alder, workspace .jj-ws/alder-ffi-library-cap, based on hazel/integration 75c797be.
