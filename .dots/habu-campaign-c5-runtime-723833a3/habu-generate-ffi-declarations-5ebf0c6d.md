---
title: Generate FFI declarations from C headers
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:10:29.927873+03:00"
---

Problem: every foreign library (libcurl, libpq, OpenSSL, libc) needs dozens of typed FUNCTION: declarations, and VFX Forth's sqlite3h.fth shows that hand-transcribing a header is mechanical, large and error-prone. Acceptance: a checked Habu tool that reads a C header subset (function prototypes with scalar, pointer and const char* parameters, typedefs to scalars, enums, #define integer constants) and emits a Habu source file of FUNCTION: declarations and constants for a named library, deterministic across runs; libpq-fe.h and curl/curl.h are the first two inputs and their outputs are what lib/db/pq.f and lib/net/curl.f require; unsupported constructs are reported by line, never silently skipped. Files: tools/cbind.f (new), tools/cbind-test.f, docs/cbind.md. Verify: the tool's tests on fixture headers; the two real headers regenerate byte-identically. Depends: the FFI declarer. Ownership: tools/cbind.f. Claim: unassigned.
