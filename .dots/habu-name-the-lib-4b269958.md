---
title: Name the library a failed dlopen could not open
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T17:23:15.233879+02:00"
---

Problem: src/lib/ffi-abi.f LIB-RESOLVE (around line 432) throws E-FFI-DLSYM (-3502, missing symbol) when DLOPEN-RAW returns 0, and the message names neither the library path nor the loader's reason, so a missing shared library reads as a missing symbol with no name. Reported by phil: on spark, Zen test/opc.f with libzip.so.5 off LD_LIBRARY_PATH prints only -3502. Joel 2026-10-31 named the error E-FFI-DLOPEN (-3505, free in src/lib/errors.f). Acceptance: a failed dlopen throws E-FFI-DLOPEN after writing an error line (ERROR-TEXT:TYPE) naming the path it tried and the loader's reason; a missing symbol in an opened library still reports E-FFI-DLSYM naming the symbol; a test through the real load path declares a FUNCTION: in a library that does not exist and asserts the path in the output. Files: src/lib/ffi-abi.f, src/lib/errors.f, the FFI e2e test the worker names. Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-dlerr.
