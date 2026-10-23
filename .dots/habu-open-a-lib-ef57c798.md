---
title: Open a library globally or relative to the executable
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T10:48:30.938884+03:00"
---

Problem: FFI:DLOPEN takes only FFI:NOW, and LIBRARY takes one literal path. Maki's kernel worker opens its OCCT façade at run time and resolves the façade's functions through PROCESS-SYMBOLS (RTLD_DEFAULT); that works on macOS only because an opened library is global there. On Linux the symbols stay local. Acceptance: a library can be opened with RTLD_GLOBAL (e.g. FFI:GLOBAL), or LIBRARY can name a path relative to the executable, with a Linux test that resolves an opened library's function through the process-wide lookup. Files: Habu FFI (owner decides). Verify: that test on linux-aarch64 and macOS. Depends: none. Ownership: Habu FFI owner. Caller: Maki kernel worker on Linux hosts, deferred until Maki's Linux work starts (Joel 2026-10-07). Claim: unassigned.
