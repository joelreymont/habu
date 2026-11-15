---
title: Print an FFI load fault as one record
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.765946+01:00"
---

Problem: ffi-abi.f FN-LOOKUP's SYMBOL-REPORT and LIB-RESOLVE's OPEN-REPORT print their own E-FFI-DLSYM / E-FFI-DLOPEN line and then throw, so a call to a missing symbol in a loaded program ends as that line plus the uncaught-throw line, rc 67. Acceptance: a loaded program gets one placed record naming the symbol and library, no other line. Depends: habu-place-an-uncaught-d11d0078. Claim: unassigned.
