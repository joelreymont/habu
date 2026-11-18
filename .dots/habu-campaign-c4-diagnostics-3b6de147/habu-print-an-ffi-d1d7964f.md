---
title: Print an FFI load fault as one record
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.765946+01:00"
---

Problem: ffi-abi.f FN-LOOKUP's SYMBOL-REPORT and LIB-RESOLVE's OPEN-REPORT print their own E-FFI-DLSYM / E-FFI-DLOPEN line and then throw, so a call to a missing symbol in a loaded program ends as that line plus the uncaught-throw line, rc 67, and a caught miss still prints its line. Moving the record onto REJECTION's carried form (fs-mutate.f TMPDIR-REJECT) shares REJECTION's process-wide record buffers between tasks, which are pthreads; fs-mutate.f's records already do. A memory fault (crash.f row 6, C-CRASH-THROW) has the same defect: the signal handler writes its record, with no help, even when a catch or a task's end handles the fault. Acceptance: a loaded program gets one placed record naming the symbol and library, no other line; an uncaught memory fault prints one record with its address, word and a help, and a caught one nothing; a caught fault prints nothing; a record built in one task never mixes with another's. Depends: habu-place-an-uncaught-d11d0078. Claim: unassigned.
