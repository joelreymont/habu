---
title: "Carry a library failure's detail with its throw"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:55.521738+01:00"
---

Problem: ffi-abi.f:330-473 writes a whole record at the cause and prints even when the program catches; E-FFI-DLSYM and E-FFI-DLOPEN (error.f:161,166) have no ERROR:CODE row, so the uncaught head reads E-THROW. Design (approved, Joel: 'Message plus help'): REASON moves into the engine as a per-thread band, paired with its throw by the thread's throw count; a library throws its detail through REASON as a message and its own help line and prints nothing at the cause; the two FFI codes get ERROR:CODE rows; THROWN-TEXT shows the REASON text. Maki's six REASON:THROW calls (drawing.f:4881,5373,7531,7861,8706, ops.f:7827) change with it (Maki's agent). Acceptance: a caught missing symbol or library prints nothing; uncaught, one record headed by its code with the library, the paths tried, the loader's reason and its help, placed at the program's call. Depends: none. Claim: unassigned.
