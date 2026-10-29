---
title: One C-string conversion in FFI that refuses a NUL byte
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T16:38:37.295889+02:00"
---

Problem: 30 separate words turn a Habu string (ptr u8, n), which may hold any byte, into a NUL-terminated C string (census: .out/carl-file/cstr-census.md, C1-C30). Only four refuse an embedded 0 (FS-PATHZ-INTO, COPY-Z, pg ARENA-CSTR, serial PATH-OPEN); the rest cut the string silently at the C boundary (e.g. a PROCESS:ENV value 'b\0C=x' reaches the child as 'b'). Several also copy into fixed PATH-CAP+1 buffers, and three have no length check (QUIET-READ, TAB+, ES-PATHZ). Joel 2026-10-29 (interview, NUL question): 'what's best for long term? how can we prevent this from happening in the first place? why would a habu word even have a NUL byte concept? this smells and should be confined to the FFI package, prove me wrong!' Acceptance: one conversion lives in package FFI and refuses a string holding a 0 byte; every C1-C30 site uses it and its private copier is deleted; no module outside FFI mentions NUL termination. Design: Plan (Fable) from a problem brief, then the FFI word and any public API change go to Joel. Depends: none. Ownership: carl. Claim: unassigned.
