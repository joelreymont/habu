---
title: Refuse negative and lying lengths at lib guards that add nothing
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T17:34:37.501709+02:00\""
---

Problem: raw role casts are not validators (docs/forth.md "Raw role casts are not validators"), so a public lib word can be handed a negative length, and some guards only compare the length with the room left, which a negative passes. Seen while auditing the wrapping guards (change kzxssvzv), not audited: lib/string.f SB-APPEND-LEN moves SB-LEN backwards for a negative len (SB-APPEND is safe because STR-LEN refuses negatives); lib/json-write.f JW-ROOM relies on JW-SPAN upstream; lib/pty-harness.f:356 and :486, lib/unicode.f:155 and lib/ffi-abi.f NAME-ROOM / PATH-ROOM let a negative length reach BYTE-COPY, which dies rc 76 instead of throwing. A second shape in the same words: a scan of the caller bytes runs before the capacity guard, so a length larger than the buffer is read past its end before it is refused (lib/process-env.f PROC-ENV-CHECK-NAME and PROC-ENV-HAS-EQUAL?, lib/base64.f CHECKED). Acceptance: every public lib word that takes a caller-supplied length and reaches a copy, a fill or a cursor move is listed with whether a negative length is refused before the effect; each reachable one refuses it with its existing named error; where a word scans caller bytes, the capacity bound comes first so the scan is bounded by the capacity; tests (-1, the minimum cell, one past capacity) are written first through the public word. Files: lib/*.f and their tests. Verify: the owning suites. Depends: habu-refuse-lengths-that-5d2bf9c7. Ownership: those guards; not src/. Claim: agent=kestrel workspace=.jj-ws/r4-neg.
