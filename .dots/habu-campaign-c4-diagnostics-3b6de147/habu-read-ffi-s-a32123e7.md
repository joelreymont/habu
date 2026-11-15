---
title: "Read FFI's open declaration from FFI in the verifier"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T10:47:44.963182+01:00"
closed-at: "2026-11-16T04:07:34.837774+01:00"
---

Problem: the master merge into C4 keeps C4's unclosed-FUNCTION: rejection in src/habu/verify-source.f through FFI-OPEN-A/FFI-OPEN-U (FFI-UNCLOSED, RECORD-FFI-FUNCTION), a second copy of the open-declaration state src/lib/ffi-abi.f already keeps privately (its OPEN cell and SOURCE-ROOT:OPENED); the two can disagree. Acceptance: the verifier's scan reads whether a FUNCTION: group is open, and its opening token, from one owner; FFI-OPEN-A/FFI-OPEN-U are gone; a FUNCTION: left open at a source's end or before another FUNCTION: is rejected once, at its opener, under the three readers. Depends: the master merge into C4. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
