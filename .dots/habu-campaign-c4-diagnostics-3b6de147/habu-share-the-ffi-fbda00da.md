---
title: Share the FFI record helpers
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:10:10.709767+01:00"
closed-at: "2026-11-16T04:07:34.082534+01:00"
---

Problem: src/lib/ffi-callback.f (FFI-CB) holds copies of src/lib/ffi-abi.f's private record helpers (FFI-PLACE as REJECT-AT, FFI-REJECT as REJECT, LIMIT-HELP, KEPT), and the kept-span and limit-note forms are written again in src/core/generated-declaration.f TOKEN-SPAN@, src/core/rejection.f and src/habu/interpreter.f. Acceptance: one writer each in REJECTION, every copy gone, records byte-identical. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
