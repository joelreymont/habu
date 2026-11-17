---
title: "Check an FFI declaration's library in the verifier"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T23:11:57.939404+01:00"
---

Problem: a FUNCTION: declaration that reads cleanly with no library selected (/tmp/jerry-ffiord/nolib.f) is E-FFI-LIBRARY at 1:1 under hb --load but passes tools/check.f --verify-only: src/habu/verify-source.f RECORD-FFI-FUNCTION (:3586) calls FFI:READ-FUNCTION only, and the verifier does not track LIBRARY, VERSIONED-LIBRARY or PROCESS-SYMBOLS (src/lib/ffi-abi.f :1875-1890, CHECK-SCOPE :1655). Acceptance: --verify-only rejects nolib.f with the load's record; a test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ffilib.
