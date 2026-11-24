---
title: "Leave a CALLBACK: group's close to the run"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T21:00:07.090324+01:00"
---

Problem: src/habu/verify-source.f DEFER-DECL leaves only FFI's FUNCTION: declaration to the run; the check's callback-group (:3432) has no left state, so a CALLBACK: closed by rendered text (CALLBACK: CBX ( -- ) s" ;CALLBACK" evaluate) is rejected E-UNCLOSED by tools/check.f while the run accepts it. Acceptance: test/ffi-struct-verify.f's CALLBACK: twins of FFI-SV-RENDERED are deferred with no E-UNCLOSED, and a callback word closed by a literal ;CALLBACK after a deferral is defined. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cbleft.
