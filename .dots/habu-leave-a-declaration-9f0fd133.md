---
title: "Leave a declaration's close to the run after a deferred render"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T18:08:38.949278+01:00"
---

Problem: after the check defers a render inside an open FUNCTION: or CALLBACK: declaration (FUNCTION: GP getpid ( -- n ) s" ;FUNCTION" evaluate; likewise s" ;CALLBACK" or s" 0 FALLBACK" evaluate), the load exits 0 but the check reports W-CHECK-DEFERRED and then a false E-UNCLOSED or missing-fallback rejection: it still decides how the declaration closes. Acceptance: the check records no close verdict for a declaration a deferred render may close, and accepts these programs; a case in test/ffi-struct-verify.f holds the FUNCTION: form and one in test/ffi-callback-verify.f the CALLBACK: forms. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/carl-defclose (FUNCTION:), .jj-ws.noindex/carl-cbchk (CALLBACK:).
