---
title: "Declare CALLBACK: to the checker"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T16:41:43.636824+02:00"
---

Problem: src/lib/ffi-callback.f:522 CALLBACK: has no row or intrinsic, so the source check stops at the first callback declaration (src/lib/net/tls.f:158) and checks nothing after it; a parses row alone leaves NAME, NAME-BODY, NAME-ENTRY and NAME-DISPATCH undefined to the check. Acceptance: check-all-errors on a source that requires src/lib/net/tls.f reports an error placed after it; a callback declaration's four words check as the run defines them. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/bob-callback.
