---
title: Add the LOG library and STDERR capture helper
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T19:07:06.311129+02:00"
---

Logging lane 3. Brief: carl-dots/.out/carl-file/brief-log3-log.md. Acceptance: src/lib/log.f (LOG:LINE/TEXT+/INT+/;LINE, ERROR, WARN), src/lib/test/stderr.f (STDERR:CAPTURE/;CAPTURE, E-NESTED), ws-test on it, log-test registered and green. Files: src/lib/log.f src/lib/log-test.f src/lib/test/stderr.f src/lib/net/ws-test.f src/lib/errors.f test/gate-stdlib-cases.f. Verify: the brief's proof. Depends: habu-add-fmt-int-a95323c3. Ownership: carl. Claim: agent=worker workspace=.jj-ws.noindex/carl-logl
