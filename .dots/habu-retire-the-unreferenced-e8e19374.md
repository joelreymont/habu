---
title: Retire the unreferenced FFI symbol error codes
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T19:53:21.865204+03:00"
---

Problem: after the FUNCTION: declarer (b7e89592) resolves symbols at first call with E-FFI-DLSYM, lib/errors.f still carries E-UDP4-SYMBOL (-9102) and E-TCP4-SYMBOL (-9182), which no code references (aspen, 2026-09-16, after landing udp4 499a96ac and tcp4 6a3fe29b). Acceptance: both constants removed from lib/errors.f with their reservation comments adjusted, tools/error-code-lint.f clean, docs/udp4.md and docs/tcp4.md name E-FFI-DLSYM where they described the retired codes, no other reference in the tree (rg). Files: lib/errors.f, docs/udp4.md, docs/tcp4.md. Verify: tools/error-code-lint.f; lib/net/udp4-test.f and tcp4-test.f. Depends: none. Ownership: error table. Claim: unassigned. E-CURL-SYMBOL (-9231) joins them: lib/net/curl.f (171dfcc7, 2026-09-16) binds through FUNCTION: and never references it; docs/curl.md already names E-FFI-DLSYM as the missing-symbol error.
