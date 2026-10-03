---
title: Refuse a false trust signature length
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T05:37:50.632585+02:00"
---

Problem: trust (src/core/checker.f, near TRUST :14249) with a maximum-cell signature length hangs in the signature parser (rc 124 under timeout 20), and with -1 dies rc 76 'checker: bad stored signature' after printing the signature text it was given, reading bytes the length does not describe. Measured by the r4-wrap2 lane ($HOME/.cache/tmp/kestrel-r4-wrap2/trust-sigmax.f). Acceptance: a signature length that cannot describe memory (negative, or an end that wraps) is refused with the trust layer's own error before any byte is read or printed, both spellings exit promptly with that refusal, and true signatures are unchanged; cases through test/trust-row-test.f written before the code. Files: src/core/checker.f, test/trust-row-test.f. Verify: test/trust-row-test.f, native build convergence. Depends: habu-refuse-false-lengths-cf675f11. Ownership: trust signature length guard.
