---
title: "Check the source loader's entries"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:08:03.765888+02:00"
closed-at: "2026-10-31T14:16:13.217989+02:00"
close-reason: "Landed 56694101: include.f 7 -> 0 and main.f 3 -> 0 TRUSTED:, EVALUATE-RANGE-RAW dropped for evaluate-range. main.f is only in the x86-64 engine: whitebox check.f on main.f rc 0, main-argv 34 cases. Full suite 510/510 and x86-64 cross-build rc 0 on the batch stack."
---

Problem: include.f's 7 and main.f's 3 TRUSTED: words build as : (census tc2); EVALUATE-RANGE-RAW is a wrapper around evaluate-range. Acceptance: the 10 are : (EVALUATE-RANGE-RAW dropped, callers call evaluate-range), engine build passes, loader rows pass. Files: src/core/include.f, src/habu/main.f, src/habu/interpret.f (comment). Verify: engine build; rows source-root, source-root-exe, include-refusal, load-reject-diag, load-argv-contract, source-unit-range, app-image. Depends: none. Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tc2
