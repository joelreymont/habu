---
title: Collapse dictionary checker bottleneck
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:33.466331+02:00\""
close-reason: "completed locally: dictionary/checker phase is 4997ms on the hot macos-arm64-12x2 proof and 4966ms on cache-fill; negative diagnostic assertions remain in the suite; focused lints and full Mac hot/cold proofs pass"
---

Problem: Mac hot test suite reports native dictionary/checker gate phase around 8.7-9s. It still runs many checker negatives through child hb/stdin paths. Fix: use direct checker/all-errors APIs for pure negatives, batch remaining fail-closed CLI sentinels, and keep process boundaries only where public CLI behavior is the subject. Acceptance: dictionary/checker phase under 5s on Mac hot profile, negative diagnostics still asserted, fail-closed CLI sentinel retained.
