---
title: Re-arm curl image cleanup after each capture
status: open
priority: 2
issue-type: task
created-at: "2026-09-20T02:02:45.053381+03:00"
---

lib/net/curl.f FORGET-GLOBAL clears GLOBAL-DONE but not GLOBAL-REGISTERED; IMAGE-LIFECYCLE:PREPARE consumes the hook, so subsequent GLOBAL-READY skips cleanup registration and later images can retain initialized state from another process. Source finding from FFI lifecycle reduction (9c283543). Claim: alder, .jj-ws/alder-lifecycle-followups; Hazel says fix after the current stripped-lifecycle/FFI/deadline stack. Measure repeated initialization/prepare cycles, reset the registration at its owner, and prove restored images initialize correctly. Keep scope to this lifecycle defect and its fixture.
