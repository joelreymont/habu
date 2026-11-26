---
title: Charge a C sample above the code to its caller
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T12:23:11.106372+01:00"
---

Problem: the profiler sends any pc at or above ARN-HI to the deferred samples (src/habu/prof.f:810-811), which a sync files under "new" when no record owns it (:1287): it takes "above the dictionary's code" for "compiled after the build" (src/habu/prof-abi.f:60). On Linux libc, the vDSO and ld.so lie above it, so a C sample whose x20 and x26 survive (pthread_sigmask, the vDSO's clock_gettime) lands in "new" instead of being charged to its Habu caller; test/prof-owner.f's child reports new 292 of 314. Acceptance: a pc outside Habu's code ranges is foreign and charged as debugging.md says, on both arches; "new" holds only code compiled after the build; a test fails before the fix. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-pnew.
