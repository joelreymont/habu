---
title: Compile tiny words into their callers
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T16:32:56.729373+01:00"
---

Problem: on a compile about 24% of calls go to words of a few literals and one-cell operations (TAG, RES-FALSE, PAY, ISVAR); a call and its return cost x86 0.6 ns and arm64 about none. Acceptance: T0 keeps such a word's steps beside its code (record flag DNAME-COPY, a field of its saved entry, at most COPY-MAX steps) and lowers them at each mention in the caller's registers; a changed word recompiles its callers, warm and cold builds are byte-identical, a breakpoint on one says it is compiled into its callers, the load is faster on x86 and no slower on arm64, and neither engine nor Zen, Tender or Maki grows. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-tiny.
