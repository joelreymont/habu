---
title: "Infer prefill: bounded chunk driver"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.738844+02:00"
blocks:
  - habu-infer-prefill-write-5147bac5
---

Why this exists:
long prompts need chunked execution over the same kernel and KV state, with no hidden chunk policy.

Required result:
process an explicit chunk size, preserve model positions across chunks, and commit each completed chunk transactionally.

Done when:
chunked and unchunked outputs/KV are equivalent; failure in any chunk leaves prior committed chunks valid and cancels the active request cleanly.

Expected touch points: new maki/infer/chunked-prefill.f, focused test.
Smallest check: focused chunk equivalence and failure test.
Prerequisites: write shared KV layout.
Owned result: chunk execution driver only.
Claim: unassigned.
