---
title: Stream a fetch response in the browser host
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T10:48:30.988342+03:00"
---

Problem: the browser host's fetch/post operations wait for complete responses. Maki's browser chat and preview cannot show updates as they arrive. Acceptance: a wasm module receives response body chunks as the host reads them, with a browser test that observes the first chunk before the response completes. Files: Habu browser host (owner decides). Verify: that test. Depends: none. Ownership: Habu browser-host owner. Caller: Maki viewer chat and preview. Claim: unassigned.
