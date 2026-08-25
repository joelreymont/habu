---
title: Wire fetch.f into standalone-load coverage
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T15:30:44.203608+02:00"
---

Why: maki/examples/nanogpt/fetch.f (checked GPT2-FETCH engine replacing the retired bash fetchers) proves standalone-loadability only manually; tools/standalone-load-test.f owns that regression class but has no row for it, so a future require-graph break surfaces only when someone runs the live fetcher. Owned result: one SL-LOADS row for maki/examples/nanogpt/fetch.f in tools/standalone-load-test.f. Acceptance: standalone-load-test green on the tree with the row; red when fetch.f's require list is broken in a fixture copy. Owning gate: bin/hb --load tools/standalone-load-test.f. Depends: lands after habu-replace-nanogpt-fetch-5f905f97 merges. Files: tools/standalone-load-test.f only.
