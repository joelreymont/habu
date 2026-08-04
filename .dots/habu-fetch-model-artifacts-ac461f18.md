---
title: Fetch model artifacts transactionally in Habu
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:48.225537+02:00"
---

The GPT-2 model and vocabulary fetch launchers contain substantial shell/curl/hash policy and publish directly to final filenames before validation. Implement one checked Habu import transaction for externally fetched artifacts. A compatibility launcher may only exec bin/hb into that tool. Download each member into an exclusively created unique sibling temporary file, retain exact source and transport diagnostics, verify pinned size and digest, validate the complete model or vocabulary pair as a unit, sync files, publish all members through an explicit commit protocol, sync the directory, and unwind every temporary on failure. Existing final artifacts remain byte-identical until the whole transaction is valid; concurrent importers produce one coherent version. Add injected failure at each transfer/hash/validation/sync/rename boundary, mixed-old-new and concurrent writer tests, symlink attacks, retry after interruption, and exact provenance output. Reuse the safe-filesystem and fsync capabilities; if a required primitive is missing, depend on its existing dot rather than adding shell logic. Files: package-owned Habu fetch/import tool, thin launchers, GPT-2 artifact manifests and focused tests. Verify hermetic local transport fixtures, optional live fetch, Habu-only/host/dot lints, and full native gate.
