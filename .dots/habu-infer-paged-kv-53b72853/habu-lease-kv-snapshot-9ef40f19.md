---
title: Lease KV snapshot publication
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T23:32:48.534830+02:00"
blocks:
  - habu-infer-kv-snapshot-1cdc055a
---

This is the campaign record for safe KV snapshot publication. Do not dispatch it as implementation work. The immutable snapshot-storage leaf owns the bytes and page pins; the publication-handshake leaf owns release, launch authority, device acknowledgement, and retirement. This campaign closes only after both leaves land and the paged-decode integration proves that cancellation cannot recycle pages or buffers still named by an in-flight launch.
