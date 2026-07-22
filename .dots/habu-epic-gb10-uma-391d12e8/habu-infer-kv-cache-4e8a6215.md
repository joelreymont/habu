---
title: "Infer: KV-cache quantization"
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T16:45:21.609887+02:00"
blocks:
  - habu-infer-kv-quant-ccbdf8a8
---

This is the key/value-cache quantization campaign record. Do not dispatch it as implementation work. Its leaves define the quality profile, account for every byte, convert appends atomically, read compressed pages directly in attention, measure long-context quality, and make the release performance decision. The campaign closes with a measured supported-profile verdict.
