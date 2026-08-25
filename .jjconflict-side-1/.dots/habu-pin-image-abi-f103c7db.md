---
title: Pin image-ABI constants across DDC chains
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:04.491842+02:00"
---

Depth review: bootstrap/cg/forth.fs:21-25 independently re-hardcodes REGION, RBASE-VA, SNAP-MAGIC, SNAP-FORMAT-VERSION vs native src/habu/layout.f:8-10 + per-OS layout.f. Shared ABI facts with no compile-time cross-check: a format bump in one chain corrupts the other silently. Add ~30-line gate assertion comparing both chains' ABI constants. Preserves DDC independence.
