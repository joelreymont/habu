---
title: Package AOT capture
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:28:57.582615+02:00"
---

src/habu/aot-capture.f:23-315 and :549-621 exposes 61 raw AOT-/ACAP-* globals. Trusted representation casts AOT-DBASE, AOT-A>U8, and AOT-N>U8 are globally callable; dictionary scanners, relocation/name buffers, serializer state, reset/report helpers, and build-only self-test machinery are also public. Only owner validation is packaged in the nested AOT-OWNER block, while stdin.f calls just ACAP-CAPTURE and ACAP-BOOTRUN+. Wrap the full module in package AOT-CAPTURE, retain the nested owner boundary, export only CAPTURE and BOOTRUN+, and keep casts/storage/scanners/serializers/tests private. Coordinate habu-move-aot-regressions-22c1ee71 so the inline self-tests move out rather than becoming private production payload. Preserve owner-WID validation, compact record inverse, relocation/name/protected-WID data, capture ordering, bootstrap behavior, and exact captured blob bytes. Add old-global and qualified-cast/private rejects, public entry positives, corruption/truncation/capacity cases, and exact capture tables. Measure dictionary-name bytes, JIT/DATA/CODELEN, captured size, and capture latency before/after. Verify capture/AOT positive/negative/bootstrap/fixpoint/snapshot gates, package/host/filemap/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c.
