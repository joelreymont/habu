---
title: Compact warm snapshot code cursor
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-30T20:51:39.936361+02:00\""
blocks:
  - habu-redesign-native-gate-cf54d3f3
---

Problem: tools/warm-image-lib.f generates warm-source with WI-HIDE-MARKER then HIDE-DEFS-FROM, but HIDE-DEFS-FROM only rewinds ndict/checker metadata. src/habu/snap.f serializes SCL=cp@-dbase@, so hidden/generated compile bytes remain in the snapshot. Evidence: bin/hb is 97K, empty warm snapshot is 11M, and otool shows __TEXT filesize ~0xB34000. Fix: make warm snapshot generation mark the keeper prefix and hide/forget only the image-emitter/snap tail before SNAPGO, rewinding cp safely as well as ndict/checker state; or replace warm-image use where resident bin/hb already suffices. Acceptance: empty warm snapshot is near bin/hb size plus bounded payload, full runner cold build drops materially, focused warm-image tests prove hidden tail words absent and cp payload compact, full Mac hot/cold suite green.
