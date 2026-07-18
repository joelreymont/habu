---
title: "lib/render.f+report.f: dead chain or parameterize into fmt (~200 lines)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:40.506526+02:00"
---

Depth review: render.f is a second string-builder (admits sharing fmt.f SB-U digit recursion) + third JSON-ish surface (QSTR/QK duplicating json-write role), fixed 16KiB buffer; sole consumer report.f (CSV/MD emitter) loaded by nothing but its test. CAVEAT: built for byte-exact parity with an out-of-repo Zig analyzer — if that app is dead, delete; if alive, parameterize fmt.f SB over caller buffer (hashmap.f caller-owned pattern) and delete the duplicate.
