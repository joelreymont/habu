---
title: Emit JSON strings in runs
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T20:33:34.985044+03:00"
---

Measured Otto profile /Users/joel/.cache/tmp/dots-profile/report.md For Habu: json-write STRING emits byte-by-byte after validation,417 instructions/byte vs Zig12.9; escape-free run experiment saves181M instructions on unchanged Dots ls --json. KEY validates twice; RAW costs1135 instructions/call. Reuse span appends for escape-free runs, one authoritative string validation path, one writer-state lookup for RAW. Preserve UTF-8, JSON escaping, nesting/state and failure mutation semantics; inspect existing E2Es before test/code, no DOM/parser duplicate or rollback framework. Reproducer D/probe/jp2/main.f and supplied mode counts in report; measure same-source byte-identical outputs and real instruction/runtime changes, independent Astra review, focused JSON consumers, matching qualified library release.
