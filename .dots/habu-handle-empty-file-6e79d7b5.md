---
title: Handle empty FILE dependency hashing
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T07:18:54.504174+03:00"
---

At frozen83530bcd, KEY-MATCH? in tools/native-file-cache.f reserves sourceu then indexes 0 HASH-SOURCE. When the first checked child is empty, reserve0 leaves zero capacity and DYNAMIC-STORAGE:OFFSET throws E-LAYOUT-BOUNDS instead of replay or fallback. Keep an addressable zero-length hash/read base without modifying the parent active source. Existing real FILE E2E should include an empty child, successful fresh/warm builds and executable output. Carl independent_file_core owns implementation; independent review and matching native FILE checks remain required.
