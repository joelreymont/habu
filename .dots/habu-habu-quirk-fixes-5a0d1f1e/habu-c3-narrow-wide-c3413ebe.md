---
title: "C3: narrow->wide int widening"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T13:15:58.118003+02:00"
---

Define widening rules so u8/u16/u32 auto-widen to n/i64 in arithmetic/comparison (all cells at runtime). Fixes 'expected i64 actual u8' when a c@ byte flows into an integer param. src/core/checker.f type lattice.
