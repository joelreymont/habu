---
title: "C3: narrow->wide int widening"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-27T13:15:58.118003+02:00\""
closed-at: "2026-06-28T15:02:04.793796+02:00"
close-reason: "Landed on habu master@origin 261d3c5b gate-green (warm 153030ms<=160000ms, fixpoint, 0 non-budget fails). CON-OK? lets int-family widths interchange (u8/u32->i64 widens; positive test/c3-widen-test.f); nominal roles (pid/fd/rc) stay strict (negative: ptr u8->i64 rejected 'expected i64 actual ptr u8')."
---

Define widening rules so u8/u16/u32 auto-widen to n/i64 in arithmetic/comparison (all cells at runtime). Fixes 'expected i64 actual u8' when a c@ byte flows into an integer param. src/core/checker.f type lattice.
