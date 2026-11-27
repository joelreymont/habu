---
title: Floor and ceiling a float to an integer in FMATH
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T14:47:19.533212+01:00"
---

Problem: FMATH (src/lib/fmath.f) rounds a float to an integer only by FROUND (:309); Maki's viewer.f defines its own >FLOOR and >CEILING (f>s, then a step down or up) for a toolbar range's whole-mm bounds (tim's request /tmp/tim-habu-1127.md). Acceptance: FMATH has a floor and a ceiling ( r -- n ) beside FROUND, rejecting a nonfinite or out-of-range value as FROUND does, with negative, integral and half-way cases in src/lib/fmath-test.f; Maki can drop its own pair. Depends: none.
