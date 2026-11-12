---
title: Write the map slot-state line once and test it
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:00:54.612452+02:00"
---

Problem: src/lib/map.f BAD-STATE writes `E-BAD-TAG habu: map slot state <n> outside 0..1` in five writes to fd 2, so another thread's output can interleave, and no test asserts the line (src/lib/map-test.f:204-207 and :226 check only the 85); the header (:4-5) says malformed storage throws E-MAP-*. Acceptance: the line is written in one write before the throw of 85; map-test asserts the line; the header states the slot-state refusal. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mapline.
