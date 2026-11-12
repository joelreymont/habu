---
title: "Throw the map's own code for a bad slot state"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T17:00:54.612452+02:00"
closed-at: "2026-11-12T17:04:47.914102+02:00"
---

Problem: src/lib/map.f SLOT-STATE@ prints `E-BAD-TAG habu: map slot state <n> outside 0..1` in five writes to fd 2 and throws ENGINE-ERROR:BAD-TAG (85), an exit status, while the map's other malformed storage (a bad hash, a bad key length) throws E-MAP-BAD-CAP as the header (:4-5) says. Acceptance: a bad slot state throws E-MAP-BAD-CAP and prints nothing; src/lib/map-test.f asserts the code. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mapline.
