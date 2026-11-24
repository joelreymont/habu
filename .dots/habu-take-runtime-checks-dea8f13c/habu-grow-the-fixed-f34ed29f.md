---
title: Grow the claim names and fault texts
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T18:45:09.554303+01:00"
closed-at: "2026-11-24T16:03:36.004196+01:00"
---

Problem: src/habu/data-claims.f NAMES (a fixed blob of counted names, a die past NAMES-CAP and past a 255-byte name) and MSG-BUF (silently truncated at MSG-CAP), and src/habu/fault-record.f FAULT-PROSE/FAULT-JSON (a die past FAULT-TEXT-CAP), are fixed build-side buffers. Acceptance: each is a vector<u8> that grows, with no capacity constant, cap die or truncation; engine builds converge. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-grow.
