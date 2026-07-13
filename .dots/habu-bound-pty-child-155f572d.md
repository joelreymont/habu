---
title: Bound PTY child completion
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:14:28.571012+02:00\""
---

Full context: pending lib/process-pty.f WAIT first polls PTY HUP then calls blocking PROC-WAIT-RC. A child can close stdio and continue, causing an unbounded wait; a descendant can retain the PTY after the child exits, causing a false timeout. Add a checked nonblocking wait outcome primitive and poll child completion independently while draining PTY; deadline must kill and reap. Dependency: owner persistence support hardening.
