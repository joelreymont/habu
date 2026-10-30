---
title: "uref: unchecked native bodies over an uncallable word exit 70, not an uncaught 67"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T17:49:39.737819+02:00"
---

With T0 default, 0 set-check : X ( -- ) NOSUCHWORD ; exits 67 on uncaught -9600 (tier 1 -8286); JIT 70. Workspace .jj-ws.noindex/jerry-uref on qpvtyqns. Brief /tmp/jerry-big/t0-uref-brief.md; fixes owner-access INTERNAL-ONLY.
