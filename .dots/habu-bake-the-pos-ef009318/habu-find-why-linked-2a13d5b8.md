---
title: Find why linked parts make fewer wordlists
status: closed
priority: 2
issue-type: task
created-at: "2026-11-19T13:53:39.712846+01:00"
closed-at: "2026-11-19T14:08:50.781176+01:00"
---

Problem: with part B recompiled after A grew, B's `get-current` answers 830 when the earlier parts were linked from their objects and 1183 when they were compiled (reproducer /tmp/fable-bake-p, /tmp/jerry-bake-r). Cause unknown: a compile makes wordlists a link does not, so anything that numbers or counts them differs warm and cold. Acceptance: the cause named with its reproducer; then a warm and a cold build make the same wordlists. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-wlc.
