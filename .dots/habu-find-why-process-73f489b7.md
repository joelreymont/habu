---
title: Find why process-test.f stalls under caffeinate
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T15:34:21.384887+02:00"
---

Problem: `caffeinate -i bin/hb --load src/lib/process-test.f` (installed hb; tree master 1f0cba57 plus the capture lane's new test TEST-CAPTURE-SPAWN-THROWS, whose failure there leaves the capture descriptors open) sat idle with no output for over 3 minutes, with caffeinate seen as a child of the hb process; the same run without caffeinate finished. Cause unknown; agents run builds and tests under caffeinate -i. Acceptance: the cause traced and fixed at its layer; the run finishes under caffeinate -i as it does without it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-caff.
