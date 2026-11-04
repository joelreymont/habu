---
title: "Build lane 11's engine from a host that exists"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T09:59:38.360781+02:00"
closed-at: "2026-10-31T20:23:16.519949+02:00"
close-reason: "landed: lane 11 as 4b54ed21 Reuse compiled files in incremental builds, 9a25b36f, a8bdf2af on master; gate at the lane tip: native-build rc 0, registry rc 0 1164 s, P0 cold 164 compiled, warm 162 reused/2 compiled, warm == cold, two-generation rc 0; after rebase over ef2bf183 the overlapping rows (check, check-verify, gate-diagnostics, check-all-errors, repair-packet, load-reject-diag, runtime-regression, native-file-frame) rc 0"
---

On the rebased stack, master engines die -8597 after util.f running the stack's hb-build; lane engines lack spawn-fds and refuse master's libraries. Find the cause; fix the stack or add a staged-host route. Brief carl-dots/.out/carl-file/brief-l11-host.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-l11h.
