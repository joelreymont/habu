---
title: process-signals misses a SIGSTOP report
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T13:42:11.078339+02:00"
---

Problem: a full macOS suite failed process-signals assert 5, 'a child sent SIGSTOP is reported stopped by that signal' (src/lib/signal-test.f:299-315): STOP-SEEN? (:288-295) gives up after STOPPED-MS 1000 ms (:89) or on PROBE-OTHER, and does not say which. It passed 3 of 3 alone on the same engine. Acceptance: the cause is found and fixed; the test waits for the stop report without a deadline a loaded host can miss, and names the outcome it saw when it fails. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-sig.
