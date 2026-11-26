---
title: Charge allocated bytes to the words that allocate them
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:08:36.719142+01:00"
---

Problem: Habu has no memory profiler; finding where a SPARQL compile's memory went took wrapping 755 words in a source copy to count arena bytes, and perf cannot run on spark (perf_event_paranoid 4). Acceptance: a run's report charges allocated and resident bytes to the calling words, as the sampling profiler charges ticks. Depends: none.
