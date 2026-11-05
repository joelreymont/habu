---
title: "MODEL's log line shows what a stalled stream carried"
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T11:09:21.991203+02:00"
---

From stuart (Tender corpus), 2026-11-05. Problem: about 12 of 360 calls stalled 1366-3630 s while carrying 34k-81k events, and MODEL's log line does not show what such a stream carried, so a stall cannot be told from a long think. Evidence: /Users/joel/Work/tender/.jj-ws.noindex/stuart/.out/corpus-main26a8-live/source/. Acceptance: the line MODEL logs when a call ends (E-TRANSPORT, E-RESPONSE, or a failed status) carries the time to the first output event and the event counts by type; no bound or tier change (those are Joel's). Files: src/lib/net/model.f, src/lib/net/model-test.f. Verify: model-test.f with STDERR:CAPTURE of the line. Depends: none. Ownership: carl. Claim: carl, workspace carl-slog (brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-model-slog.md).
