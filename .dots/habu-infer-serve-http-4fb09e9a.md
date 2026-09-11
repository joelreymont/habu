---
title: Close terminal completion connection
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.072966+02:00"
blocks:
  - habu-close-completion-conn-7b2fdd67
---

Why: once scheduler cleanup fails, later connection teardown must authenticate the terminal owner without attempting cancellation again. Result: public SERVE-CONN:CLOSE-AFTER-SCHED-FAIL ( SCHED:terminal SERVE-CONN:conn -- SERVE-CONN:terminal-close-result ) calls SCHED:MATCH-TERMINAL with the connection's stored id. Mismatch returns the unchanged terminal and connection before touching the writer or descriptor. A match closes the JSON-WRITE:writer once, attempts socket close once without cancellation, and returns the terminal, every non-socket buffer owner, and any live request handle, plus the socket error when close fails. No completed arm retains a socket or writer owner. Owner: authenticated terminal-scheduler connection close only. Production red: a terminal from one server can otherwise authorize teardown of another server's connection. Acceptance: idle and live matching connections close each owner once; terminal A against connection B returns both byte-identical and makes no close call; close failure preserves every non-socket owner and is never retried; first-of-many and two-server traces keep terminal identity exact. Forbidden: OPEN, healthy CLOSE, cancellation through terminal, read, write, decode, result apply, socket retry, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/connection-state-test.f. Claim: unassigned.
