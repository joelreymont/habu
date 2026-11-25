---
title: A public TIME word for milliseconds since a reading
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T12:11:17.763783+01:00"
---

Problem: Maki (tim 2026-11-25, /tmp/tim-habu-logging.md) needs the whole milliseconds since a TIME:MONO-NS reading for its log lines; TIME (src/lib/time.f) has only MONO-NS, NS-PER-MS is private to several packages (process.f:99, process-tree.f:60, http.f ;ACCESS-LINE) and Maki defines its own. Acceptance: TIME:ELAPSED-MS ( ns -- ms ) (Joel 2026-11-25 named it), the whole milliseconds since the reading, truncating; http.f's access line uses it, and the http suite's access-line case covers it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-elapsed.
