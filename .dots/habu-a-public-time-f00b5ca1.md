---
title: A public TIME word for milliseconds since a reading
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T12:11:17.763783+01:00"
---

Problem: Maki (tim 2026-11-25, /tmp/tim-habu-logging.md) needs the whole milliseconds since a TIME:MONO-NS reading for its log lines; TIME (src/lib/time.f) has only MONO-NS, NS-PER-MS is private to several packages (process.f:99, process-tree.f:60, http.f ;ACCESS-LINE) and Maki defines its own. Acceptance: one public word ( ns -- ms ) in TIME, truncating, named by Joel; http.f's access line uses it; one test. Depends: none. Claim: unassigned.
