---
title: Establish HTTP low-speed fixture preconditions
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-25T09:00:48.057324+02:00\\\"\""
closed-at: "2026-09-25T09:29:40.965480+02:00"
close-reason: "Wait for complete server cleanup after both intentional timeout cases before starting the next timed request; preserve transfer deadlines, low-speed options and assertions. Independent review approved sequence identity and completion ordering. Full 490-suite gate passes. Both engines also pass the full HTTP E2E with forced 2500 ms first-stall and aborted-dribble cleanup delays, with both markers and correct completion ordinals verified. Original failed logs retained; their unreported CURLcode is not inferred retrospectively. Evidence: ~/.cache/tmp/habu-opt-round2/curl-race/RESULTS.md."
---

The combined native gate at frozen a99459ca runs the unchanged curl HTTP E2E
beside compiler suites. DRIBBLE-LOW-SPEED assertions 33–36 report failed-result
kind 2, status 0 and empty body; the actual curl error is absent from the log.
The earlier combined gate passed this suite. Diagnose with the full existing
E2E and capture LAST-CODE, elapsed time and server progress before fixing it.

The fixture serially serves a timed-out prior dribble, then expects a new writer
sleeping 100 ms per 64-byte chunk to stay above 10 B/s over one second.
TASK:SLEEP specifies a minimum, not maximum, scheduling delay. Preserve all
production errno semantics, transfer limits, rates and assertion strength.
Fix the responsible fixture or harness layer only after evidence. Acceptance:
classified failure on baseline and candidate, meaningful focused actual-HTTP
evidence and full native registry; retain failing logs. Evidence directory:
`~/.cache/tmp/habu-opt-round2/curl-race/`; original failure:
`combined/final-gate.log`.

Pre-fix diagnostic evidence: both engines pass the unchanged full E2E alone,
but the next low-speed transfer starts 130–260 ms before the prior aborted
request completes. Delaying only that prior cleanup by 2.5 seconds reproduces
CURLE_OPERATION_TIMEDOUT on both engines after about 1.13 seconds, with the new
request not yet parsed. Those forced-failure runs also report a later thread
count assertion (159); the complete failing logs are retained. The original
gate lacked this trace, so its exact
timeout cause remains unclassified. The demonstrated fixture coupling is
independently wrong: one case's cleanup must finish before the next timed case.

Design: count request completion after ROUTE returns, including drain and close.
Each of the two consecutive timeout cases records its expected request sequence
before starting and waits for completion before handing the server to the next
case. Independent review caught that waiting only after the second timeout
could mistake the first request, not yet parsed, for its own sequence. Bound
the fixture wait by the existing general request budget; leave all transfer
deadlines and speed options unchanged. Do not use request arrival as completion
or mistake an unaccepted request for an idle server. Validate the same forced
cleanup delay with the barrier and retain original failures.

Final acceptance: independent review approved both barriers. Frozen source
a904e2ed passes the complete 490-suite gate, exit 0, with the unchanged optimized
engine. A full-E2E control delays the first /stall before its hit counter and
the first aborted dribble cleanup by 2.5 seconds each. Both engines pass, with
both delay markers followed by the correct completion ordinals before the next
case. The exact frozen HTTP fixture also passes on the accepted baseline.
All logs and repeatable drivers are in `curl-race/RESULTS.md` under the evidence
directory above. The original gate's unreported curl code remains unknown.
