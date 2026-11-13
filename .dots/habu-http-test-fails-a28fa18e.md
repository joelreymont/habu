---
title: http-test fails an assert intermittently
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T05:02:40.876017+02:00"
---

Problem: src/lib/net/http-test.f failed on master tree a91d2953 in 1 of 3 runs with 'F281 TFAIL assert 281 expected true got false', on a loaded host (several hb jobs); the same rate showed on the T0 stack engine (jerry-char), and it reddened a full suite run in jerry-t0f (RED: http kind=exit code=1). Which case assert 281 was is not proved: a static count of the assertions LIFECYCLE runs at a91d2953 puts 281 at FLOOD-CASE's `FLOOD-ENDED @ raised > TTRUE` (then :2145), the assert habu-stop-http-test-c5c7c6c6 holds, but the same count puts TRANSCRIPT-CASE at 235 where habu-write-http-test-2d73d4a1 measured 255, so loops shift it. A failure now prints its label beside F<case> (src/lib/test/assert.f:1-6), so a new failure names its case. Every run also logs 'http: request rN failed, throw' lines. One run of today's file on the audit's engine (some commits behind master, cut at 10 seconds before it ended) logged six: -9990 (E-BOOM, the test's own fixture refusal, http-test.f:92) and -3900 (E-JSON-READ-MALFORMED, src/lib/json/read.f:17), which cases provoke on purpose. The -5702 seen earlier (E-BUF-STATE, a touch of a disposed buffer, src/lib/error.f:257) is unexplained; a passing test with such a failure may be masking it.
Acceptance: the cause of the intermittent failure is reported at file:line, or shown to be c5c7c6c6's or 2d73d4a1's and this dot closes into that one; each 'request failed' line of a passing run is traced to the case that provokes it, -5702 included.
Files: src/lib/net/http-test.f, src/lib/net/http.f.
Verify: first, with a hb built from master, run bin/hb --load src/lib/net/http-test.f repeatedly under load and read each failure's label; then map each request-failed line of a passing run to its case.
Depends: none. Ownership: unassigned. Claim: unassigned.
