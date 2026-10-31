---
title: "Write http-test's transcript under TMPDIR"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T21:25:39.989368+02:00"
---

Problem: src/lib/net/http-test.f TRANSCRIPT-CASE writes and reads the fixed path build/http-transcript.txt under the working directory (:1085, :1092; header :4), so two http-test runs from one directory overwrite each other's file: assert 255 failed in 7 of 160 parallel runs (flood lane, /tmp/carl-flood). A test writes only under its own temporary directory. Acceptance: the transcript lives under the run's TMPDIR scratch (as other tests make theirs); 20 parallel http-test runs from one directory pass assert 255; the header comment names the new place. Files: src/lib/net/http-test.f. Verify: four concurrent hb --load src/lib/net/http-test.f runs from one directory, five rounds. Depends: none. Claim: unassigned.
