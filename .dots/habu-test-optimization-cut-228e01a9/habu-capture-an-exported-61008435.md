---
title: Capture an exported private word
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:34:48.875528+02:00\""
closed-at: "2026-10-01T17:37:56.946424+02:00"
close-reason: Fixed by rqrmzstt 173673ce (review 161 ACCEPT, comment fixed at integration)
---

Problem: an AOT capture of a window word that calls a private word made public by EXPORT is refused REFUSE-SCOPE (wl 367), though the loader accepts the call: a false refusal. Found by r4-acap (commit 6fea6d34) probe s3 in $HOME/.cache/tmp/kestrel-r4-acap/probe/ (HABU_BAND=empty, bin/hb of that lane). Acceptance: the capture qualifies an exported word through the wordlist the export put it in, so s3 captures (rc 0, one site); the scope refusal still fires for a word the window cannot name (probes s1, s1b, s2); a case in test/aot-prelude-band-suite.f seen to fail first. Files: src/habu/aot-capture.f, src/habu/xref.f.
