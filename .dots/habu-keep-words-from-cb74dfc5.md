---
title: Keep words from an earlier cache load capturable
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T22:40:57.512045+02:00"
---

Census C out-of-scope: in one builder session, a definition that calls or ticks a word made by an earlier NDEF-CACHE:LOAD is always refused capture-profile-unsupported, and its file prints binding-key-unknown (routed to ordered-dependency). Probe /tmp/carl-b1/c-probe: P0 (references nothing outside its load) kept, P1-P4 refused; CAPTURE-PREPARE again changes nothing. Every reached capture-refused and executed-closure-unknown case (and frame fixtures fl.f, fq.f, fpar.f, evx.f) comes with it.
