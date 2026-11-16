---
title: "Name a candidate's text as the load does under check"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:17:16.613193+01:00"
---

Problem: a rejection in text CHECK-CANDIDATE! checks is placed at the subject under tools/check.f (engine-suite.f:1:30, probe.f:1:1) where --load places it at <input> (probes /tmp/carl-wd/probe.f, probe2.f). A suspected cause, unverified: REJECTION:LABEL! (src/core/rejection.f ~1067), which check's run prefix sets, names every unnamed text after the subject. Acceptance: a candidate's records name the same place under check.f as under --load. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-lb.
