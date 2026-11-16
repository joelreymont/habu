---
title: Charge foreign calls to callers on x86
status: closed
priority: 2
issue-type: task
created-at: "2026-11-17T11:02:43.631419+01:00"
closed-at: "2026-11-17T14:06:28.862422+01:00"
---

Problem: the x86 profiler (src/habu/prof-x64.f HEAD, ~878) has no 'foreign charged to callers' accounting or report field that arm64 has (src/habu/prof.f ARN-FCALLED ~73, ~437-439, ~821, ~1148), so test/x86-64-kernel-prof.f's host capture fails on an x86 host. Acceptance: x86 counts and reports the field as arm64 does; the test's two CUT-FIELD lines for it are deleted; x86-64-kernel-prof passes on both hosts. Depends: Switch on the x86 boot-harness suites. Claim: agent=carl workspace=.jj-ws.noindex/carl-xpf.
