---
title: Cache a parent that loads a child from another scope
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T23:13:17.978746+02:00"
closed-at: "2026-10-31T20:52:55.179093+02:00"
close-reason: "Obsolete after lane 11: FILE-SCOPE-KEPT/scope-at-load 0 hits; a warm load re-runs the top level, so children load in the live scope."
---

tools/native-unit-object.f FILE-BEGIN refuses scope-at-load (NUNIT-DEP:FILE-SCOPE-KEPT?) and native-definition-cache.f refuses scope-exit: a parent whose search order, current wordlist or package at a child load differs from its begin scope gets no object, because replay loads the child in the parent's begin scope. Observed with wlk.f (bare wordlist current at require) in the wlid2 lane. Every admitted shape must cache: replay must load the child in the scope recorded at the load event, and the file's exit scope must be part of its object. Measure how many library/app files hit these refusals first.
