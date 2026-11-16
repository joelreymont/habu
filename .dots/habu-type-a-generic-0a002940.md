---
title: "Type a generic structure's field accessors over its parameter"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T18:26:49.210137+01:00"
---

Problem: a generic STRUCTURE reached through a pointer refuses every layout-family instance: STRUCTURE held<a> DERIVE addr FIELD value a FIELD set bool, then HELD-SET on ^held<thing> stops E-MISMATCH 'takes ^held<a>; the stack holds ^held<thing>' (/tmp/jerry-held/p2.f; bob's /tmp/bob-held-probe.f, an ARENA of held<thing>). Cause: SD-ARG-AT (src/core/checker.f:22947-22950) leaves a width-slot variable under ^ as TVK-ANY, and LAYOUT-BLOCK? (:4205-4218) refuses it against any layout family; the accessors' offsets and AT stride count a as one cell, so a wide instance (thing is two cells) would read the wrong cells. Acceptance: an instance of any width, reached through a pointer, is accepted and its accessors read its own cells, as MAKE and UNMAKE already take wide instances by value (test/structure-certify-suite.f:62-72); what stays refused names its reason. Depends: none (designed; held-1 of 3 running). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-held
