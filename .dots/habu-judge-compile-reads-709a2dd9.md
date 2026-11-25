---
title: "Judge compile-reads.f's code in programs"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:20:36.320871+01:00"
---

Problem: a stripped program keeps about 4.6 KB of src/core/compile-reads.f (measured in tenderd); hypothesis: APPEND exits while STATUS is ST-OFF, so a program never uses it. Acceptance: the edge that keeps it is named, and a program that compiles nothing keeps none of it. Depends: none. Claim: unassigned.
