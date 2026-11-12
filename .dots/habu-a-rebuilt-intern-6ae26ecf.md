---
title: A rebuilt intern index misses a node a kept record shares with a retired one
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:43:15.505174+02:00"
---

Problem: UIX-REC-ADD's comment (src/core/checker.f:10212-10215) records it: a node a kept effect record shares with a retired one stays in the retired record's span, so after UIX-BUILD (:10232) rebuilds the index (a fresh process, a snapshot boot, a foreign rewind) the index lacks that node and a later definition of its shape interns a twin. A twin costs store bytes, never a wrong answer (every hit compares nodes), but a store that differs by whether a rebuild happened can make a warm build's stored effects differ from a cold one's. Acceptance: after a rebuild every live node reachable from a kept record's four row roots is in the index, or a test shows a twin cannot change any byte a build writes and the comment says so. Depends: none. Claim: unassigned.
