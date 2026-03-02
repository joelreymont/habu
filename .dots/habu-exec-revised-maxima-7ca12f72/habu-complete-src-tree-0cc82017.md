---
title: Complete source-tree search parity after deep review
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-03-07T20:26:35.400742+01:00\\\"\""
closed-at: "2026-03-07T21:21:18.374353+01:00"
close-reason: done (extended lib/maxima-post-load.lisp source-tree bootstrap with *maxima-topdir*/*maxima-docdir*/*maxima-userdir* plus $file_search_usage and userdir/topdir search roots; validated with direct ./zig-out/bin/habu probe resolving romberg via $file_search_usage and showing topdir/docdir/userdir are set)
---

lib/maxima-post-load.lisp:186-220 still lacks upstream init-cl.lisp parity for  and userdir/topdir search roots. Deep review found remaining gaps versus ../maxima/src/init-cl.lisp:243-301. Extend post-load bootstrap without reintroducing installed-prefix assumptions.
