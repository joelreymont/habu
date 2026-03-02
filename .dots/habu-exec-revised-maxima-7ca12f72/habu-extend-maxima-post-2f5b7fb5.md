---
title: Extend maxima-post-load search bootstrap to sharedir/wxm/demo
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T19:20:07.502132+01:00\\\"\""
closed-at: "2026-03-07T20:04:09.482850+01:00"
close-reason: done (extended lib/maxima-post-load.lisp source-tree bootstrap to detect *maxima-sharedir*/*maxima-demodir*, populate recursive share-backed / with .lisp/.mac/.wxm coverage, and broaden  to real demo extensions; validated with direct ./zig-out/bin/habu probes resolving ode2, trgsmp, test_readbase_maxima, and manual.demo)
blocks:
  - habu-implement-recursive-wildcard-32f96a70
---

lib/maxima-post-load.lisp:129-170 and ../maxima/src/init-cl.lisp:243-301. Root cause: Habu bootstraps only src/tests, omitting *maxima-sharedir*, .wxm, and demo search vars. Fix: set sharedir and align // with upstream source-tree expectations. Why: makes source-tree file search truthful after wildcard support exists.
