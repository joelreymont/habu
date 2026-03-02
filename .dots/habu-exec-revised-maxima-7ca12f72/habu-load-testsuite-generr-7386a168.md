---
title: Load testsuite/generr/macdes before canonical test runs
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T19:20:07.508042+01:00\\\"\""
closed-at: "2026-03-07T20:06:45.658579+01:00"
close-reason: done (updated lib/maxima-loader.lisp to load generr before clmacs, testsuite before mload, and macdes in the display/docs helper block; validated with direct ./zig-out/bin/habu probe showing errset macro, , and mread-noprompt are present after maxima-load-all)
blocks:
  - habu-extend-maxima-post-2f5b7fb5
---

lib/maxima-loader.lisp:27-86; ../maxima/src/testsuite.lisp:1-260; ../maxima/src/generr.lisp:1-25; ../maxima/src/macdes.lisp:80-86. Root cause: canonical testsuite metadata, errset, and mread-noprompt are absent from the image. Fix: load these files in the Habu Maxima image. Why: test-batch/run_testsuite and file-driven asksign paths depend on them.
