---
title: Restore real dbm-read/mdebug support for batch/demo
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-03-07T19:20:07.549975+01:00\\\"\""
closed-at: "2026-03-08T00:12:11.196628+01:00"
close-reason: done (plain $batch now executes real files; pathname open support fixed; mini canonical test-batch passes 2/2 with answers-from-file; remaining rtest1 issues are substantive Maxima/runtime failures, not missing batch/demo infrastructure)
blocks:
  - habu-load-testsuite-generr-7386a168
---

lib/maxima-stubs.lisp:314-331; ../maxima/src/mdebug.lisp:262-340; ../maxima/src/macsys.lisp:163-313. Root cause: current dbm-read stub is too small for continue/batch/demo semantics. Fix: load mdebug or provide a contract-correct replacement for the exact continue path. Why: closes the remaining :batch/:demo infrastructure gap after composite stream support exists.
