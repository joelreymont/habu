---
title: Load residu.lisp after defint is stable
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-03-07T19:32:55.795764+01:00\\\"\""
closed-at: "2026-03-08T18:07:29.848734+01:00"
close-reason: "done: re-enabled residu in lib/maxima-loader.lisp:79-81 before defint to preserve dependency order. Validation: /tmp/probe_defint_loader_enabled.lisp printed RESIDUE=t and canonical timeout run of tools/maxima-rtest.lisp rtest1 still reached the known mid-suite blocker."
blocks:
  - habu-load-defint-lisp-2d9a3b9f
---

lib/maxima-loader.lisp and ../maxima/src/residu.lisp. Root cause: residu is excluded in the same family as defint. Fix: load it only after the defint blocker is understood and closed. Why: residue-related coverage depends on the same Stage-4 capability restoration.
