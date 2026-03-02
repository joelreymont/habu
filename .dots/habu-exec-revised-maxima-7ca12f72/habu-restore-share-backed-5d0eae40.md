---
title: Restore share-backed autoload after bootstrap and module audit
status: active
priority: 2
issue-type: task
created-at: "\"2026-03-07T19:20:07.561825+01:00\""
blocks:
  - habu-audit-missing-upstream-2761d1dd
---

../maxima/src/autol.lisp:1-53; ../maxima/src/max_ext.lisp:1-420; lib/maxima-post-load.lisp:129-170. Root cause: autoload depends on truthful share/** search paths, canonical load/test plumbing, and clear module classification. Fix: verify representative .mac and .lisp autoloads from a clean image and close missing share path cases. Why: this is the Stage-2 gateway to ode2/trigsimp/stringproc-style coverage.
