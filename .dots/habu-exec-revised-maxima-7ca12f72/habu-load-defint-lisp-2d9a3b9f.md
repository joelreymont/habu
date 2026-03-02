---
title: Load defint.lisp after VM/control-stack blockers are fixed
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T19:32:55.789743+01:00\\\"\""
closed-at: "2026-03-08T18:07:29.844039+01:00"
close-reason: "done: re-enabled defint in lib/maxima-loader.lisp:79-81 after focused probe /tmp/probe_defint3.lisp proved direct post-bootstrap load now succeeds. Validation: /tmp/probe_defint_loader_enabled.lisp printed DEFINT=t."
blocks:
  - habu-audit-and-harden-5576b7ee
---

lib/maxima-loader.lisp:66-68; ../maxima/src/defint.lisp:1-120. Root cause: defint/residu are excluded because loading them overflows current VM limits. Fix: close the real overflow blocker, then load defint and rerun the rtest10 definite-integral cluster. Why: this is the major Stage-4 capability gate.
