---
title: Restore correct meval* top-level environment fully
status: open
priority: 2
issue-type: task
created-at: "2026-03-07T19:32:55.806186+01:00"
blocks:
  - habu-harden-handler-restart-d526af6a
---

lib/maxima-post-load.lisp:89-103; ../maxima/src/suprv1.lisp:69-85; ../maxima/src/compar.lisp clearsign paths. Root cause: current override avoids a VM bug but still diverges from upstream with-top-level-environment semantics. Fix: remove or minimize the override while preserving correct cleanup and errcatch/context behavior. Why: completes the hard-cutover path after the interim mitigation.
