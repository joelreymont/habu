---
title: Audit loader failures
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:04.159105+01:00"
blocks:
  - habu-define-maxima-gates-aca4e665
---

lib/maxima-loader.lisp:1 and /tmp/maxima/src/*.lisp. Cause: per-file success hides per-form semantic failures. Fix: strict form-level failure capture and symbol binding audits.
