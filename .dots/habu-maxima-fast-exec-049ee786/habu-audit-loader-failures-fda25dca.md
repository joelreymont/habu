---
title: Audit loader failures
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:04.159105+01:00\""
closed-at: "2026-02-20T00:12:24.717243+01:00"
close-reason: loader now exposes stop-on-error and required-binding audits via maxima-load-all internal controls
blocks:
  - habu-define-maxima-gates-aca4e665
---

lib/maxima-loader.lisp:1 and /tmp/maxima/src/*.lisp. Cause: per-file success hides per-form semantic failures. Fix: strict form-level failure capture and symbol binding audits.
