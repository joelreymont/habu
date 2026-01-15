---
title: Test package system
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:43:47.312580+02:00"
---

Create test: (defpackage :test-pkg (:use :cl) (:export :foo)), (in-package :test-pkg), (defun foo () 42), (in-package :cl-user), (test-pkg:foo). Dependencies: habu-wire-pkg-primitives-1e808e97. Verify: packages isolate symbols.
