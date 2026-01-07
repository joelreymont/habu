---
title: CL Spec Compatibility - TRACKING
status: closed
priority: 1
issue-type: task
created-at: "2025-12-29T16:08:30.456732+02:00"
closed-at: "2026-01-01T00:56:05.040002+02:00"
---

Master tracking dot for CL spec compatibility.
TIER 1 (Foundation - No deps, do first):
- destructuring-bind
- reduce
- mapcar/mapc/mapcan/maplist
- ignore-errors

TIER 2 (Core features):
- Sequence: every/some, *-if variants, subseq, substitute, sort
- Control: typecase, ecase, prog, multiple-value-setq, nth-value
- Data: setf system, copy-*, coerce, concatenate
- Misc: apply improvements

TIER 3 (Advanced):
- Full loop macro
- Condition system: restart-case, handler-bind, cerror
- Strings: comparisons, trim, format directives
- Hash tables, streams, packages, defstruct improvements

TIER 4 (Major systems):
- CLOS: defclass, defmethod, defgeneric, make-instance
- Multi-dimensional arrays
- Numeric types: rationals, complex, floats
- Reader macros
