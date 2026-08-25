---
title: Model pool-window intern paths
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T22:15:51.933132+02:00"
---

Full context: formal/Common/Interning.v covers the fixed-field intern paths (IR-SYM:INTERN, IR-TYPE:INTERN4, IR-ATTR:INTERN5) but not IR-TYPE:FN-END or the IR-ATTR list/record/string paths, whose scan predicates compare a WINDOW of the payload pool in addition to the fixed row cells, and whose capacity check is a PAIR (row table and pool) rather than a single ceiling. The abstract key equality subsumes 'compare the whole canonical content' but says nothing about the window itself. Extend the model to describe pool windows and the two-ceiling atomicity, keeping the zero-axiom discipline. Acceptance: the FN-END and attribute list/record paths each have a functionality, injectivity and fail-closed-ceiling theorem naming their Habu correspondent.
