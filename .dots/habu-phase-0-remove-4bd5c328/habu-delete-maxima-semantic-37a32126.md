---
title: Delete Maxima semantic stubs
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.043133+02:00\""
closed-at: "2026-04-01T22:35:25.966906+02:00"
close-reason: "done: lib/maxima-stubs.lisp reduced to package declarations only; rg -n 'defun|defmacro|setf|setf symbol-function' lib/maxima-stubs.lisp is clean; full validation still blocked by existing zig build test baseline errors in disasm/opcode coverage and builder.lambda arity callsites"
blocks:
  - habu-delete-maxima-patch-ec88cbb4
---

Problem: lib/maxima-stubs.lisp still changes semantics instead of exposing missing CL support. Acceptance: semantic fallback operators, macros, and fake packages are gone or reduced to non-semantic declarations. Files: lib/maxima-stubs.lisp:2-34,97-215,265-340. Verify: rg -n 'defun|defmacro|setf|setf symbol-function' lib/maxima-stubs.lisp and Maxima load now fails only on real Habu gaps. Blockers: habu-delete-maxima-patch-ec88cbb4.
