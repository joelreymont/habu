---
title: Add get-setf-expansion
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T12:16:07.131571+01:00"
---

docs/setf-expander-research.md:32 outlines get-setf-expansion contract; docs/cl-symbols.md:1086 marks missing. Root cause: setf macro hardcodes places, no expander API despite define-setf-expander existing. Fix: implement get-setf-expansion in lib/stdlib.habu + runtime registry for define-setf-expander; refactor setf macro to call get-setf-expansion for non-built-in places; add ohsnap snapshot tests for expansions.
