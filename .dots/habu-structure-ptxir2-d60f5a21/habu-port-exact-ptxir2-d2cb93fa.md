---
title: Port exact PTXIR2 optimizations
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:47.961623+02:00"
blocks:
  - habu-verify-ptx-virtual-50281017
---

Full context: design sections 8.8 and GPU Wave A retain only exact structured PTX passes: copy propagation, CSE, DCE, move simplification, predicate simplification, and exact address folding. Acceptance: each pass has mutation fixtures and independent validation; no floating reassociation, text matching, generic fallback, or unbound witness.
