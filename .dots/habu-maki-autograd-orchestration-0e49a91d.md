---
title: "Maki: autograd orchestration + user API"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:50.973429+02:00"
blocks:
  - habu-write-docs-maki-782bef52
---

D. Implement the maki-level autograd ORCHESTRATION per docs/maki/autograd.md: the tensor-op VJP rule table lowering onto the Habu primitive VJP: table, and the user-facing define-forward-get-checked-backward API. Distinct from the Habu kernel transform (ad-reverse).
- Files: maki/autograd.f.
- Verify: a user tensor-op forward yields a checked backward that gradchecks; the C-vs-D seam holds.
- Dep: docs/maki/autograd.md (habu-write-docs-maki-782bef52) + ad-reverse (habu-ptx-ad-reverse-26aebee3) + maki tensor types.
