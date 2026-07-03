---
title: "Maki: unified single-slot tensor value"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T01:29:11.710362+02:00"
---

CAD-PLAN section 3 prerequisite (plan-review round-2 quorum finding). A tensor travels as ONE stack slot carrying data ptr, shape, dtype, layout, alignment class - a value struct (compiler work authorized if the checker needs it; coordinate with TFAM product families rather than duplicating). Today eager ops pass multi-cell tensors (maki/linear.f LINEAR ( ptr a ptr a ptr a ptr a n n n -- )) and tensor.f is metadata-only. Deliver: the tensor value type + constructors/accessors, the planning-vocabulary base over it (descriptor mode appends IR nodes), and migration of at least LINEAR/GELU onto it as the worked example; eager path migrates module-by-module after. Blocks: cad-1 checked MODEL: capture. Depends: none (can start now).
