---
title: Collapse the comparison scaffolding
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.665154+02:00"
---

CG-30, post-cutover. The proof/benchmark lane is ~13.4K lines across 42 files: codegen-compare-gap.f defines nine capability variants with one (loop-spill) used by two rows, a generic known-loss registry exists for one row, codegen-align-sweep.f is a 926-line one-off, the lookup-cost tool likewise. Preserve the measurements and conclusions, then collapse to one canonical source/corpus table, direct expected result/refusal/size relations, one runner, and the minimum reusable measurement code. Delete historical taxonomies and campaign one-offs. Blocked by the hard cut (the old-vs-new half deletes with the old compiler).

Claim: agent=scaffold-del workspace=.jj-ws/habu-delete-src-substring-2564d854
