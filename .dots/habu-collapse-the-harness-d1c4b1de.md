---
title: Collapse the harness corpus boilerplate
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-03T16:22:48.121580+02:00\""
---

Four corpora each carry near-identical cases/migrated/new file triplets, hand-copied per lane; and --update rewrites all four tables, which has forced three lanes to snapshot-and-restore untouched baselines by hand. Wanted: (1) a per-table update (--update corpus3 style, or per-cases-file entry) so regenerating one baseline cannot touch the others; (2) factor the triplet boilerplate into shared machinery driven by each corpus's declarations (the way frozen.f collapsed the five passes' reader plumbing) - net lines strongly negative, zero behavior change, all four tables byte-identical output. Do not merge the corpora themselves: four tables with four purposes is the design, the file mechanics are the debt.

Claim: agent=simplane workspace=.jj-ws/habu-collapse-the-harness-d1c4b1de
