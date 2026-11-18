---
title: Unify two k-deep values in time linear in k
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T15:44:39.153932+01:00"
---

Problem: after the declaration fixes (dot 4a26b447), using a k-deep type by value still costs about k squared: value.f (/tmp/carl-wide2-brief.md's Reproducer) takes 0.32 / 1.36 / 4.98 s at k = 400 / 800 / 1600. At k=800 UNIFY/U-TYPE is about 37% inclusive with PARAM-PAIR-ARGS 12.6%: the W hidden cells share one argument tree and U-TYPE pairs it again per cell. A stamp holding only the other term is unsound: a pair's outcome depends on the context it was pushed with (CUR-STRICT, CUR-ROLE, CUR-LIT, CUR-QUOT, CUR-ARG; PARAM-BIND-OK? and the con checks read CUR-STRICT). Acceptance: value.f's time grows linearly in k up to k=3200; the type suites pass. Depends: none. Claim: unassigned.
