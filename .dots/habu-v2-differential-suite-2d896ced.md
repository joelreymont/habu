---
title: V2 differential suite schema
status: closed
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.451460+02:00"
closed-at: "2026-07-18T14:20:39.858719+02:00"
close-reason: "All four acceptance axes pinned green in maki/db/diff-suite-test.f (header documents the mapping): (a) per-field digest flip matrix over seed/subject/policy/domain/tolerance/normalization/minimizer/target/budget/generators, (b) incompatible domain+tolerance pairs reject typed, (c) reference-aliasing-subject rejects under the independence policy with two positive controls, (d) replay derives identical case-id sequences (different seed differs). Schema landed in maki/db/diff-suite.f with DECODE (difftensor leg) and the suite-id registry + CASESTORE consumers downstream. Dot had been left stale-active after its lane merged; claim reconciled."
---

Implement MODEL-CAD-V2-PLAN.md:1896-1915 DifferentialSuite artifacts: deterministic generators/corpora, independent references, normalization, comparison domain/tolerance, metamorphic properties, target needs, minimizer, seed, and budget. Acceptance: suite digest changes for every semantic field, incompatible tolerances/domains reject, reference cannot alias subject implementation under independence policy, and suite replay derives identical case ids.

Claim (RELEASED 2026-07-18, landed and merged; stale-active reconciled at goal sweep): agent=diffsuite workspace=.jj-ws/fable-diffsuite
