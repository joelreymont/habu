---
title: Keep a captured callable on its named row
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T11:13:53.233832+01:00"
---

Problem: : W ( R [ R -- R ] -- R ) {: f :} 1 [: f execute ;] execute drop ; with U passing [: dup c@ drop ;] certifies, then faults E-MEMORY-FAULT at address 1: the literal capturing f is typed [ -- ], its tail not tied to W's row R, so f runs on R n (found by the loop feature's review; on master too). Acceptance: the reproducer is rejected; a test fails without the fix. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-qcap
