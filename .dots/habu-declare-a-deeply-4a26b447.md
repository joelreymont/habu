---
title: Declare a deeply nested type in linear time
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T14:22:20.674789+01:00"
---

Problem: declaring a structure whose field type nests a family application k deep (sdnest<n,sdnest<n,…n>>) costs 0.085 s at k=80 and 1.04 s at k=300 on an engine built from szunmtun (no 256 cap): about k squared; the cause is not yet found. Acceptance: the declaration's cost grows linearly with k; test/structure-decl-suite.f's 300-deep case declares in well under 0.1 s. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-sdarg (read-only hunt)
