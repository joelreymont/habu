---
title: "Report a recursive call's stored-type refusal at the call"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T07:14:00.241050+02:00"
---

Problem: RECUR-CHECK (src/core/checker.f, after NP-DECLARED?) runs from NP-CHECK after the body, so a recursive call whose instance of a cast's stored variable the body later refuses is reported at the definition's last token (~/.cache/tmp/carl-l3c2/p/l1.f: "at '>ELEM' a cast's stored type would hold a linear value expected: a actual: [ -- L1Q:tok ]"), not at the RECURSE that bound it; with several recursive calls an agent cannot tell which one. Every NP-CHECK refusal reports at the last token the same way. Acceptance: the refusal names the RECURSE token of the failing instance (RECUR-INST+ keeps each call's token position beside the pair; RECUR-MEET reports there). Files: src/core/checker.f RECUR-*, test/compiler/stored-type-param.f CASE-RC-LINEAR (pin the token). Verify: that test; l1.f's refusal shows token RECURSE. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-rcur.
