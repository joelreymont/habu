---
title: Scope GPT-2 prepare imports
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T01:49:19.809464+02:00"
---

Problem: the renamed prepare suite is a changed multi-call consumer with repeated SAFET and MDLCFG-CFGKEY calls. Owner and result: package GPT2LOAD in maki/infer/gpt2-prepare-test.f opens using SAFET after its one-off model-config setup and closes it after the final normal prepare test definition, before the checker-candidate section. A nested using MDLCFG-CFGKEY surrounds only SAVE-PREPARED-KEY through PREPARED-KEY-DIFFERS?. All executable repeated calls become bare except SAFET:COUNT, whose tail collides with the live global COUNT. SAFET:LOAD is proven unambiguous in the exact owning load and becomes bare. Preserve one-off MDLCFG-ARCH:LLAMA and MDLCFG:BUILD, all types and MATCH selectors, candidate strings, comments, definitions, call order, and behavior byte-for-byte. Dependency: accepted loader rename ae1f14633232efba954e04e76f82b3b73e9e56be. Checkpoint: the exact prepare suite is green; a token-aware census proves the two repeated packages, the COUNT collision, and bare LOAD; a representative import passes the package gate. Files: maki/infer/gpt2-prepare-test.f only. Forbidden: leaving an import active across candidate evaluation, retaining SAFET:LOAD, removing SAFET:COUNT qualification, API or behavior changes, aliases, package reopen, string edits, metadata, or other files. Acceptance: exact prepare suite and exact-diff typed-local and package gates pass; scopes are balanced and no executable repeated qualifier remains except SAFET:COUNT.

Claim: agent=gpt2-prepare-using workspace=.jj-ws/habu-scope-gpt-2-79b61f0f
