---
title: Scope GPT-2 fixture imports
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T01:49:19.373976+02:00"
---

Problem: the renamed checkpoint fixture is a changed multi-call consumer with repeated qualified calls. Owner and result: package GPT2LOAD in maki/infer/gpt2-checkpoint-fixture.f keeps all behavior and opens one balanced import for each of GPT2TENSOR, MDLCFG-ARCH, MDLCFG, SAFET-MAP, SAFET, WSTORE, and MDLCFG-CFGKEY immediately after package setup, then closes them in reverse order before ;package. Make executable repeated calls bare except GPT2TENSOR:COPY-NAME? because active SAFET exports the same tail, and SAFET-MAP:LIVE plus WSTORE:LIVE because those two active imports export the same tail. Keep one-off MAKI-DTYPE:DF32, every type/effect name, MATCH selector, comment, string, declaration, call order, and result unchanged. Dependency: accepted loader rename ae1f14633232efba954e04e76f82b3b73e9e56be. Checkpoint: exact fixture and owning test loads are green; token-aware census proves the repeated calls and named collisions; a representative import passes the package gate. Files: maki/infer/gpt2-checkpoint-fixture.f only. Forbidden: API or behavior changes, aliases, package reopen, candidate/string edits, metadata, other files, or qualifier removal at a collision. Acceptance: prepare, mapped, and copy suite loads plus exact-diff typed-local and package gates pass; scopes are balanced and no executable repeated qualifier remains except the three named collision sets.

Claim: agent=gpt2-fixture-using workspace=.jj-ws/habu-scope-gpt-2-0e4403ea
