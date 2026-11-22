---
title: "SOURCE:READ-STDIN-ALL appends to a vector"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T11:05:31.496742+01:00"
---

Problem: stuart (Tender) 2026-11-22, /tmp/stuart-stdin-vector.md: SOURCE:READ-STDIN-ALL ( ^u8 len -- len ) (src/lib/source.f:45) fills a fixed span and throws E-FS-CAPACITY past it, against Joel's rule (2026-11-19) that bytes a word fills are a VEC:vector<u8>; Tender's scripts/setting.f keeps a cap for it. Acceptance: READ-STDIN-ALL ( ^VEC:vector<u8> -- ) appends all of standard input, its contract stating that growth leaves earlier blocks unwiped (a caller reading a secret reserves first); no module state; callers in Habu moved and Tender noticed. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-stdin.
