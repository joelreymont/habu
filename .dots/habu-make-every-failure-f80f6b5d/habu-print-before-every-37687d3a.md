---
title: Print before every silent kernel exit
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T14:02:21.825720+02:00"
---

Problem: 36 arm64 kernel exits end the process with nothing on fd 2 (task guards 79, seal violations 83/84 and others): CB-DIE (src/habu/habu1.f:2184-2189), DEFWRITE:REFUSE-AT (src/habu/habu2.f:1079-1080), C-CRASH-GUARD-REPORT's body (src/habu/crash.f:285-288) and inline exits each write their own way or not at all. Acceptance: one arm64 kernel emitter writes `<class> habu: <text>[ <16 hex>] lr <16 hex>` through C-HEX-DIGITS (crash.f:103) and exits with the status; it replaces CB-DIE and the habu1.f inline exits (:226 :283 :309 :1595 :1625 :3173 :3202 :3234 :3298 :3323 :3346 :4338 :5279); NFETCH-CHECK:BAD-TAG prints `E-BAD-TAG habu: <path>:<line>: layout tag <n> outside 0..<limit-1>`, exit 85; map.f:118 prints before throwing 85. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-kdie.
