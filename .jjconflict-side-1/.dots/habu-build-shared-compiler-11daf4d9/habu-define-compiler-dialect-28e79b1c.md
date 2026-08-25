---
title: Define compiler dialect schemas
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:58.856544+02:00"
closed-at: "2026-08-15T14:07:28.822083+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): dialect schemas (IR-SCHEMA, 18 named negatives). Production-consumed by the native chain; suites dual-registered, green through the real entry."
blocks:
  - habu-extend-compiler-ir-e9b76351
---

Full context: design sections 5.3 and 6.4 require closed-world opcode/data schemas before generic operations can exist. Define schema records for opcode, operands, results, successors, regions, attributes, effects, target legality, and terminator rules; dialect packages remain separate. Acceptance: duplicate/unknown opcode, incomplete cases, illegal arity/effect/target combinations, and digest mismatch reject exhaustively. Dependency: attributes.

Claim: agent=ir-schema workspace=.jj-ws/habu-define-compiler-dialect-28e79b1c
