---
title: Define compiler dialect schemas
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:58.856544+02:00"
blocks:
  - habu-intern-compiler-attrs-37cfbca5
  - habu-extend-compiler-ir-e9b76351
---

Full context: design sections 5.3 and 6.4 require closed-world opcode/data schemas before generic operations can exist. Define schema records for opcode, operands, results, successors, regions, attributes, effects, target legality, and terminator rules; dialect packages remain separate. Acceptance: duplicate/unknown opcode, incomplete cases, illegal arity/effect/target combinations, and digest mismatch reject exhaustively. Dependency: attributes.
