---
title: Complete the comparison and bitwise vocabulary
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-02T09:45:18.986984+02:00\""
---

Survey-ranked cheapest wins (habu hot-word survey 2026-08-02): the chain has < <= = and no other comparison; real code uses 0= (1047), <> (538), > (438), >= (299) - each is an existing flag/cmpbr condition inverted or swapped, near-zero cost. Plus and (1995) / or (899) / xor (18) / lshift (69) / rshift (44) / invert (3) as plain ALU forms (rshift/xor gate the checker's hottest words TAG and PAY and the SHA-256 core), cells as a const-op (8 *) exactly like 1+ (1233 uses), and 2drop as a rename row (276). All are word-model rows + selection entries + encoders that already exist in asm.f (verify AND/ORR/EOR/LSL/LSR encoders; add missing ones in asm.f style pinned in insn tests). Acceptance: each word compiles and executes identically to the engine on a targeted fixture; TAG (checker.f:152, '7 and') and WS? (json-read.f:252, or-chain) compile end to end as the survey's exemplars. Do after the call leaf lands (same files).

Claim: agent=intrinlane workspace=.jj-ws/habu-complete-the-comparison-63760034
