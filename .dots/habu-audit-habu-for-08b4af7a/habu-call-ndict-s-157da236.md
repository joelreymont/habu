---
title: "Call NDICT's targets without wrappers"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T17:57:04.324827+01:00"
closed-at: "2026-11-13T18:32:45.700723+01:00"
---

Problem: src/compiler/native/dict.f keeps one-word wrappers over owner and checker words (the block at ~476-483, OPERAND-CELLS, SPELL-DEAD?), the layer the tier-1 line removed from CHECKER-OWNER and NFAM. Acceptance: every caller calls the wrapped word and the wrappers are deleted; where the target is private, the lane stops and reports. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-ndict.
