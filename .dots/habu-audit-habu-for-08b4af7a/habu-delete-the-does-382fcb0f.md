---
title: Delete the does> glue nothing reads
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T17:57:04.307152+01:00"
---

Problem: src/compiler/native/dict.f DOES-GLUE's answers go only into definition.f's M-DOES-GIN and M-DOES-GOUT, which nothing reads; SPELL-GLUE has no caller; RS-DOES-IN and RS-DOES-OUT serve only them; dict.f's comment says the rows come from the owner's does> group, which is gone. Acceptance: DOES-GLUE, SPELL-GLUE, RS-DOES-IN, RS-DOES-OUT, their RG-SLOT branches, M-DOES-GIN, M-DOES-GOUT and the elaborator-refusal lines in definition.f are deleted after rg finds no other reader, and the engine builds. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-glue.
