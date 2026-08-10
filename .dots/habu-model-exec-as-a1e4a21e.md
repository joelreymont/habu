---
title: "Model execute as a call to the engine's own execute"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:44:39.245409+02:00"
---

S3 of the quotations design: compiled execute is ALWAYS bl <BEXEC entry> in the engine too (C-CALL-REJECT-UNSAFE refuses inlining BLR bodies), and NDICT:CALL-TARGET resolves 'execute' today (probed; arity unexported, -1 -1). Model it as hir.wordcall to that entry with SITE-SUPPLIED arity: in = 1 + the quotation's din cells, out = its dout cells, read off the certified quotation effect (the export dot). No NCLOB row exists for execute and never will - KEEP-N keeps nothing, the caller pays full save discipline, which is exactly right for an unknown callee. Refuse by name any quotation that is not EFFECT-QUOT-SIMPLE?. NO new instruction rows. Guessing arity from the site's stack shape is FORBIDDEN (value heuristic where the certified effect is the structural fact). Acceptance: APPLY migrates and computes; emission decoded - one bl to execute's entry; the real multishot site lib/array.f A-MAPI! migrates and runs; throw-edge/return-stack quotations refused by name; census execute bucket falls from 20. Files: src/compiler/native/{elaborate,dict}.f. Depends: habu-compile-a-quotation-04341c80, habu-export-a-certified-f5a7561d.
