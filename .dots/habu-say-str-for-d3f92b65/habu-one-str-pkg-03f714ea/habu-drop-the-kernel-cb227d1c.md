---
title: "Drop the kernel's copy of STR's comparisons"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T16:53:15.173720+01:00"
---

Problem: src/core/util.f:34-46 CORE-STR=, CORE-STR=CI and CORE-FOLD-C duplicate string.f's STR:=, STR:=CI and STR:ASCII-LOWER (315 and 176 call sites in src, tools, test); they exist because util.f loads in the CHECKER group (src/habu/boot-files.f:47) and string.f in RESTLIB (:126). Joel 2026-11-23: one string package, no duplicates. Copies of the same comparisons also sit in src/core/roles.f (VRDEF-*), src/core/xref.f (XREF-*) and src/core/checker.f (SYM-*). Approach (Joel 2026-11-24, interview): string.f moves to the front of the kernel (a CHECKER row after pointer-storage.f) with every STR word that needs nothing later; INDEX-OF, PARSE-POS, PARSE-NEG, >NUMBER?, HEX>NUMBER?, FIND-SUB-FROM, FIND-SUB, CONTAINS? and the SB/BUF builders move to src/lib/string-option.f, package STR, at string.f's old row, because option<a> needs ENUM, which needs the folded comparison; no word changes name or effect. Acceptance: string.f's comparisons load where the kernel needs them; CORE-STR=, CORE-STR=CI, CORE-FOLD-C and the VRDEF-, XREF- and SYM- copies are deleted and every caller uses STR's words; engine converges. Depends: none. Claim: unassigned.
