---
title: Comments and compilation.md cite words and line numbers that do not exist
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T21:16:49.034428+02:00"
---

Problem (found by the twins-2 review, already wrong on master 3aa0cfbb): src/habu/boot-files.f :2-4 says habu2.f walks the list, but habu2.f only requires it (:17) and calls no BOOT-FILES word; the walker is tools/hb-build-core.f STARTUP. docs/compilation.md :17 cites habu2.f EMIT-HOST-LOAD-PREFIX, which exists nowhere in src or tools. docs/compilation.md :38 points at src/core/check-hook.f:109, a doc passage tracking a code line. src/arch/arm64/icode.f :18 says it spells layout terms out by hand. Acceptance: each comment states what the code does without naming another file's word; the two doc passages are deleted or state a fact no code line can move. Files: src/habu/boot-files.f, docs/compilation.md, src/arch/arm64/icode.f. Verify: rg for each cited name finds its definition, or the citation is gone. Depends: none. Ownership: those passages. Claim: unassigned.

Also: docs/compilation.md "One reader", **Now:** ("The engine's reader compiles it through the tier-0 JIT as it reads") describes the machine-code compiler that dot habu-delete-the-jit-d9fdaf67's row-walk stack deletes; once that lands the passage states what no code does.
