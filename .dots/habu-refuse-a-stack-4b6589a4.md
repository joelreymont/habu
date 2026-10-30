---
title: Refuse a stack effect written after the locals group
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T03:11:57.385958+02:00"
---

Problem: the checker reads a definition's ( … -- … ) effect only when it directly follows the name; written after the entry {: … :} group it is an ordinary comment and the body is never checked against it: ': WA {: a:n :} ( n -- bool ) a ;' certifies (rc 0) while ': WB ( n -- bool ) {: a:n :} a ;' is refused with E-MISMATCH (probe on the C-string stack engine, 2026-10-30, fold-in re-review). src/core/util.f:126 RD32 ': RD32 {: p:ptr :} ( ptr u8 -- n )' is a live case whose effect has never been read; PATH0 (util.f:120) was one until the C-string stack moved its effect first, after which its body passed. Acceptance: a definition whose entry group precedes an effect-shaped comment is refused at the definition with a named error that names the misplaced effect; RD32's effect moves before its group and certifies, or its body is fixed to the effect; a rejected-program row covers the refusal; docs/forth.md 'Rules learned by refusal' and docs/forth-card.md §2 state the rule. Files: src/core/checker.f (where the effect is captured at the colon), src/core/util.f RD32, a refusal row beside the other declaration refusals, docs/forth.md, docs/forth-card.md. Verify: the refusal row on the rebuilt engine; tools/check.f -- src/core/util.f; full suite (shared checker). Depends: none. Ownership: src/core/checker.f effect capture, src/core/util.f RD32. Claim: unassigned.
