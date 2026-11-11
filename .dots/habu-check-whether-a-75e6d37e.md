---
title: Check whether a stub that always throws stops its callers loading
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T22:48:08.090766+02:00"
---

Problem: reported by Zen's agent (phil), not reproduced and with no program kept: the checker refuses code after a call to a word that always throws, so a file cannot load while a stub it calls is ': W ... E-UNSUPPORTED throw ;'. Acceptance: a two-word program (a stub that only throws, and a caller with code after the call) either loads, and the dot closes with it as evidence, or is refused, and then the dot states the refusal's text and site and whether it is the intended rule (docs/forth.md) with a remedy the message names. Files: none until the program decides. Verify: first, write and load that program under bin/hb; ask phil for Zen's program if it loads. Depends: none. Ownership: unassigned. Claim: unassigned. Consumers: met by Zen once, worked around.

Checked on the installed hb (engine of master 4255709c): refused. Programs ~/.cache/tmp/jerry-stub/s1.f, s2.f, s3.f: `: W ( n -- n ) drop E-UNSUPPORTED throw ;  : C ( n -- n ) W 1 + ;` exits 70 with `habu: in c: at '1' after 'W'; suggestion: Remove tokens after the terminating control word, or move the work before it.` at the first token after the call; the same for a stub that keeps its input and for `( -- n )`. It is the intended rule: docs/forth.md 'A word that never returns ends its branch' and docs/forth-card.md section 3 (E-DEAD-CODE). What is left: the text names neither E-DEAD-CODE (dot habu-print-a-reason-360222b1) nor why W ends the branch, and W is no control word, so the reader of a stub's caller is told to remove correct code. Acceptance now: the refusal says that W never returns because its body always ends in throw or die, at the same token; the card row quotes the new text. Files: src/core/render.f (DIAG-PROSE), docs/forth-card.md. Verify: a load-reject-diag-test.f case on the two-word program. No lane: Zen worked around it; do it with 360222b1.
