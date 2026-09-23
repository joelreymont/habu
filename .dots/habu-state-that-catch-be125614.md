---
title: State that catch restores the depth, not the values, on a throw
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T08:31:04.475394+03:00"
---

Measured by aspen (~/.cache/tender/habu-gaps/catch-stack-on-throw/probe.f) and reduced by hazel on 06c5c9e9: `7 [: CAUGHT ;] catch {: value:n code:n :}` over `: CAUGHT ( n -- n ) {: at:n :} at BOOM at ;` prints -9999 -9999. The engine is right: on the throwing branch `depth` is 2, the same as on the returning branch (Forth-2012 catch restores the depth at the catch and puts the code in the xt's slot), so the locals group binds the code to its own cell; the cell under it holds whatever the body last wrote there (E-NOPE, pushed before the throw), which the standard leaves unspecified and the checker's `n` cannot promise as the caller's value. Tender used that value as a typed-buffer index on the throwing branch and lost a job pool to E-LAYOUT-BOUNDS. docs/forth.md's catch paragraph ('unifies the live stack with its inputs and outputs') states the depth model only. Acceptance: docs/forth.md and the card's catch rule say that after a throw the depth is restored and the cells below the code are unspecified, so a value needed on both branches is kept under the code (`[: dup WORD ;]`, the documented shape); a fixture pins depth 2 and code -9999 on the throwing branch and does not read the cell below it. Files: docs/forth.md, docs/forth-card.md (23 bytes of headroom: merge, do not add), test/. Ownership: hazel. Claim: unassigned.
