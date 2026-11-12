---
title: Refuse a target word at top level and in a host body, cross-builds first
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T10:43:46.165301+02:00"
closed-at: "2026-11-12T16:33:49.168337+02:00"
---

Problem: in a cross-build top level runs program words on the host, which is why every definition is compiled twice; a target word run there writes host memory through target addresses (the parent's cases). Acceptance: in a cross-build a target word named at top level or in a host word's body is refused by name, E-TARGET-WORD, before it runs; tick names it without calling it and executing that token on the host is the same refusal; host words, a host definer's does> children, data words and hb's declaration and loader words run as before; the parent's cases are refused. Depends: the section words child. Claim: unassigned.
