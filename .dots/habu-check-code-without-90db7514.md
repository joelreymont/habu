---
title: Check code without running it
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:23:03.852521+03:00"
---

After the 2026-10-27 demo (Joel 2026-10-23). The checker compiles the subject in a short-lived child process (tools/check-verify-core.f:17-30) because Habu's declarations are produced by running code: definers such as STRUCTURE/FIELD, TYPED-BUFFER, DYNAMIC-BUFFER and CMD:COMMAND build source text and evaluate it (docs/forth.md:62-68), libraries may write their own definers, and parsing words and evaluate make reading itself depend on earlier code. A type checker should not execute the code it checks. Direction discussed with Joel: definers become declarative macros (pattern and template, name concatenation, repetition for field lists, no arbitrary code); constants are restricted to pure constant expressions; anything needing real computation becomes a build step that writes ordinary source. The checker then expands and checks with no execution, removing the verifier child, its 12-lane pool (VFY-LANES, :1521) and the per-lane sink ladder (:1698-1710). First step: a design (Fable, or Codex Astra while Fable is out) with a census of every definer in habu, ../zen and ../tender showing each fits a template, and what the language server loses or keeps.
