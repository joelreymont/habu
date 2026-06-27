---
title: "Inference: add the show-inferred form"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T08:06:44.263451+02:00"
---

Gap #20 (minor). Local type inference is built+sound, but the {: x:? :} show-inferred form (inference.md, explicitly a proposal not implemented) is missing - it binds x and prints the inferred type+tokens so an author/LLM can ask what did you infer here mid-kernel without committing an annotation. Protects diagnostics (inference.md rule 3).
- Files: src/core/checker.f (the {: x:? :} local form), render the inferred term; needs a checker change + fixpoint rebuild (recoverable via ../habu/bin/hb).
- Verify: {: x:? :} prints the inferred type for a span/tile binding; a wrong downstream use still rejects with a local anchor.
- Dep: none (inference done).
