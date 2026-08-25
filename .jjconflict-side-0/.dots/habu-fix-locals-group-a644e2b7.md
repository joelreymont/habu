---
title: Fix locals group wedge inside create does definer
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T00:44:58.289253+02:00"
---

Compiler defect found during codegen-buffer consolidation (lib/codegen.f CB-HEADER, keeps a documented stack-shuffle workaround; remove it when this lands). Reproducer: a {: :} typed locals group inside the body of a create ... does> defining word wedges the definer - the load exits 75 at does> instead of compiling. Expected: typed locals work in definer bodies exactly as in ordinary colon definitions, or the checker rejects them there with a named diagnostic explaining the constraint. Investigate the does> compilation path's interaction with the locals frame (locals cleanup vs the create/does> split). Acceptance: minimal fixture of a definer with a locals group compiles and mints working children, or rejects with a named error; regression test either way; lib/codegen.f CB-HEADER, rewritten with locals once supported. Files: src/core/checker.f or the locals/does> compile path, lib/codegen.f, new regression test.
