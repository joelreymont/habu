---
title: Cut comments to facts the code cannot state
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:15.455343+02:00"
---

Problem: Joel 2026-10-29: comments that restate their code rot with it (three T0 comments named a deleted word) and cost the next agent reading time; a comment earns its place only when the next agent would otherwise spend tool calls learning what it says. Campaign over the whole tree, one child per directory, each a comment-only commit (code lines byte-identical) reviewed by a code-reviewer against the rule below and landed with or after the T0 line. Rule: delete a comment that restates the code or narrates history; keep, cut to the fact, one that states what the code cannot (a contract between files, a refusal and its reason, a measured number, an architecture constraint, where the next thing lives); a header is a two-to-six-line map. No comment names a word that does not exist in the tree. Acceptance: every child closed. Files: src, tools, test (docs are not comments). Verify: per child, the comment-only diff filter prints nothing and the names check finds every named word. Depends: none. Ownership: the whole tree's comments, by directory. Claim: unassigned.

Fan-out 2026-10-29: src/core, src/lib, src/arch, tools and test run in parallel, one worker each in .jj-ws.noindex/jerry-t0c-<name> (children claimed); src/habu continues in jerry-t0-comments (WIP tvzpwmwt, 51 files done at the last handoff); batch 2's remaining one-line restatements run in jerry-t0c-comp6 (xnkkosyp, squashed into txpzkrxl afterwards). Batch 1 (ypyowpqq, T0 files) and batch 2 (txpzkrxl, src/compiler) are reviewed with fold-ins applied. Findings parked in their own dots: the STEP 1023-byte cap (src/habu/stepper.f:90) and NFEED:OBSERVE's visibility.

2026-10-30 00:20: batches 1 and 2 landed as master 90da7f0c (T0 files) and 18f0605d (src/compiler, with the comp6 fold-in cuts), both reviewed LAND, code byte-identical. The remaining directories are split by file list across workspaces so that workers run in parallel; each side commit is squashed into its directory's commit before review, then rebased onto master, reviewed and landed as one commit per directory. check.f proofs now run on g3.

Status 2026-11-02 (census against master 430a53e317): every directory child is closed (for example 83f95498 'Cut src/habu comments to facts'); only habu-cut-the-section-17e50782 remains (banner lines in src/habu/t0*.f and about 1475 more in src/compiler, src/lib and test). The jerry claim's workspace is retired.

Acceptance case 2026-11-10 (bob; merged from habu-state-rules-in-12b5dd92 "State rules in comments, not history", one cause with this dot): Problem: source comments break the comment policy in docs/forth.md (Comments). Audit of src, lib, tools and test (110,360 comment lines in 1,687 files): history narrative ~820 lines (e.g. tools/native-build-core.f:44-60, src/core/checker.f:10260-10283); dates, dot ids and names ~975; essays of 8+ lines 2,651 blocks / 46,777 lines (checker.f 3,916, habu2.f 2,004, layout.f 1,267; tools/lsp-test-lib.f:1-429 is one header); restating the code 240+; the same fact in several places 429 groups; stale names or paths ~357.
Ruling (Joel, 2026-10-08): rule only, cut history; the policy in docs/forth.md governs.
Placement: history, dates, ids and arguments for or against a design are deleted, not moved. Design text a reader of one file needs stays in that file's header; only a rule spanning files goes to the existing docs page for its subject, once. Text describing machinery an open dot deletes (PRIM:/PPRIM: rows, marks, the checker's own symbol table) is deleted, not moved.
Acceptance: every comment states the current rule and, where needed, the fact that proves it; no history, dates, dot ids, names, essays, restated code or duplicated facts remain; no code change.
Files: src, lib, tools, test (*.f). One worker per slice, run by package.
Verify: the suite on the built hb (comments only); rg for the history markers returns nothing.
Depends: none. Ownership: unassigned.
