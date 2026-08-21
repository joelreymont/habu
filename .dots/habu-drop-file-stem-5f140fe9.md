---
title: Drop file-stem prefix rule from package gate
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T20:19:22.988578+02:00"
---

Why: tools/package-diff-lint-core.f rejects a changed definition whose name repeats its file stem (E-REDUNDANT-FILE-PREFIX, OWNER-PREFIX? at line 616), but a source filename is not a caller-visible owner - matmul.f does not make MATMUL- redundant - so the rule is architecturally wrong and blocked the reviewed public MAKI:MATMUL-RIGHT-T; package-prefix rejection stays because MAKI:MAKI-* repeats the real qualified owner. Exact result: remove only the file-stem prefix rejection from tools/package-diff-lint-core.f and delete or replace its test expectation; package-prefix rejection, unowned-global rejection, and comment/string hostiles remain covered. Owner: package of tools/package-diff-lint-core.f in that file. Acceptance: a changed public MAKI:MATMUL-RIGHT-T in maki/matmul.f passes the gate; a changed unowned global still fails; package MAKI : MAKI-FOO still fails; hostile comment/string fixtures still fail; the focused lint suite and both diff lints green with the commit checked out; the former MATMUL-DX/MATMUL-DW findings are recorded as no longer violations, not rename debt. Forbidden: touching any other rule, allowlists, exceptions.

Claim: agent=claude workspace=.jj-ws/habu-drop-file-stem (RELEASED 2026-08-21: workspace gone, no live lane - gc)
