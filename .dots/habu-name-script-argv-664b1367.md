---
title: Name script-argv.f once in the LSP test library
status: open
priority: 3
issue-type: task
created-at: "2026-10-16T08:34:57.404387+03:00"
---

Problem: tools/lsp-test-lib.f:2886 SCRIPT-ARGV-PATH and :5836 BI-SOURCE-PATH both answer s" src/os/script-argv.f" IN-TREE. Acceptance: one word remains and every use calls it; the rows that load tools/lsp-test-lib.f pass. Files: tools/lsp-test-lib.f. Verify: those LSP SUITE rows. Depends: none. Ownership: tools/lsp-test-lib.f. Claim: unassigned.
