---
title: Run the orphaned LSP tests in the gate
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T20:56:23.273954+02:00"
---

Problem: tools/lsp-completion-test.f and tools/lsp-sumtype-test.f are in no test/gate-stdlib-cases.f SUITE (lsp-boundary at :34 runs lsp-test.f and lsp-entry-test.f; lsp-batch at :434 runs lsp-batch-test.f) and no file requires them, so the gate never runs them (found by tim's review; both pass run directly on tim-runexit). Acceptance: both run in the gate, in the existing LSP suite whose loaded state they share when compatible (one loaded tool per CLAUDE.md), else their own SUITE; measure the suite's wall and CPU before and after. Files: test/gate-stdlib-cases.f. Verify: bin/hb --load each file (rc 0), then the full suite. Depends: none. Ownership: test/gate-stdlib-cases.f. Claim: carl, workspace .jj-ws.noindex/carl-lspdup. Finding: both files are redundant, not orphaned coverage. LSP-TEST:TEST (gated in lsp-boundary) runs TEST-COMPLETIONS in DEFAULT-CASES and the three sumtype turns in TEST-DEFINITIONS and TEST-RENAMES; LSP-TEST:TEST-BUILTIN had no caller. Resolution: delete both files and TEST-COMPLETION, TEST-SUMTYPE and TEST-BUILTIN (change "Remove LSP runners the gated suite repeats"); verify tools/lsp-test.f rc 0.
