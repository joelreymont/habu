---
title: "Host-lint: enforce launcher-only shell policy"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T14:04:11.637660+02:00"
---

Problem: tools/host-lint.f:40-49,95-103 rejects only .py paths, python/.py tokens inside .sh, and the retired ./tools/seed.sh; the BLOCKING Habu-Only rule (shell allowed only as 'exec bin/hb tool.f "$@"' launcher, sole exception tools/bootstrap.sh) is otherwise unenforced. Already violated: maki/examples/nanogpt/fetch-gpt2-model.sh and fetch-gpt2-vocab.sh are full bash (curl, sha256sum, functions, conditionals) and pass host-lint with 0 findings. Expected fix: reject by policy — any .py/.pl/.js/.mjs/.rb/.awk path; any .sh whose non-comment body is not exactly one 'exec bin/hb ... "$@"' line, except an explicit tracked allowlist (tools/bootstrap.sh) whose entries must exist on disk (stale-allowlist check like filemap exclusions). Add the two fetch scripts to the tracked-debt allowlist ONLY with a blocker reference to the fetch-capability dot, or leave them failing until that dot lands (prefer failing: they are new code, not legacy). Acceptance: negative fixtures: real-logic .sh -> finding; exec-launcher .sh -> clean; allowlisted-but-missing path -> finding; comment/string containing 'python' does not false-positive. Files: tools/host-lint.f, fixtures. Verify: bin/hb --load tools/host-lint.f on repo (expect 2 findings until fetch scripts are ported/allowlisted); fixture suite. Depends: none. Ownership: host-lint policy. Claim: agent=claude workspace=.jj-ws/habu-host-lint-enforce-bda1bb33 (RELEASED 2026-08-21: workspace gone, no live lane - gc).

Checkpoint decision (orchestrator, 2026-07-22): Path A approved — split tools/host-lint-core.f (package HOST-LINT, predicates + parameterized walk, no auto-run) from tools/host-lint.f (auto-run entry, stays red on this tree); host-lint-test.f requires core only. Surface expanded: + tools/checked-boundary-lint-test-lib.f:226 (core inventory line). Rationale: matches the repo-wide -core.f convention, removes the packageless HOST-* stems, fixtures suite green on the delivered tree.
