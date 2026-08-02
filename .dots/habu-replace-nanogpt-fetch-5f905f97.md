---
title: Replace nanogpt fetch scripts with checked Habu fetch tool
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-22T14:04:11.643513+02:00\""
---

Problem: `maki/examples/nanogpt/fetch-gpt2-model.sh` and
`fetch-gpt2-vocab.sh` contain curl, hashing, and control-flow logic that
violates Habu-Only.

Result: add a checked Habu fetch tool in the nanogpt path. Use `lib/process`
to drive curl through one named, tested process boundary and verify SHA-256
through the existing hash surface; raw sockets are not required. Network and
digest failures throw distinct named codes. Delete the shell scripts or reduce
them to logic-free `exec bin/hb ...` launchers. Any required source `TRUST`
stays at the process boundary with only its source-local rationale, retirement
owner, and focused production test.

Acceptance: both pinned artifacts fetch successfully; a local fixture proves
the normal, network-failure, and corrupted-download paths; no partial or
digest-mismatched file publishes; host-lint needs no allowlist for these files.
Files: the checked fetch tool, focused fixtures, and the two existing shell
paths. Run the local fixture, an authorized pinned-URL smoke, host-lint, and the
unchanged Maki gate. Ownership: nanogpt fetch path.

Claim: agent=claude workspace=.jj-ws/habu-replace-nanogpt-fetch-5f905f97.
