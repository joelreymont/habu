---
title: Replace nanogpt fetch scripts with checked Habu fetch tool
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-22T14:04:11.643513+02:00\""
---

Problem: maki/examples/nanogpt/fetch-gpt2-model.sh and fetch-gpt2-vocab.sh are real bash logic (curl + sha256sum + control flow) violating Habu-Only; no Habu capability exists for pinned-hash HTTP fetch. Expected: a checked Habu tool (e.g. tools/fetch.f or maki/examples/nanogpt/fetch.f) using lib/process to drive curl as a named tested process boundary (spawn-curl boundary is acceptable; raw sockets are not required), verifying sha256 via the existing content-key/hash surface, with named throw codes for network/hash mismatch; the two .sh files reduce to 'exec bin/hb <tool>.f "$@"' launchers or are deleted. Acceptance: fetch of both artifacts with pinned hashes succeeds; corrupted-download fixture -> named throw; host-lint clean without allowlist entries for these files. Files: new fetch tool, the two .sh files, TRUSTED.md if a process boundary row is needed. Verify: run the tool against the pinned URLs (or a local file:// fixture for CI), host-lint, maki/test.f untouched. Depends: habu-host-lint policy dot (verify ordering only). Ownership: nanogpt fetch path. Claim: agent=claude workspace=.jj-ws/habu-replace-nanogpt-fetch-5f905f97.
