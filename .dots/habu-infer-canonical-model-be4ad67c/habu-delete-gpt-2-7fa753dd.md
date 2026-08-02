---
title: Delete GPT-2 format identity
status: active
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:49.657617+02:00"
blocks:
  - habu-pin-gpt-2-cdb5cfe0
---

Why: GPT2TENSOR:FORMAT-ID has no product consumer once checkpoint identity belongs to GPT2PIN. Result: delete FORMAT-ID, its tests, and any duplicate digest constants; downstream fixtures consume GPT2PIN constants. Owner: obsolete GPT-2 format identity only. Production red: two packages claim checkpoint identity. Acceptance: FORMAT-ID does not resolve, all duplicate digest literals are gone, and GPT2PIN/GPT2TENSOR fixtures pass. Forbidden: replacement format tag, manifest, version, compatibility identifier, or lint. Smallest owning check: GPT2PIN and GPT2TENSOR focused tests.

Claim: agent=codex workspace=.jj-ws/gpt2-format-cut
