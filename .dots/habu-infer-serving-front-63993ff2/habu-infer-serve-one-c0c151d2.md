---
title: Serve completions from one command
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.376529+02:00"
blocks:
  - habu-drive-completion-cmd-b4ca1b5d
---

Why: the engine is not a usable vLLM replacement until one native process retains an authenticated model and serves more than one exact completion request.

Result: close this umbrella only after `habu-start-completion-runtime-412475f9` and `habu-drive-completion-cmd-b4ca1b5d` land, their real DGX Spark commands pass on the exact integrated tree, the obsolete HTTP/socket/JSON/scheduler/concurrency serving dots are retired, and no duplicate generation loop or transport scaffold remains. The shipped command is `bin/hb --load tools/gpt2-serve.f -- <model-root>` with the frozen bounded binary stdin/stdout protocol. Owner: integrated persistent GPT-2 serving outcome only. Acceptance: two exact `Hello` completions separated by a refused request run through one authenticated child, output matches the pinned bytes, the model is closed once at EOF, measured generation remains the established decode path rather than a second implementation, master and origin are exact and green, and merged workspaces are removed. Forbidden: additional implementation, HTTP, socket, JSON, scheduler, concurrency, compatibility, ABI/version, framework, manifest, lint, suite, or retained superseded serving dot. Smallest owning check: the exact integrated `maki/infer/gpt2-serve-device-test.f` command followed by the required native gates.

Claim: unassigned.
