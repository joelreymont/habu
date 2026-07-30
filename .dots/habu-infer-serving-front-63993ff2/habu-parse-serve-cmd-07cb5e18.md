---
title: Parse serve command arguments
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:41:05.630389+02:00"
blocks:
  - habu-own-checked-ipv4-44f29beb
  - habu-own-completion-srv-2cffaf4b
---

Why: command-line grammar is independent from model and socket startup. Interface: package SERVE-CMD owns immutable opts and PARSE-ARGV. It requires exactly one each of --model gpt2|qwen2.5-7b-instruct, --root PATH, --listen IPv4:PORT, --max-requests, --max-batch, --kv-tokens, --connections, --idle-ms, --body-bytes, --prompt-bytes, and --output-bytes; --once is the sole optional flag. While handling each named server-capacity flag, PARSE-ARGV calls only its matching SERVE constructor and opts stores those four nominal values, never four interchangeable raw cells. The model option is only a closed arm selection; opts stores no public model name, valid count, token-byte bound, batch cap, response bound, scheduler result cap, or derived storage extent. PARSE-ARGV returns parsed(opts) or refused(arg-error), preserves root spans for the command lifetime, and converts every other number and numeric IPv4 address with checked arithmetic. Owner: exact serve argument grammar and named requested values only. Production red: tools/serve.f would otherwise mix syntax with subsystem startup and publish role-swappable server limits. Acceptance: exact GPT-2 and Qwen invocations parse; each typed limit reaches the real SERVE:PLAN role; swapping any two constructor calls fails the production parser-to-plan test; missing, duplicate, unknown, malformed, empty, zero, overflow, unsupported model, and positional arguments reject before any file, model, derived capacity, allocation, or socket action; obsolete or invented buffer flags reject as unknown. Forbidden: requested-limit type or constructor definition, model limit, derived capacity, unnamed default, environment variable, filesystem access, model open, listener open, subsystem start, loop, documentation, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load tools/serve-args-test.f -- with the real parser entry.
