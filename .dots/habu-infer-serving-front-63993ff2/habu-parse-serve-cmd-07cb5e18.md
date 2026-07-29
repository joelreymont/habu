---
title: Parse serve command arguments
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:41:05.630389+02:00"
blocks:
  - habu-own-checked-ipv4-44f29beb
---

Why: command-line grammar is independent from model and socket startup. Interface: package SERVE-CMD owns immutable opts and PARSE-ARGV. It requires exactly one each of --model gpt2|qwen2.5-7b-instruct, --root PATH, --listen IPv4:PORT, --max-requests, --max-batch, --kv-tokens, --connections, --idle-ms, --body-bytes, --prompt-bytes, and --output-bytes; --once is the sole optional flag. body-bytes is each connection's decoded request-body span, prompt-bytes is its extracted prompt span, and output-bytes is its accumulated decoded-token span. The command derives the read span as the named HTTP header bound plus body-bytes, the JSON span from RESPONSE-BOUND(output-bytes), the write span as the named HTTP response-header bound plus that JSON span, poll rows from connections, and scheduler result rows/arena from SCHED:RESULT-CAP; none is another option. PARSE-ARGV returns parsed(opts) or refused(arg-error), preserves root spans for the command lifetime, and converts every number and numeric IPv4 address with checked arithmetic. Owner: exact serve argument grammar and options only. Dependency: checked numeric IPv4 address construction. Production red: tools/serve.f would otherwise mix argument parsing with three-subsystem startup. Acceptance: exact GPT-2 and Qwen invocations parse; each numeric option maps to the stated owner; missing, duplicate, unknown, malformed, empty, zero, overflow, unsupported model, and positional arguments reject before any file, model, or socket action; obsolete or invented buffer flags reject as unknown. Forbidden: unnamed capacity, default, environment variable, derived-storage option, filesystem access, model open, listener open, subsystem start, loop, documentation, version, or compatibility option. Smallest owning check: bin/hb --load tools/serve-args-test.f -- with the real parser entry.
