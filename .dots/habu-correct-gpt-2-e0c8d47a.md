---
title: Correct GPT-2 service limit proof
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T02:43:05.447968+02:00"
---

Why: the real GPT-2 service-device gate deterministically expects retired E-TOK-CAP after d76993365451 removed the 1024-byte tokenizer chunk ceiling, so exact master reports three false failures and cannot serve as the engine baseline. Behavior: keep the existing 4096-NUL, max=1 request through tools/gpt2-serve.f and public GPT2:GENERATE; it must pass framing and tokenization, fail the nctx fit check with E-LIMIT -5667, emit little-endian bytes $DD $E9, terminate, leave stdout empty, and report -5667 on stderr. Rename only the test helper and label from tokenizer failure to context-limit failure. Do not change production code, restore the removed ceiling, enlarge the service protocol, or add machinery. Dependencies: exact master 35c0be0fb6504034567e0c4b3fd49ac503965064. Owned result and package owner: maki/infer/gpt2-serve-device-test.f in GPT2-SERVE only. Checkpoint: bin/hb --load maki/infer/gpt2-serve-device-test.f -- /home/joel/Work/Habu/gpt2-model </dev/null exits 1 with failures 72, 73, 83: expected -5324 bytes and stderr but observes -5667. Acceptance: derive $DD $E9 from the response frame's little-endian I64 encoding of the named E-LIMIT constant, not from the observed failure; the same real-device command prints test: ok; bin/hb --load maki/infer/gpt2-generate-test.f -- /home/joel/Work/Habu/gpt2-model </dev/null remains green and retains exact 4096 acceptance plus 4097 refusal; typed-local and package checks accept the exact diff; no other file changes.

Claim: agent=codex-service-limit workspace=.jj-ws/habu-correct-gpt-2-e0c8d47a.
