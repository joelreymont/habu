---
title: Preflight completion request
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:54.254847+02:00"
blocks:
  - habu-read-completion-req-fa2d50f5
  - habu-infer-serve-openai-1dca13cd
  - habu-render-completion-json-9fff2d34
---

Why: all output, JSON, and HTTP capacities must be proven before inference changes scheduler state. Result: SERVE-CONN:PREFLIGHT consumes a body-ready connection, runs OPENAI-COMP:DECODE with its prompt storage and INFER:info, computes out_need = max_tokens times info.max-token-bytes, json_need = OPENAI-COMP:RESPONSE-BOUND(info.name-len,out_need), and wire_need = HTTP-COMP:RESPONSE-BOUND(json_need) with checked arithmetic, and proves exact fit in token-output, JSON scratch, stable response, and HTTP write spans. It returns ready(connection,request) or refused(connection,request-error) with scheduler state untouched. Owner: request decode and complete response-capacity admission only. Dependencies: bounded read, strict OpenAI decode, JSON response bound, and HTTP response bound. Production red: escaping or decimal growth can discover a short buffer only after inference. Acceptance: six-byte escaping, every decimal-width boundary, arithmetic overflow, exact fit, and one byte short in each span refuse or succeed before MATCH-ID, SUBMIT, OPEN-SEQ, or writer mutation. Forbidden: scheduler identity, submit, socket I/O, render, allocation, retry, compatibility, metric, or lint. Smallest owning check: real decoded requests through maki/serve/connection-admission-test.f.

Claim: unassigned.
