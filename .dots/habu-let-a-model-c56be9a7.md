---
title: Let a model call run without a total bound
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T09:39:18.878362+02:00"
---

Joel 2026-11-01 (LESSONS 'long model calls'): MODEL:CHAT speaks OpenAI Responses in background mode with store=false, polls GET /responses/{id} every POLL-MS under TASK:STOP-FOR, ends an exchange only by a CURL:LOW-SPEED! stall test, cancels with POST /responses/{id}/cancel; TIMEOUT!, DEFAULT-MS, E-DEADLINE go; FINISH-REASON$ becomes INCOMPLETE-REASON$; usage keys and words take input_tokens/output_tokens in one cutover. Design: .jj-ws.noindex/carl-dots/.out/carl-file/model-background-design.md. Lanes: A1 drop the total bound; A2 Responses background mode; D usage names. Tender (stuart) and Zen (phil) drop their bounds and readers after the install. Claim: carl.
