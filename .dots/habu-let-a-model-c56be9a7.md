---
title: Let a model call run without a total bound
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T09:39:18.878362+02:00"
blocks:
  - habu-clear-model-s-4b6bc3dc
---

Joel 2026-11-01 (LESSONS 'long model calls'): MODEL:CHAT speaks OpenAI Responses in background mode with store=false, streams the background response (stream:true, SSE) and resumes a dropped stream with GET /responses/{id}?stream=true&starting_after=<seq> (Joel 2026-11-01 "Stream it", replacing the 2 s poll), ends an exchange only by a CURL:LOW-SPEED! stall test, cancels with POST /responses/{id}/cancel; TIMEOUT!, DEFAULT-MS, E-DEADLINE go; FINISH-REASON$ becomes INCOMPLETE-REASON$; usage keys and words take input_tokens/output_tokens in one cutover. Design: .jj-ws.noindex/carl-dots/.out/carl-file/model-background-design.md. Lanes: A1 drop the total bound; A2 Responses background mode; D usage names. Tender (stuart) and Zen (phil) drop their bounds and readers after the install. Claim: carl.

Status 2026-11-01: A1, A2a, A2b, A2c (poll), D done in workspace carl-model; lane A2c-stream (brief .out/carl-file/brief-model-stream.md) running in carl-model, folds into A2c at landing. Then Opus lane review, Fable feature review, land with an install.
