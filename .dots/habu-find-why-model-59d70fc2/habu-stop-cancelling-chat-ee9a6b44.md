---
title: Stop cancelling chat responses at the provider
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T19:11:45.685094+02:00"
---

Lane C1 of the held-stream change (DECISIONS 'long model calls', Joel 2026-11-04 'Held stream'). OpenAI cancels only responses created with background=true (API reference, POST /responses/{id}/cancel), so a held response cannot be cancelled there; remove MODEL's provider cancel. Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-chat-c1.md. Workspace: .jj-ws.noindex/carl-chat-cancel. Verify: model-test.f, test-model-test.f. Claim: agent=carl workspace=.jj-ws.noindex/carl-chat-cancel.
