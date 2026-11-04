---
title: "Read OpenAI's error event nested or flat, log its body"
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T19:11:45.707199+02:00"
---

Lane E. ERROR-EVENT (src/lib/net/model.f) reads only a flat code and message; a live server_error event became E-HTTP with no message and its body was not logged. Read code and message flat or nested under error, and log the event's whole body on the failure line (Joel 2026-11-01 'Log the whole body'). Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-chat-error.md. Workspace: .jj-ws.noindex/carl-chat-error. Verify: model-test.f. Claim: agent=carl workspace=.jj-ws.noindex/carl-chat-error.
