---
title: "Bind native-runtime.f's capture producers to it"
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T07:55:17.958394+02:00"
---

During src/habu/native-runtime.f's frame the bound capture producers are still the host engine's: BIND-PRODUCER runs before the engine build rewinds and FILE-SUCCESS rebinds only on success, so the owner spans its object encodes are the host's. Also LOAD-ACTION refuses on every recorded write before EARLIER-SLOTS filters owned spans. Acceptance: the frame's owner spans are the built engine's, proven by a census of native-runtime.f's object; an owned write at a load action is filtered before refusal. Workspace: none yet. Claim: unassigned.
