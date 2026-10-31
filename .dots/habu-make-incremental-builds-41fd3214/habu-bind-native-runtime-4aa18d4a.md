---
title: "Bind native-runtime.f's capture producers to it"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T07:55:17.958394+02:00"
closed-at: "2026-10-31T20:52:55.045762+02:00"
close-reason: "Obsolete after lane 11: objects carry no owner spans (the top level re-runs); EARLIER-SLOTS 0 hits; native-runtime.f reused."
---

During src/habu/native-runtime.f's frame the bound capture producers are still the host engine's: BIND-PRODUCER runs before the engine build rewinds and FILE-SUCCESS rebinds only on success, so the owner spans its object encodes are the host's. Also LOAD-ACTION refuses on every recorded write before EARLIER-SLOTS filters owned spans. Acceptance: the frame's owner spans are the built engine's, proven by a census of native-runtime.f's object; an owned write at a load action is filtered before refusal. Workspace: none yet. Claim: unassigned.
