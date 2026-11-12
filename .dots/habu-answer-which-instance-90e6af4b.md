---
title: Answer which instance and face a pixel shows
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T10:49:31.147141+02:00"
---

Problem (Maki, tim 2026-11-12): picking runs on the CPU over every triangle (Maki viewer.f:878), which needs the whole model in wasm. Acceptance: turn record pick (15: x, y, id) answered by event picked (18: instance id or -1, the vertex's fourth u16, the id); the headless Chrome peer clicks a known pixel. First caller: Maki viewer.f (design /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/design-view.md section 4). Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-pick.
