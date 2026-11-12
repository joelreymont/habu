---
title: "Answer which boxes pass the last draw's depth"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T10:49:31.158286+02:00"
---

Problem (Maki, tim 2026-11-12): a viewer cannot learn that a part is hidden, so it loads the inside of an assembly while the person looks at the outside. Acceptance: turn record occlude (16: boxes of six f32, an id) answered by event occluded (19: one byte per box, 1 if any of it passed the last draw's depth, the id), by WebGPU occlusion queries; a box behind a drawn quad answers 0, beside it 1. First caller: Maki viewer.f (design /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/design-view.md section 4). Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-occlude.
