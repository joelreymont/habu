---
title: Keep meshes on the GPU and draw them instanced
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T10:49:31.135626+02:00"
---

Problem (Maki, tim 2026-11-12): a draw record is every triangle's corners (src/lib/browser/turn.js:544) and viewport.js:137-167 writes the whole corner buffer and a new flags array to the GPU on every draw: 15.2 MB per draw on Maki's crossmember, past wasm32's 4 GiB at 1000x. Acceptance: turn records mesh (13: u32 vertex and triangle counts, vertices of four u16, triangles of three u16, a module-chosen handle), unmesh (14), draw (2) as 64-byte instance rows (twelve f32 transform, u32 mesh handle, id, flags, highlight offset) with the matrix, highlight (6) as u32 face-bit words; Habu's test/browser display.f and navigation.f moved to them; the headless Chrome peer draws one mesh twice and reads the pixels. First caller: Maki viewer.f (design /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/design-view.md section 4). Depends: none. Claim: unassigned.
