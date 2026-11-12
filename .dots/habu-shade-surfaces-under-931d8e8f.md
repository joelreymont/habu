---
title: Shade surfaces under a studio light
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:25:12.733112+02:00"
---

Problem (Maki, bob 2026-11-12): Joel chose real-time studio shading for a photorealistic arm; the adapter lights faces by one fixed direction. Acceptance: metallic-roughness surfaces lit by one studio environment generated in the shader (the view's CSP allows no fetch) plus a key light, tone-mapped to sRGB, with 4x multisampling; the headless Chrome peer reads the pixels of a lit instance. design /Users/joel/Work/maki/.out/audit/design-arm.md section 3; first caller Maki view dot maki-turn-a-joint-ded3c7f5. Depends: habu-paint-each-drawn-cfce5e99. Claim: unassigned.
