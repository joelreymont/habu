---
title: Paint each drawn instance
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:25:12.720020+02:00"
---

Problem (Maki, bob 2026-11-12): every instance draws one colour. Acceptance: the draw row grows from 64 to 72 bytes with a u32 sRGB colour and a u32 holding metallic and roughness as two u16; the mesh record's vertex grows to 12 bytes with the surface normal, octahedral in two i16; Habu's test/browser callers move to them; the headless Chrome peer draws two instances in two colours. design /Users/joel/Work/maki/.out/audit/design-arm.md section 3; first caller Maki view dot maki-turn-a-joint-ded3c7f5. Depends: none. Claim: unassigned.
