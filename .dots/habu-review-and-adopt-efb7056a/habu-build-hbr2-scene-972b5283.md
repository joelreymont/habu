---
title: Build HBR2 SCENE and RENDER for the viewport
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.946917+03:00"
---

Problem: Maki's viewport needs HBR2 §14-§16 (scene bundles, GPU leases, reliable picks); RENDER must not import Maki (HBR2 §1.2). Acceptance: lib/scene/, lib/render/, a WebGPU executor in host/browser/, HBR2 gate G3 (camera isolation, skipped-frame click, device loss) with the Maki viewport as caller. Files: lib/scene/ (new), lib/render/ (new), host/browser/gpu* (new). Verify: the G3 fixture; W29. Depends: habu-build-hbr2-browser-84328e34; a Maki viewport caller. Ownership: lib/scene/, lib/render/. Lane: tim, later. Claim: unassigned.
