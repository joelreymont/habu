---
title: Answer the depth at a picked pixel
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T20:25:12.758696+02:00"
blocks:
  - habu-answer-which-instance-90e6af4b
---

Problem (Maki, bob 2026-11-12): the view needs the grabbed point to turn a joint, and picked answers only the instance and face. Acceptance: event picked also answers the depth at the pixel, in its unused c; the headless Chrome peer clicks a known pixel and reads its depth. design /Users/joel/Work/maki/.out/audit/design-arm.md section 3; first caller Maki view dot maki-turn-a-joint-ded3c7f5. Depends: habu-answer-which-instance-90e6af4b. Claim: agent=carl workspace=.jj-ws.noindex/carl-pick.
