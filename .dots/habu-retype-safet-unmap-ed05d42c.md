---
title: Retype SAFET unmap as total
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:25:12.193704+02:00"
blocks:
  - habu-make-owned-release-79de2b5c
---

Why: with MEM:RELEASE fatal-on-failure (habu-make-owned-release-79de2b5c), SAFET:UNMAP-MAPPING's result shape is a false failure model - its err arm can only be minted by a failure class that now terminates the process. Behavior: UNMAP-MAPPING drops the result and returns plain bytes-given-back n; callers that folded its code simplify; no catch around any release. Owner: SAFET package only. Dependencies: habu-make-owned-release-79de2b5c. Acceptance: safetensors suite green on the total surface with each retired err-arm assertion named in the report, not silently dropped; typed-local and package diff lints clean on the exact artifact; destruction review before merge. Real pre-change defect: the err arm is unreachable today except through the exact throw the flip reclassifies as fatal - a result nobody can mint is a lie in the signature.
