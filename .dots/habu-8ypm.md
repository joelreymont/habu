---
title: Implement block/return-from for non-local exit
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-04T22:10:42.103958+02:00"
closed-at: "2025-12-05T21:07:13.643472+02:00"
close-reason: ""
---

Critical for self-hosting. (block name ...) establishes named exit point, (return-from name value) exits to it. Many CL constructs desugar to block/return-from. Have catch/throw in runtime but not integrated.
