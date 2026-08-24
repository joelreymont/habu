---
title: Delete redundant lint policy layers
status: closed
priority: 1
issue-type: task
created-at: "2026-08-24T02:21:40.127895+02:00"
closed-at: "2026-08-24T02:55:31.561206+02:00"
close-reason: "Landed 31e3a431: deleted duplicate-definition, process-primitive, and strict-signature policy layers; preserved native/checker/process/AOT behavior and passed the exact full gate."
---

Delete duplicate-definition lint, process-primitive lint, and optional strict-signature mode plus fixtures, registrations, and stale docs; preserve engine duplicate rejection, process wrappers and real tests, declared effect verification, and the AOT lint path.
