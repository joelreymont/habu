---
title: Correct boot census exec classification
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T11:20:33.220160+03:00"
closed-at: "2026-10-21T11:45:17.976976+03:00"
close-reason: Count observed macOS exec generations once using proc identity version; retain same-image reexecs and explicitly incomplete moving or missing-parent observations. Independent Astra follow-up clear. Existing sleep/fork checks, same-image reexec E2E, delayed observation stress and parent recovery probes pass; final native gate 486/486. Unobserved coalesced execs are not reconstructed.
---

Full native gate on3782448f/948295de reproducibly observed boot-census tree fixture count2 /bin/sleep instead of exactly1, with total fresh exec3 and fork1, lost0. Existing docs allow total exec at least2 for preexec observations but one actual sleep exec must not be double-counted. Reproduce under load, distinguish sampling preexec state versus exec transition, and fix responsible census classification if incorrect; do not merely weaken the assertion or add a timeout. Public Habu tool only, retain exact process-tree behavior and exit status. Existing red boot-census row is acceptance; independent review.
