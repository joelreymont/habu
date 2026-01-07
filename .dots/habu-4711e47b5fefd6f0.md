---
title: "Hash table correctness: avoid EMPTY/DELETED sentinel collisions with valid Values, enforce power-of-two capacity or change probe math, and error/resize instead of silent fail; files src/runtime/objects.zig:138-155, src/runtime/heap.zig:266-289, src/interp/vm.zig:2386-2438."
status: closed
priority: 2
issue-type: task
created-at: "2025-12-29T09:07:13.570831+02:00"
closed-at: "2025-12-29T10:10:35.340278+02:00"
close-reason: "Fixed: sentinels use impossible chars, capacity is power-of-two, resize on failure"
---
