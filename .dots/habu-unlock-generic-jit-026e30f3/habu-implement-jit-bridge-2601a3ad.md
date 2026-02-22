---
title: Implement JIT bridge non-local-exit trampoline
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-22T20:57:18.857200+01:00\\\"\""
closed-at: "2026-02-22T21:06:07.183053+01:00"
close-reason: Use C trampoline for true JIT bridge unwind
---

src/interp/vm.zig bridge call path + src/jit/backend.zig runtime bridge state: replace post-call bridge error lane with true abort/unwind of active JIT frame when bridge call raises VM error. Add C-ABI jump trampoline helpers under src/jit. Update backend API/stub. Add integration regression proving bridge throw exits JIT frame immediately and preserves existing recursive JIT tests. Depends on habu-relay-jit-bridge-535dce04. Est: 2-3h
