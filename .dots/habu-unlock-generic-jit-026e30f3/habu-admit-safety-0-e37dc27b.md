---
title: Admit safety>0 lambdas after bridge trampoline
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-22T21:08:17.865704+01:00\\\"\""
closed-at: "2026-02-22T22:48:25.101894+01:00"
close-reason: Removed safety gate in JIT candidacy, validated bridge throw relay under safety=3, and rebaselined maxima-hotspots (sk_safety=0, jit_compiled=397).
---

src/jit/candidates.zig: remove lambda.safety gate now that JIT bridge unwinds via trampoline. Keep structural constraints (captures/optional/key/rest/assert body). Add regression in src/tests/integration.zig proving safety>0 defun can be JIT-admitted and still respects throw/error semantics. Rebaseline jit_adm via tools/maxima-hotspots --json to confirm sk_safety drops and jit_compiled rises. Depends on habu-implement-jit-bridge-2601a3ad.
