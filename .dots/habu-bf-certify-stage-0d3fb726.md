---
title: BF-CERTIFY-STAGE in build-fixpoint
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T23:07:20.891291+02:00\\\"\""
---

Concrete implementation of habu-staged-fixpoint: insert BF-CERTIFY-STAGE between BF-STAGE2-SOURCE and BF-BOOTSTRAP-STAGE in tools/build-fixpoint.f - stage N binary runs the existing check-only path (tools/check.f --source-list / CHK-CHECK-HOOK / VERIFY:SOURCE-BUF, already used batched by the gate) over stage2-src, stdin-src, and the generated REPL LSRC; any reject fails the build. Run NON-BLOCKING first (report the miss list - checker.f/util/structures/render won't certify until habu-checker-self-typing), flip to blocking after. Also: retire BF-PREFLIGHT textual asserts (:407-468) in favor of real certification; hash-pin the boot prefix (sha256 baked into image) to close the boot-reload TOCTOU; retire stdin.f:62 REPL set-check once repl sources certify. Effort M (~4d). Conflicts: tools/build-fixpoint.f owned by cache worker - start after merge.

Progress 2026-07-03: nonblocking BF-CERTIFY now runs in tools/build-fixpoint.f
for generated stage2, stdin, and snap sources. It uses VERIFY:SOURCE-BUF in a
candidate checker scope with buffered diagnostics, reports rejects on stdout,
and does not fail the build until checker self-typing closes the known source
misses. Current reduced first miss after typed cleanup is src/core/checker.f
PSTACK at RECURSE. Blocking flip, preflight retirement, boot-prefix hash pin,
and stdin set-check retirement remain.
The only new checker.f trust sites are thin mmap result refinements
(`*-RC>PTR`); mmap invocation and failure checks remain checked.
