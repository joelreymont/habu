---
title: BF-CERTIFY-STAGE in build-fixpoint
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-01T23:07:20.891291+02:00\""
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

## Blocking flip landed (2026-07-06, on 50ed2c53)

Certification went clean and BLOCKING: the render.f typed cleanup removed the
last VERIFY:SOURCE-BUF misses (stage2/stdin/snap sources all rc 0), and
BF-CERTIFY-GENERATED now throws E-BUILD-STATUS on any reject (see
habu-make-fixpoint-certify-a11dbad5, closed, for the full evidence chain).
REMAINING here, now unblocked: retire the BF-PREFLIGHT textual asserts in
favor of the real (now blocking) certification; hash-pin the boot prefix
(sha256 baked into the image) to close the boot-reload TOCTOU; retire the
stdin.f REPL set-check now that repl sources certify.

## Preflight retirement + boot pin ported (2026-07-06, on 2017301c)

Both residue items were found already implemented on the unmerged fable lane
(2026-07-04) and were PORTED with provenance rather than redone: preflight
typed-shape asserts retired (fable c33ec3e66479 -> 10fecb46, extended: icode's
typed-shape asserts also retired since the blocking certify flip covers the
check-off window; kept: mmap fail-closed + no-static-allot invariants and the
two same-type codegen-role asserts, dot habu-preflight-codegen-role-a52ea587);
boot-prefix hash pin build-time half (fable eb9ee4631166 -> ported commit)
with E-BUILD-BOOT-DRIFT + `boot pin mismatch` regression; boot-TIME bake =
engine work, dot habu-boot-pin-bake-8b284046. REMAINING here: stdin.f REPL
set-check retirement (src/habu/stdin.f declared this worker's for that dot
only).
