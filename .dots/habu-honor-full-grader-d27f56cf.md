---
title: Honor full grader artifact paths
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-24T07:02:15.367893+02:00\""
---

Problem: `maki/device-artifacts.f` guarantees `MAKI-GRADE:CUBIN$` paths through
`FS-PATH-CAP`, but `maki/eval/device.f` narrows the child path three times.
`ED-LCUBIN` and NUL-terminated `ED-PATH` are each 64 bytes, and
`GRADE-WRITE-LAUNCHER` interpolates the path into an `SB-CAP=FS-PATH-CAP`
source buffer whose quoting is unsafe and whose syntax consumes capacity. A
legal long private `TMPDIR` therefore makes `EVAL:LAUNCH-CUBIN!` throw
`E-FS-PATH`; the outcome decoder then reports a device fault for a correct
kernel. On candidate `9e1d46f3`, the production correct SAXPY grade returns
`EVN-DEVICE-FAULT` with `TMPDIR=/tmp/habu-device-health.x9lTOv` and
`EVN-GREEN` with `TMPDIR=/tmp/h.7wLsee`; `maki/eval/emit-device-test.f`
passes the real `sm_121a` launches.

Exact design: package `EVAL` remains the sole owner. Allocate raw
`ED-LCUBIN` with `FS-PATH-CAP` bytes and NUL-terminated `ED-PATH` with
`FS-PATHZ-CAP` bytes. `EVAL:LAUNCH-CUBIN!` accepts lengths from zero through
`FS-PATH-CAP`; it rejects negative or larger lengths with `E-FS-PATH` before
copying bytes or changing `ED-LCUBIN-U`. Stop putting provider bytes into
generated source. `GRADE-WRITE-LAUNCHER` emits fixed source that obtains the
path with `0 SCRIPT-ARGV$`, calls the existing launcher setter, and exits
through `EVAL:LAUNCH-EXIT`. `GRADE-LAUNCH-ARGV` appends `--` and the exact
`MAKI-GRADE:CUBIN$` span after the launcher file, so spaces, quotes, and
backslashes remain argument bytes rather than source syntax. Do not add a new
public test or launch hook.

Dependencies: the existing `lib/fs.f` path constants, script-argument
interface, process-argument builder, and `MAKI-GRADE:CUBIN$` provider. This
leaf lands directly on `9e1d46f3`; active dot
`habu-type-device-grader-974c9e44` must rebase after it. No verdict-domain
dependency belongs here. Owned result: device-grader path transport, canonical
storage capacity, and its focused regression. Exact write set:
`maki/eval/device.f`, `maki/eval/device-fault-test.f`, and `LESSONS.md`.

Checkpoint: under a private `TMPDIR` whose resulting cubin path exceeds 63
bytes, the real correct SAXPY grade currently returns
`EVN-DEVICE-FAULT`; the same production path under a short root returns
`EVN-GREEN`. On a representative source diff, the package gate must still
accept every changed definition before implementation continues.

Acceptance: reopen package `EVAL` in the existing focused test, seed
`LAUNCH-CUBIN!`, and compare private `ED-LCUBIN$` directly. A provider-valid
boundary span round-trips byte-for-byte. Negative and over-capacity lengths
throw `E-FS-PATH` without changing the prior pointer contents, recorded
length, launcher source, or process arguments. The generated launcher contains
no cubin path bytes and the real spawned child receives the exact path through
script argument zero. The production device-fault test under a long private
`TMPDIR` grades the correct candidate green, the deliberate fault as a fault,
and reports success; assertions do not depend on the numeric values of legacy
`EVN-*` constants. The short-root behavior remains unchanged.

Forbidden: shortening or unsetting `TMPDIR`, special-casing a gate, truncating
paths, retaining any magic 64/63 path limit, quoting provider bytes into
source, changing unknown-exit classification, adding a fallback or public
hook, or broadening into the active verdict-domain migration.

Verify: the focused real-device test with a private long `TMPDIR`, focused
boundary and rejected-length mutations through `EVAL:LAUNCH-CUBIN!`, the exact
owning load, typed-local and package diff lints, then root's combined Maki
gate. Claim: agent=grader_path_impl
workspace=.jj-ws/habu-honor-full-grader-d27f56cf.
