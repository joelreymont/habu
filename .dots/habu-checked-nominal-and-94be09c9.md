---
title: Checked nominal and linear handle minting
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:44:49.846693+02:00"
---

Capability dot from the checked-PTY recovery (slices landed under
`habu-recover-checked-pty-04fcb611`): the checker cannot mint a use-once linear
handle or a distinct role nominal (supervisor pid vs group pid vs target pid;
three watch-descriptor roles) from a raw cell in an empty checked body, so
`lib/process-pty-handle.f` carries 18 paired TRUSTED mint/erase coercions.
Until a coherent source transfer lands, each boundary retains its checker-gap
rationale and `habu-recover-checked-pty-04fcb611` as retirement owner. Its
owning check is `lib/process-pty-handle-test.f`. Design and implement the
checker capability that expresses refined-nominal and linear-handle minting,
then transfer and retire the 18 casts coherently. Related precedent boundaries
that would shrink with the same capability are the current 34 role casts in
`src/core/roles.f` cited by task, FFI ABI, and memory code.
