---
title: Account FFI clause runs by the declaration row
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:27.601997+02:00"
---

PENDING lane, public word FFI-DECL:PENDING approved by Joel 2026-10-26. Files using STRUCT-ARG/WRITES-ARG (src/lib/f64-text.f, fs-anchored.f, net/tcp4.f, udp4.f, crypto/evp.f) were refused run-unaccounted on every build. Change ulxuqysn in .jj-ws.noindex/carl-rebase: OPEN becomes a clause count, RUN-JUDGE admits clause runs by the declaration row, DECL-CLOSED-CK; PENDING-XT cleared with the other producer cells (it leaked an ASLR address into app images). ffs.f fixture isolated from library-table growth by ffsl.f, which proves the ffi-grown refusal. Acceptance: frame/auto/types/state e2e rc 0, ffi-test ok, second engine build reuses the named FFI files; major review (public word, judge rule). Workspace: .jj-ws.noindex/carl-rebase. Claim: carl.

Landed on master 9f905e31 (PENDING lane + tqvxtrmm boundary tests; frame/auto/types/state e2e pass in the full suite). Open acceptance item: a second engine build reuses the named FFI files; prove it with the warm engine builds of gate row 2ec826c7, then close.
