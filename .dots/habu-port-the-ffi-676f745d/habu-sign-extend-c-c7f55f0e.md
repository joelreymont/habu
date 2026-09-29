---
title: Sign-extend C int results in FFI declarations
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.765664+03:00"
---

Problem: baseline defect on both arches. `lib/ffi-abi.f:734-739 OUT-TOKEN` admits `ptr u8`, `r` or a bare cell; a C `int` arrives with unspecified upper bits, so `lib/fs-mutate.f:332 OPEN-CALL open ( ... -- n )` reads `0xFFFFFFFF` for `-1`, writes to fd -1 and `FS-MUT-ATOMIC-CLEAN-TEMP` unlinks the colliding file (strace-verified). Neither AAPCS64 nor SysV defines bits 32-63 of a 32-bit result. Land before any x86 FFI work; unblocks the `fs-mutate` red on spark.
Acceptance: a result token `i32` renders `n` in the checker effect and a sign extension in the generated body (`dup $80000000 and if $FFFFFFFF00000000 or then`); `u32` renders a mask; every C-`int` row re-declared. The rows (of the 136 `rg -n "FUNCTION: .* -- n \)" lib src tools --glob '*.f'` hits, those whose C prototype returns `int`): `lib/fs-mutate.f:223,332,333`; `lib/fs-identity.f:18` (fstat); `lib/os-memory.f:15` (getpagesize); `lib/task.f:306-349` (sem_*, nanosleep; the mach rows return kern_return_t); `lib/task-test.f:1006` (clock_gettime); `lib/signal.f:125` and `lib/signal-test.f:101` (sigaction); `lib/process.f:150` (waitpid); `lib/pg.f:158-182` (all PQ* rows except pointer returns); `lib/net/udp4.f:151-161` (socket/bind/close/getsockname; sendto/recvfrom are ssize_t); `lib/net/tcp4.f:202-225` (socket/bind/listen/connect/shutdown/close/accept4/getsockname; send/recv ssize_t); `lib/net/curl.f:110-174` (CURLcode/CURLMcode rows and `fclose`; `*_init`, `strndup`, `open_memstream` return pointers, `strlen`/`memcpy` size_t/pointer); `lib/crypto/evp.f:63-119` (int rows; `EVP_*_new`, `EVP_sha*`, `HMAC` return pointers); `lib/crypto/evp-test.f:368` (getrusage); `lib/aio-macos.f:46-57` (fcntl/poll/accept/connect/getsockopt are int; read/pread/write/pwrite/lseek are ssize_t/off_t); `lib/fs-copy-alias-test.f:7` (link); the `lib/ffi-test.f` getpid/strncmp/snprintf rows; `tools/hb-build-test.f:131` (getpid); `lib/engine-id.f:45` and `src/habu/proc-maps.f:212-213` (macOS int rows); a new `lib/ffi-test.f` case calls a C function returning a negative `int` and asserts the negative cell.
Files: `lib/ffi-abi.f` and each declaration file listed.
Verify: spark `bin/hb --load lib/ffi-test.f` plus the `fs-mutate`, `tcp4`, `udp4` and `tasking-threads` rows; macOS arms correct by construction, reported untested.
Depends: none.
Route: Alder (shared: lib/ffi-abi.f, the lib/ declaration files listed, tools/hb-build-test.f, src/habu/proc-maps.f).
Ownership: krait (Intel lane).
Claim: unassigned.
