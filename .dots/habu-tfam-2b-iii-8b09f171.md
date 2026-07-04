---
title: "TFAM 2b-iii: sound FFI protected-pointer seal guard (cat 5)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:48:58.607376+02:00"
---

Syscall writers (read/ioctl/mmap/readlink/stat64/lstat64/getdirentries64/poll) are PROT-GUARD'd (2b-i; seal.f proves E-SEAL-VIOLATION 83). The FFI trampolines BFFI-CALL (src/habu/habu1.f:1390), BFFI-CALL-N (:1457), BFFI-CALL-ABI-CORE (:1425) are NOT guarded, so a friend-band pointer packed as an FFI arg (via lib/ffi-abi.f FFI-ARG! or a raw hand-packed argbuf) reaches the foreign fn which can write through it, tampering a sealed cell. A mechanical x0-x7 PROT-GUARD is UNSOUND: the FFI ABI reuses a shared, partially-initialized argbuf - lib/ffi-abi.f FFI-CALL0 sets zero args so ffi-call (BFFI-LOAD-X0-X7) loads 8 STALE cells; lib/ffi.f dlopen sets only argbuf[0..1]; ffi-call-abi also loads x8 sret + d0-d7 from possibly-stale slots - so guarding all loaded arg regs would band-check stale non-arg cells and can false-trap legit low-arity calls. Sound guard needs arg-count/pointer awareness. Options: (a) carry nargs into the trampoline (ffi-call-n already has x14=nargs; ffi-call/ffi-call-abi would need it) and PROT-GUARD only [0..nargs) integer arg slots + sret x8 when present; (b) guard at the FFI-ARG! checked boundary (lib/ffi-abi.f) and document raw ffi-call/ffi-call-n/ffi-call-abi as an explicit unchecked boundary. Red forge: friend-band pointer as arg[0] with fn=0 -> guard must trap 83 BEFORE the (16 BLR,). Positive: real FFI suite (lib/ffi-test.f) + zero-arg FFI-CALL0 stay green. seal-absence.f already pins ffi-call absent in gforth (no mirror). Cat 5 syscall-writer portion is complete (2b-i); this is the remaining FFI-writer portion.
