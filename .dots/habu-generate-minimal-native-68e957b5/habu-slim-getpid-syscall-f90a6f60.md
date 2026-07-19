---
title: Slim getpid syscall
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T22:28:08.599779+02:00"
---

Current master exact native-code waste in src/habu/habu1.f BGETPID: the comment correctly says getpid cannot fail, but the emitter calls generic SYS-PUSH. SYS-PUSH emits carry/error normalization before G-PUSH, including cset, cbz, movn -1, and branch instructions whose result is impossible for getpid, adding 16 dead bytes on both targets. Linux SYS, also emits the generic -4095 status comparison even though getpid ignores error flags. Emit the syscall followed directly by G-PUSH, or introduce a shared no-fail syscall emitter only if a census proves its source and baked-prefix cost wins overall. Add exact macOS and Linux disassembly before/after, prove syscall number/result and stack effect are unchanged, and lower exact CODELEN attribution on both targets. Coordinate the semantic pid result with habu-type-getpid-result-f18dd1c2; this dot owns only impossible error-path bytes. The Linux exact row was independently corrected by 9402654f after getpid landed, so no attribution repair remains.
