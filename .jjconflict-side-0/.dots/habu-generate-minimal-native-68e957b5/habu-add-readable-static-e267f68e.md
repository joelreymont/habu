---
title: Add readable static C strings
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:40:58.383504+02:00"
---

Invariant: stable NUL-terminated human text used by foreign-function calls has a readable checked declaration, not hand-transcribed decimal bytes; true binary tables, digests, instruction data, and exact byte vectors remain explicit bytes. The task module exposes the defect directly, and a broad lexical census finds 82 candidate decimal-byte lines across 22 files, so a local rewrite would miss the global issue while a blanket rewrite would corrupt legitimate binary data.

Provide the smallest checked static C-string declaration surface with stable lifetime, explicit terminator, bounded length, and no hidden allocation. Define escaping and reject embedded NUL, malformed escape, overflow, and any declaration whose pointer lifetime cannot satisfy the foreign call. Classify every candidate: migrate only data proved to be readable human text, including library and symbol names; retain binary data with a clear semantic comment or typed owner. Coordinate the concrete task migration with habu-remove-task-ascii-5db716b9 and avoid duplicate conversion scratch buffers when static storage is correct.

Prove empty, one-byte, escaped, maximum-length, over-limit, embedded-NUL, pointer stability, alignment, exact bytes, bootstrap availability, recovery and native parity, snapshots, ahead-of-time compilation, fixpoint identity, and FFI calls on supported platforms. Run the classified modules, task and FFI suites, package, host, size, and full native gates. Measure source tokens, dictionary entries, DATA, JIT, CODELEN, image size, and load time before and after; readable declarations must not increase resident size without exact justification.
