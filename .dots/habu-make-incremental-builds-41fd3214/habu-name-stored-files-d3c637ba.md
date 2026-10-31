---
title: Name stored files by what compiles them
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T21:10:09.136981+02:00"
---

Problem: an engine build names the object store and every object key by the compiling binary's SHA-256 (tools/build-out.f ROOT$, tools/native-definition-cache.f FILE-KEY via ENGINE-ID:KEY$); the binary is the support image, whose inputs are the whole engine closure, so any tree edit recompiles every file (dot habu-measure-a-lib-1209a534). Joel 2026-10-31 (LESSONS.md): track file dependencies as make does, whole files, comment or no comment; no hooks. Acceptance: keys and store place carry the kernel role, the compiler and checker roles and a whole-byte build-tool digest instead of the binary hash; unknown kernel or tool digest compiles live and stores nothing; no public word added or changed; the gate row (habu-gate-the-recompile-2ec826c7) passes with memory.f edits recompiling everything the tree's compiler compiles and a file outside every role recompiling alone (interface edits: plus the files naming the word); warm == fresh cold. Files: tools/native-definition-cache.f, tools/build-out.f, tools/build-producer.f, src/core/source-producer.f, test/compiler/native-build-edit-e2e.f, docs/gate.md. Verify: brief carl-dots/.out/carl-file/brief-ident-make.md Proof. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-gate2.
