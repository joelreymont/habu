---
title: A refused build of an aio program ends in SIGSEGV on the process-state stack
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:24:23.325830+02:00"
---

Problem: on the process-state stack's tip (259191a9, unpushed) a program that requires src/lib/aio.f is refused by the linker as on master (AIO-LOAD:HOST, a compiled word that calls required), but then the build dies of SIGSEGV (rc 139, a habu-crash regs dump; one dump reads as a fetch from address 0x50) where master prints 'hb-build: ... threw -8570' and exits 74. A refusal whose loaded file is not aio.f (a word that requires src/lib/date.f) still exits 74 there. Reproducer: printf 'require src/lib/aio.f\n: MAIN ( -- ) AIO:RUNNING? drop ;\n' > p.f; bin/hb --load tools/hb-build.f -- p.f -o prog. Acceptance: the cause named as path:line and fixed in the stack before it lands; the reproducer exits 74 with the two lines master prints (or builds, once the loaders change). Files: unknown until the cause. Verify: the reproducer on the stack's engine. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-crash (hunt brief .jj-ws.noindex/jerry-wasm/.out/briefs/crash-hunt.md; the fix is a second brief).
