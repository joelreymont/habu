---
title: Implement x86 native breakpoints
status: active
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.383912+03:00"
---

Problem: src/lib/debug.f writes ARM BRK0 ($D4200000) into x86 code and x86 has no SIGTRAP handler (boot-x64.f INSTALL-CRASH sends SIGTRAP to the crash dump). Fails x86 test/gate-debug.f from 'native breakpoint resumes' (all profiler cases now pass) and runtime-regression process/pty (2). Design first (Fable Plan): int3 patch + SIGTRAP handler twin of habu2.f EMIT-TRAPH; resolve persistent BP*/BPN re-arm (TF single-step vs out-of-line execution), 1-byte patch vs debug.f's 4-byte CODE-BYTES/patch32 assumption, target encoding selection for cross builds. Acceptance: gate-debug.f rc 0 on pulsar, ARM unchanged. Depends: none. Ownership: debug.f, kernel-x64.f/boot-x64.f. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.

Pulsar update: Pulsar active: worker in .jj-ws.noindex/pulsar-intel-breakpoints at intel 184663fe. Implement INT3 and SIGTRAP resume/rearm through existing debugger slots; persistent breakpoints use one-instruction TF stepping. Preserve ARM behavior and all master tests. ARM checks of shared debugger changes remain OPEN for the Mac.
