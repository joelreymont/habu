---
title: Run and decode the x64 T0 emitter in a test
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T05:07:29.212067+02:00"
---

x86 phase, after arm64. No test runs or disassembles T0's x64 emitter (src/habu/t0-x64.f): the C lane review (lvwnlymn, writeback) found mutant MH (DSP+ imm8 add emitted as sub) passes every test; its decode of push/pop/DSP+ imm8/imm32 encodings (/tmp/jerry-t0c/rev/seam/2.o) is the only evidence. Add a rows file for x64 like t0-arm64-rows.f, run on joel@pulsar-ua-106.
