---
title: Load the x86 disassembler only for DEBUG
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.485119+01:00"
---

Problem: src/habu/debug.f:12 requires src/arch/x86-64/disasm.f on x86 alone for X64DIS:SPAN (an instruction's length and pc use at a breakpoint's entry), so x86's hb and every `--repl` program carry its 21,238 code bytes, printing included, that arm64's do not; arm64's debugger uses asm.f, which both engines hold anyway. Acceptance: the debugger loads only the decoder; the text printing loads only with tools/code-dump-x64.f; x86 engine code is at or below arm64's (tools/engine-size.f); code-dump-x64 and engine-stack-debugger pass on pulsar. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x86-dis.
