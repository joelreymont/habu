---
title: Run x86 persistent breakpoints on every instruction form
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:05:33.306443+01:00"
---

Problem: an x86 unit may begin with call rel32, jmp, jcc or a rip-relative instruction since the short-forms lane, and src/habu/debug.f:134-143 TRAMP! takes none of them, so DEBUG:BP* and BPN become one-shot. Acceptance: the engine-stack-debugger suite runs on x86 with a calling word, a tail word and a string word, each hit twice. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-bp.
