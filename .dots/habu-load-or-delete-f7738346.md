---
title: Load or delete the x86 text-sites table
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:42:26.911093+01:00"
---

Problem: src/habu/text-sites-x64.f is loaded by no file (rg -n 'text-sites-x64' src tools test finds no requires or included of it), so the x86 text-site table it holds is checked by nothing and cannot be told from dead code. Acceptance: either the x86-64 engine build loads it and a test reaches what it defines, or it is deleted with whatever names only it. Depends: none. Claim: unassigned.
