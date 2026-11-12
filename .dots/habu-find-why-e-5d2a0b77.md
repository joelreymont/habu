---
title: Find why E-CTOR-PROTECTED acts in a build only
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:46:16.748714+02:00"
---

Problem: docs/design.md:245 says the checker refuses reopening or defining into a family's constructor namespace (E-CTOR-PROTECTED). On the installed hb, `package OPTION public : ZZ ( -- n ) 1 ; ;package` and the same for RESULT, IR--TYPE-KIND and HIR-OPCODE answer rc 0; inside a --repl build, `package <ctor namespace> seal-package ;package` threw 7111. Acceptance: the cause of the difference as path:line with evidence, and whether a program's own family's constructor namespace is refused on hb; the fix is a second brief. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/hunt-ctor.
