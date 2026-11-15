---
title: Link an export whose word was undefined
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T05:35:56.797083+02:00"
closed-at: "2026-11-13T05:59:29.025617+02:00"
---

Problem: a file that undefines an exported word builds cold, but its warm build throws 7113 E-EXPORT-UNDEFINED (undefine PKG:X of the export itself) or 7156 E-BAD-STORED-SIGNATURE (undefine of the export's private source inside the package), and with the source undefined a --repl image's lookup of the export throws 7156 where loading from source prints its value. A warm link replays the retirement (src/habu/linker.f LINK-FACTS) before READ-ALIASES reads each alias from its source (src/core/checker.f READ-ALIAS, ALIAS-BODY), and an image reads an export through its source (READ-BOUND); a retired record has no symbol (REC-SYM). Acceptance: both files build warm equal to cold, and their images answer as a load from source does; a test/file-objects.f case guards both. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-ud.
