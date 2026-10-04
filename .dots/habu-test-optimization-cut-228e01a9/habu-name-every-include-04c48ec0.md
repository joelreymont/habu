---
title: Name every include I/O refusal that reaches the process exit
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:42:01.292202+03:00"
---

Found by lane 535 (dot 1f8f44f4): src/core/include.f throws INCLUDE-IO-RC (74) bare, and an uncaught throw of a code below 256 is a process exit status, so the process ends rc 74 with no message. Measured on 9084b558: a --load file calling `s" /nonexistent/zz" [: ;] SOURCE-ROOT:WITH` exits 74 with 0 bytes on stdout and stderr (ROOT-CANON, include.f:393 `TRY-CANON 0= if INCLUDE-IO-RC throw then`). The other bare INCLUDE-IO-RC throws (include.f:321, :418, :437, :473, :554, :633, :683, :698, :1239, :1325, :1357, :1368) are unmeasured. The class of 9e94c013 and 1f8f44f4. Acceptance: a census of every INCLUDE-IO-RC throw and whether source can reach it uncaught; each reachable one either throws a named negative code that renders (word, path) or writes one named line ending in LF before the exit; a case through the real load path for each distinct refusal, seen failing first (rc 74, empty stderr); the library's catchable contract for callers that catch 74 kept or migrated with every caller; baked: rebuild, g1 == g2, two-generation build. Base: after 1f8f44f4 lands (include.f CWD-DIE).
