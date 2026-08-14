---
title: Bake the structure size as a literal
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T09:54:42.240792+02:00"
---

Option found by the does-conv lane, needs a ruling: END-STRUCTURE can publish the size word as : NAME ( -- n ) <size> ; with the size a LITERAL - it is a compile-time constant by then. No storage, no base, thread-invariant trivially, better codegen than does> @. Cost: the three field definers' threading effects narrow from ( ptr a n ) to ( n ); declaration sources unchanged; one STRUCTURE-MISUSE fixture pushes 0 0 for the current arity and would change. Decide and land with (or instead of) the origin-word conversion for BEGIN-STRUCTURE. Files: src/core/structures.f. Depends: sequencing with 67147cae's deletions (structures.f may be deleted wholesale later - check before working).
