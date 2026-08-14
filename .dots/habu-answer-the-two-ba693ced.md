---
title: Answer the two wide-family checker limits
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T02:43:19.841931+02:00"
---

Found by the ctor-pads lane building fixtures, both fail-closed, both pre-existing: (1) a definition cannot name two instantiations of one family in its declared output - CONSTRUCT-DECL-TERM takes the first and the body is refused; (2) a construction followed by a MATCH over the same wide family in one body is refused by the ENGINE with 'undefined word'. Establish whether each is a design limit to document or a bug to fix; the second smells like a reader/scope issue worth a Checker-Miss RCA if it is one. Files: src/core/checker.f, src/core/type-family.f. Depends: none.
