---
title: Reject a caught failed require once
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:53:39.808736+01:00"
---

Problem: a build whose word catches `required` of a file whose load fails (`: Q ( -- n ) [: s" /abs/bad.f" required ;] catch ;  : MAIN ( -- ) 1 . cr ;  Q drop`, bad.f = `: BAD-W ( -- n ) 1 ;` then `79 throw`) prints E-SOURCE-ROOT, then 'linker: ... the cell at space +3880664 in ROWS +0 holds <address>, outside hb's data and the build's space' and throws -9695 LINKER:E-ADDRESS, rc 67, on the installed engine too. Cause unknown. Acceptance: the cause named and fixed: the build gives one rejection at the require's line, the same warm and cold. Depends: none. Claim: unassigned.
