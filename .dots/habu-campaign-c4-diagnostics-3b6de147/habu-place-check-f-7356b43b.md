---
title: "Place check.f's record at its input line's own column"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T16:50:58.188685+01:00"
closed-at: "2026-11-17T23:15:57.731361+01:00"
---

Problem: a record placed at a token of an outer file, made while a file it includes runs, is placed in the wrong buffer. /tmp/jerry-inputcol/tok.f keeps a token of its own line 2 and includes mark.f, which places a record there: under hb --load it prints <input>:1:12 on the root line 's\" tok.f" script-required'; under tools/check.f, <input>:1:66 on check.f's own child line; hb tok.f prints mark.f:1:1; the token's place is tok.f:2:12. Cause: src/core/rejection.f INVOKER and INVOKED? (:569-576) take the frame below the subject as the root stream, but a file load pushes an evaluate frame, so IN-EVALUATED? (:586) finds the token in an outer file's saved bytes and INVOKE-PLACE (:610) places it at the token before that frame's read position, counted in that frame's buffer. Only the top file's bytes are published (include.f PUBLISH-FRAME, SRCLOC:OWNED-CELL). Acceptance: a token in any open file's bytes is placed at that file's path, line and column: tok.f prints tok.f:2:12 for both records under the load, tools/check.f and hb tok.f; a test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-place.
