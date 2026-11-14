---
title: "Point open dots' citations at the lines that hold them"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T08:47:37.705877+01:00"
---

Problem: open dots cite lines that no longer hold the word they name, and a dot that points at the wrong line sends its worker to the wrong code. A word-checked scan of the forms 'path WORD (:N)', 'WORD (path:N)' and a bare ':N' after a word still finds 105 misses: 17 whose word lies in another file than the path before it, 71 whose word has no single definition (deleted, renamed, or defined in several places) and 17 whose file is gone (src/habu/interpret.f, src/trim.f, tools/native-build-core.f among them). Acceptance: every open or active dot's line citation names a line that holds what the clause says, a clause whose code is gone is cut, and the scan finds no miss. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cite.
