---
title: Stop installing after a child moves the sink
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:40:57.483739+02:00"
---

OWN Revision 5 (brief-fix-own.md): a full-hit file whose child moved the origin sink (ENGINE-ORIGIN:ARM) kept and reinstalled an object a cold build refuses (warm != cold, since mlluspps). Fold-in oztxspls ef51dca3 on qzrwknxt in .jj-ws.noindex/carl-fown; Opus review: REVISE (a sink move during the parent's own top level is still missed, probe C). Revision 6: utyqvons 79eacea5 "Check the sink before each install" on oztxspls (probes A-E warm == cold, history red/green, P0 warm == cold 147/8); Opus re-review running. Lands with lane 11. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fown.
