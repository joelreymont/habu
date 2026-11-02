---
title: "itick2: read the trusted cell at the internal tick"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:41:06.979634+02:00"
closed-at: "2026-11-02T11:33:14.735962+02:00"
close-reason: "Landed, squashed into 16477b319f 'Refuse internal ticks in unchecked native bodies': owner-access.f:97-98 OA-TICK1 REJECTS; VISIBLE-RECORD? is private (dict.f:49); 413345aafc later moved the ticks to TICK-REFUSED?."
---

itick review fold-in: unexport NDICT:VISIBLE-RECORD? (callers read TRUSTED-CELL), tier-1 REJECTS in owner-access INTERNAL-TICK, three comments. Brief /tmp/jerry-big/t0-itick2-brief.md. Base mnzvrsym. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-itick2
