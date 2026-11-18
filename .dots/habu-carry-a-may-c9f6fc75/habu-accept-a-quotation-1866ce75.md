---
title: "Accept a quotation whose declared type is the slot's"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T10:00:54.028839+01:00"
closed-at: "2026-11-18T11:24:41.955469+01:00"
---

Problem: /tmp/jerry-nested/m2.f passes MK ( -- [ [ -- ] -- ] ) to ST3 ( [ [ -- ] -- ] -- ), the same declared type, and C5 is rejected at ST3 with expected [ [ -- renders ] -- ], found [ [ -- renders ] -- renders ]: ST3's SV ! (SV [ [ -- renders ] -- renders ]) widened its parameter's input facts in place (src/core/checker.f QUOT-PAIR :4864-4865), not its output facts, and that widened type is what ST3 saves. Acceptance: a quotation is accepted where the slot's declared type equals its own, or ST3 is rejected at its own SV !; a test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nC.
