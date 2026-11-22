---
title: Let a JSON reader carry its cursor
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T12:32:58.535066+01:00"
closed-at: "2026-11-22T13:29:58.894058+01:00"
---

Problem: src/lib/json/read.f:191-194 mint and erase JSON:reader on ^n through the casts >CELLS and >CURSOR; READ-DISPOSE (:565-569) zeroes the record through >CELLS; RECORD (:1561) and src/lib/json/pointer.f:148,183 carve a cursor out of a scratch byte span. Acceptance: MINT-READER and ERASE-READER take and answer ^cursor; no cast or byte view reaches a cursor. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-json.
