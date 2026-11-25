---
title: "Hold the JSON reader's strings in vectors in its last byte rows"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-25T11:49:34.517618+01:00"
closed-at: "2026-11-25T12:34:40.296074+01:00"
---

Problem: src/lib/json/read.f STR-INNER ( ^cursor ^u8 n -- n ) (:1192; callers json/value.f:439, copy.f:23 and :29, equal.f:454, :597 and :640) decodes into a caller's buffer and size. Acceptance: it appends to a VEC:vector<u8> its caller owns (as READ-STR-APPEND does) and each caller passes one; json-read and the json suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-strinner.
