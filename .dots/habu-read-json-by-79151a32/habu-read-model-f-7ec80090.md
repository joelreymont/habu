---
title: "Read model.f's JSON by value"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.502687+01:00"
---

Problem: src/lib/net/model.f has 15 READ-INIT sites and a reader field in its record (:158). Acceptance: one commit per region of about 30 edits, the settings checks (:1766-1820) first, read by value; the last deletes the reader field. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-jmodel.
