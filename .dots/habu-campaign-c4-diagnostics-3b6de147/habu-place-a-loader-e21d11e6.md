---
title: "Place a loader's record at its interpreted string"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:38.797895+01:00"
closed-at: "2026-11-16T14:39:36.985505+01:00"
---

Problem: under bin/hb an interpreted s" copies its bytes out of the source, so a loader's path record (E-PATH-RANGE) stands at the loader word, not at the string the source wrote. Acceptance: the record stands at the string literal in all three readers. Depends: habu-reject-a-path-672d66ba. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w224.
