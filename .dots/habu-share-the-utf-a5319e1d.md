---
title: Share the UTF-16 surrogate split
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T12:55:24.666861+02:00"
---

Problem: src/lib/json/write.f:69-73 and JW-PAIR (write.f:427-430), json/read.f's SUR-HI/SUR-LO/SUR-BASE/SUR-SHIFT and src/lib/utf16.f's PAIR-FIRST/HIGH-FIRST/LOW-FIRST/PAIR-SHIFT/PAIR-MASK repeat one surrogate split privately (JSON header mode review). Acceptance: one public word in utf16.f splits a scalar into its pair, and json/write.f, json/read.f and utf16.f's own encoder use it. Depends: the JSON header mode lane (habu-write-json-that-cb7a9301) landing. Claim: unassigned.
