---
title: Drop the exact-span bit
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T10:49:10.170957+01:00"
---

Problem: every recorded code span now carries bit 31 (src/habu/code-span.f CODE-SPAN:FULL), so the bit tells nothing: package rows are told apart by their wordlist and an empty span by a zero count, and every reader masks it off. Acceptance: spans are plain byte counts on both arches, the saved dictionary format changes in a staged pair, and code-span.f's MASK, FULL and EXACT go. Depends: habu-record-every-code-59f18c64. Claim: unassigned.
