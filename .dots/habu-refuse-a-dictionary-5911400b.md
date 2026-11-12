---
title: Refuse a dictionary lookup in a stripped program
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T18:38:18.823430+02:00"
---

Problem: a stripped program has no dictionary, but src/habu/link-band.f UNSAFE-NAME? (:157-168) does not name xref-search-wl or xref-wl-newest, so the link accepts a program that calls CHECKER-RESOLVE:WL-PROBE (src/core/checker.f:3167) and it dies at run time with E-MEMORY-FAULT at 0x29, rc 110 (probe: a stripped program whose MAIN looks a name up in BOOT:NAMESPACE-WL). Acceptance: the link refuses such a program as it refuses the other names UNSAFE-NAME? holds, naming the word; every primitive that reads the dictionary's records or wordlists is in the list or shown unreachable from a stripped program; a rejected fixture program pins the refusal. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/dict-refuse.
