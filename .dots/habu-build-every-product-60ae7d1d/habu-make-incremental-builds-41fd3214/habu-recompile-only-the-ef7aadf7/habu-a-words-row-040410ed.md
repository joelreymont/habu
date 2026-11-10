---
title: A WORDS row can bind the wrong record of two same-name records in one part
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T19:36:41.247634+02:00"
---

Problem (inferred by the next-lane design's read of src/habu/linker.f at carl-d6 2d5cdbe6, not reproduced): a record's identity counts same-name records per file across its parts (IDENTIFY :887) and WORD-TIME (:3858) gives every changed word of a piece the same time, so after an earlier part of the same unedited file compiles a different set of records, two same-name records in one part can be confused and a WORDS row binds the wrong one with a matching time. Reachable today through COMPILED-TIME and OLD-WORD; linking parts after a compiled part (habu-link-a-file-558cec4c) keeps it reachable. Acceptance: a reproducer program (two definitions of one name in a later part, an earlier part that gains or loses a same-name record) either shows a wrong binding, and then identity counts per part and the case joins test/file-objects.f, or shows why it cannot happen, recorded beside IDENTIFY. Files: src/habu/linker.f, test/file-objects.f. Verify: the reproducer, file-objects, warm equals cold. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-words (hunt worker running, brief carl-layout/.out/briefs/hunt-same-name-records.md; scratch ~/.cache/tmp/carl-words/same-name).
