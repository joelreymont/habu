---
title: A warm link binds an earlier same-name word after a count shift
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T21:18:00.898005+02:00"
closed-at: "2026-11-10T23:37:09.221902+02:00"
close-reason: "landed in 'Count a record within its own part'; suite 273 of 273 and the measure green on the landing tip (Maki, Zen, Tender warm equal to cold)"
---

Problem: src/habu/linker.f IDENTIFY counts a record among its file's earlier records of its name sum, across the file's parts. When an earlier part gains a same-name record without the file's source changing (a private does> definer under [if] on another file's constant), a later part links unchanged, its records take new counts, and a user's object still holds the old (file, sum, count): WORDS-CK finds the word now at that count with the same time and the link binds it. Reproducer ~/.cache/tmp/carl-words/same-name/v7 with rerun.sh: warm-make prints 5, cold-make 105, warm differs from cold. Acceptance: a record's count is among its own part's earlier records of the sum, PLACES keys a record by file key, part, sum and count, and the v7 shape in test/file-objects.f SAME-CASE prints what a cold build prints after each build. Files: src/habu/linker.f, test/file-objects.f. Verify: file-objects passes on an engine built from the tree; rerun.sh v7 prints 105 four times and warm equals cold unsigned. Depends: none. Ownership: src/habu/linker.f IDENTIFY, PLACE, FACT-REC; test/file-objects.f WRITE-G, WRITE-SAME, SAME-RUN. Claim: agent=carl workspace=.jj-ws.noindex/carl-sn
