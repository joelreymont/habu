---
title: Delete the uncalled fs-anchored TRY words
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T11:28:36.734465+01:00"
---

Problem: src/lib/fs-anchored.f's public TRY-OPEN-DIR-AT, TRY-OPEN-READ-AT, TRY-OPEN-WRITE-AT, TRY-OPEN-ENTRY-AT, TRY-LINK-AT and TRY-REMOVE-DIR-AT have no caller outside the file and its test on spark or neubau (rg -i --no-ignore ~/Work). Acceptance: they, the private words only they use and their tests are gone. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-fsdead.
