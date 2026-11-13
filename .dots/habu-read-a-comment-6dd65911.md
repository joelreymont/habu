---
title: "Read a comment after a does> word's call as a comment in the pre-pass"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:13:48.296907+02:00"
---

Problem (lead's sweep of tools/check.f over test files, installed hb = master 4255709c engine, outputs /home/joel/.cache/tmp/carl-jsonchk/sweep/test_certify-does-definer.f.plain.*): plain bin/hb --load tools/check.f -- test/certify-does-definer.f fails 'source preverify failed before run', rc 70, E-UNDEFINED-TOP-LEVEL token 'Section' at 67:3, a word inside the comment line that follows the top-level call 'CDD-PKB' (65) of a does> word defined at 64; bin/hb --load of the file passes. The pre-pass reads the comment as code. Acceptance: the cause found (which reader treats the next line as the call's input, and why the load does not) and fixed in that layer; plain check of the file reaches its run. Files: unknown until the hunt (src/habu/verify-source.f). Verify: the command above; a REFUSED-MODES or admitted case in test/diag-position-test.f. Depends: none. Ownership: pre-pass top-level reading. Claim: agent=carl workspace=.jj-ws.noindex/carl-hcdd

Waiting (no consumer meets it). Fix commit zkmnmrls (f455cb19, on 8f6ec543, "Read a create-less does> word as a patch") is kept unlanded; its workspace carl-hcdd is retired. Review: REVISE, findings F1-F8 (F1 a regression against master); redo = two fold briefs into zkmnmrls, 2a = F1, F2, F6, F7 with their F8 comments, 2b = F3, F4, F5 with theirs (.jj-ws.noindex/carl-layout/.out/handoff-notes-43.md; probes ~/.cache/tmp/rev-dpw/cases/). Rebase onto master first. Claim: none.
