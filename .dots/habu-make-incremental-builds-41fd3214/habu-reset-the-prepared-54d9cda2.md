---
title: Reset the prepared wordlist index per preparation
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T23:34:53.795533+02:00"
closed-at: "2026-10-28T17:13:33.481849+02:00"
close-reason: "landed: PREPARE-ROWS resets PREP-WIDS-INDEXED (tools/native-file-reference.f)"
---

Found by wlfix worker 1 (~/.cache/tmp/carl-file/wlfix-handoff.md): tools/native-file-reference.f PREPARE never resets PREP-WIDS-INDEXED and PREPARE-FILE sets it 1, so a definition prepared later never indexes its wordlist owners (MAYBE-INDEX-PREPARED-WIDS). Needs a reproducer before any change.
