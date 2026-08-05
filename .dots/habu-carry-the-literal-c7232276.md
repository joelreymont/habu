---
title: Carry the literal memo through OPEN-PLAIN
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T20:28:53.254263+02:00"
---

elaborate.f's block-local literal memo clears at every BEGIN-BLOCK including OPEN-PLAIN, whose predecessor dominates it and which already keeps its inherited value vector — the memo could legally survive that boundary, and clearing it leaves folds on the table. Extend the one-sentence dominance rule to name the OPEN-PLAIN case, measure the additional folds on the corpus, and keep the conservative clearing at the other two sites. Found 2026-08-05.
